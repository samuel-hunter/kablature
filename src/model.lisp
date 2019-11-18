(defpackage #:kablature.model
  (:use :cl)
  (:export :duration
           :chord
           :make-chord
           :note
           :dottedp
           :keys
           :restp
           :beat-length
           :beamed
           :make-beamed
           :chords
           :tablature
           :make-tablature
           :title
           :timesig
           :bars-per-staff
           :repeats
           :repeat-starts
           :repeat-ends
           :accidentals
           :bars
           :beats-per-bar
           :beat-root
           :bar-duration))

(in-package #:kablature.model)

(defgeneric duration (obj))

(defparameter +valid-notes+ '(1 2 4 8 16))
(defparameter +valid-beamed-notes+ '(8 16))

(defclass chord ()
  ((note :type fixnum :accessor note :initarg :note)
   (dottedp :type boolean :accessor dottedp :initarg :dottedp)
   (keys :accessor keys :initarg :keys)))

(defun make-chord (note dottedp keys)
  (check-type note integer)
  (assert (member note +valid-notes+)
          (note)
          "Expected a note within ~S, but received ~S"
          +valid-notes+ note)
  (check-type dottedp boolean)
  (loop :for key :in keys :do (check-type key fixnum))
  (make-instance 'chord :note note :dottedp dottedp :keys keys))

(defmethod duration ((chord chord))
  (with-slots (note dottedp) chord
    (if dottedp
        (/ 3/2 note)
        (/ note))))

(defun restp (chord)
  "Returns whether CHORD is actually a rest."
  (null (keys chord)))

(defclass beamed ()
  ((chords :accessor chords :initarg :chords)))

(defun make-beamed (chords)
  (loop :for chord :in chords
        :do (assert (member (note chord) +valid-beamed-notes+)
                    (chord)
                    "Expected a chord with a note in ~S, but received ~S"
                    +valid-beamed-notes+ (note chord)))
  (make-instance 'beamed :chords chords))

(defmethod duration ((beamed beamed))
  (with-slots (chords) beamed
    (loop :for chord :in chords
          :sum (duration chord))))

(defun beat-length (construct beat-root)
  "Return the number of beats that compose a construct."
  (* beat-root (duration construct)))

(defun group-constructs (constructs timesig)
  "Group constructs together into bars."
  (loop :with bars := ()
        :with bar-constructs := ()
        :with beats := 0

        :for construct :in constructs
        :do (incf beats (beat-length construct (beat-root timesig)))
        :do (push construct bar-constructs)
        :do (restart-case (cond
                            ((= beats (beats-per-bar timesig))
                             (push (nreverse bar-constructs) bars)
                             (setf bar-constructs ())
                             (setf beats 0))
                            ((> beats (beats-per-bar timesig))
                             (error "Found ~S beats in bar ~S by construct ~S."
                                    beats (length bars) construct)))
              (skip-rest ()
                :report (lambda (stream)
                          (format stream "Skip grouping the rest of the constructs."))
                (return (nreverse bars))))

        :finally (restart-case
                     (if (zerop beats)
                         (return (nreverse bars))
                         (error "Found ~S beats in the final bar ~S by construct ~S."
                                beats (length bars) construct))
                   (skip-rest ()
                     :report (lambda (stream)
                               (format stream "Skip the last unfinished bar."))
                     (return (nreverse bars))))))

(defclass tablature ()
  ((title :type string :reader title :initarg :title)
   (keys :type fixnum :reader keys :initarg :keys)
   (timesig :type (cons integer integer) :reader timesig :initarg :timesig)
   (bars-per-staff :type (or integer null) :reader bars-per-staff :initarg :bars-per-staff)
   (repeats :type list :reader repeats :initarg :repeats
            :documentation "Pairs of numbers determining the beginning and end of repeats.")
   (accidentals :reader accidentals :initarg :accidentals)
   (bars :reader bars :initarg :bars)))

(defun make-tablature (title keys timesig constructs
                       accidentals repeats bars-per-staff)
  (check-type title string)
  (check-type keys fixnum)
  (check-type timesig (cons integer integer))
  ;; TODO check all chords that their keys are within the tablature's key max.
  (check-type bars-per-staff (or integer null))
  (check-type repeats list)
  (assert (evenp (length repeats))
          (repeats)
          "Expected an evenly lengthed list, but found ~S" repeats)
  (check-type accidentals string)
  (make-instance 'tablature
                 :title title
                 :keys keys
                 :timesig timesig
                 :bars (group-constructs constructs timesig)
                 :accidentals accidentals
                 :repeats repeats
                 :bars-per-staff bars-per-staff))

(defun repeat-starts (tablature)
  (loop :for (starts ends) :on (repeats tablature)
        :by #'cddr :collect starts))

(defun repeat-ends (tablature)
  (loop :for (starts ends) :on (repeats tablature)
        :by #'cddr :collect ends))

(defun beats-per-bar (timesig)
  (car timesig))

(defun beat-root (timesig)
  "Return the note which defines a beat in the tablature.."
  (cdr timesig))

(defun bar-duration (timesig)
  "Return the portion of a whole note that a bar holds with the given time signature."
  (/ (beats-per-bar timesig) (beat-root timesig)))
