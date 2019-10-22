(defpackage #:kablature.model
  (:use :cl)
  (:export :duration
           :chord
           :note
           :dottedp
           :keys
           :restp
           :beamed
           :chords
           :tablature
           :title
           :timesig
           :constructs
           :measures
           :beats-per-measure
           :beat-root))

(in-package #:kablature.model)

(defgeneric duration (obj))

(defclass chord ()
  ((note :type integer :accessor note :initarg :note)
   (dottedp :type boolean :accessor dottedp :initarg :dottedp)
   (keys :accessor keys :initarg :keys)))

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

(defmethod duration ((beamed beamed))
  (with-slots (chords) beamed
    (loop :for chord :in chords
          :sum (duration chord))))

(defun beat-length (construct beat-root)
  "Return the number of beats that compose a construct."
  (* beat-root (duration construct)))

(defclass tablature ()
  ((title :type string :accessor title :initarg :title)
   (keys :type integer :accessor keys :initarg :keys)
   (timesig :type (cons integer integer) :accessor timesig :initarg :timesig)
   (constructs :reader constructs :initarg :constructs)
   (measures :type integer :reader measures)))

(defun beats-per-measure (tab)
  (car (timesig tab)))

(defun beat-root (tab)
  "Return the note which defines a beat in the tablature.."
  (cdr (timesig tab)))

(defun count-measures (tab)
  (loop :with measures := 0
        :with beat-root := (beat-root tab)
        :with beats-per-measure := (beats-per-measure tab)

        :for construct :in (constructs tab)
        :for beats := (beat-length construct beat-root) :then (+ beats
                                                                 (beat-length construct beat-root))
        :do (cond
              ((= beats beats-per-measure) (incf measures) (setf beats 0))
              ((> beats beats-per-measure) (error "Found ~S beats in measure ~S; expected ~S."
                                                  beats measures beats-per-measure)))
        :finally (return measures)))

(defmethod initialize-instance :after ((tab tablature) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value tab 'measures) (count-measures tab))
  nil)

(defmethod duration ((tab tablature))
  (with-slots (duration) tablature
    (if (slot-boundp tablature 'duration)
        duration
        (setf duration (loop :for construct :in constructs
                             :sum (duration construct))))))
