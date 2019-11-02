(defpackage #:kablature.model
  (:use :cl)
  (:export :duration
           :chord
           :note
           :dottedp
           :keys
           :restp
           :beat-length
           :beamed
           :chords
           :tablature
           :title
           :timesig
           :constructs
           :bars
           :beats-per-bar
           :beat-root
           :bar-duration))

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
  ((title :type string :reader title :initarg :title)
   (keys :type integer :reader keys :initarg :keys)
   (timesig :type (cons integer integer) :reader timesig :initarg :timesig)
   (bars :reader bars :initarg :bars)))

(defun beats-per-bar (timesig)
  (car timesig))

(defun beat-root (timesig)
  "Return the note which defines a beat in the tablature.."
  (cdr timesig))

(defun bar-duration (timesig)
  "Return the portion of a whole note that a bar holds with the given time signature."
  (/ (beats-per-bar timesig) (beat-root timesig)))