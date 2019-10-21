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
           :tab-timesig-upper
           :tab-timesig-lower))

(in-package #:kablature.model)

(defgeneric duration (obj))

(defclass chord ()
  ((note :type integer :accessor note :initarg :note)
   (dottedp :type boolean :accessor dottedp :initarg :dottedp)
   (keys :accessor keys :initarg :keys)))

(defmethod duration ((note chord))
  (with-slots (note dottedp) chord
    (if dottedp
        (* note 3/2)
        note)))

(defun restp (chord)
  "Returns whether CHORD is actually a rest."
  (null (keys chord)))

(defclass beamed ()
  ((chords :accessor chords :initarg :chords)))

(defmethod duration ((beamed beamed))
  (with-slots (chords) beamed
    (loop :for chord :in chords
          :sum (duration chord))))

(defclass tablature ()
  ((title :type string :accessor title :initarg :title)
   (keys :type integer :accessor keys :initarg :keys)
   (timesig :type (cons integer integer) :accessor timesig :initarg :timesig)
   (duration :type integer :initarg :duration)
   (constructs :accessor constructs :initarg :constructs)))

(defmethod duration ((tab tablature))
  (with-slots (duration) tablature
    (if (slot-boundp tablature 'duration)
        duration
        (setf duration (loop :for construct :in constructs
                             :sum (duration construct))))))

(defun tab-timesig-upper (tab)
  (car (timesig tab)))

(defun tab-timesig-lower (tab)
  (cdr (timesig tab)))
