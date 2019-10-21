(defpackage #:kablature.print
  (:use #:cl #:kablature.model)
  (:export :print-kab))

(in-package #:kablature.print)

(defparameter octave-notes "ABCDEFG" "Letters used in each octave")
(defparameter octave-root 2 "Letter at Key 1")

(defparameter tabnote-offset-y 5)
(defparameter tabnote-width 15)
(defparameter tabnote-color "white")
(defparameter tabnote-marked "salmon")

(defparameter measure-thickness 3)
(defparameter font-size 10)

(defparameter note-radius 4)
(defparameter beat-height (* 2 tabnote-width))

(defparameter thin-style "stroke-width:1;stroke:black")
(defparameter measure-style (format nil "stroke-width~A;stroke:black" measure-thickness))
(defparameter text-style (format nil "font-size:~A;fill:black" font-size))

;; FIXME: 4/4 timesignature is hardcoded in this procedure.
(defun count-measures (tab)
  "Count the measures within a tablature"
  (loop :with duration := 0
        :and measures := 0
        :for construct :in (constructs tab)
        :do (progn
              (incf duration (duration construct))
              (assert (<= duration 1))
              (when (= duration 1)
                (incf measures)
                (setf duration 0)))
        :finally (return measures)))

(defun print-kab (kab &optional (stream t))
  (format stream "Measures: ~A~&" (count-measures kab))
  kab)
