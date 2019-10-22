(defpackage #:kablature.print
  (:use #:cl #:kablature.model)
  (:export :print-kab))

(in-package #:kablature.print)

(defparameter octave-notes "ABCDEFG" "Letters used in each octave")
(defparameter octave-root (position #\C octave-notes :test 'char-equal) "Letter at Key 1")

(defparameter tabnote-offset-y 5)
(defparameter tabnote-width 15)
(defparameter tabnote-color "white")
(defparameter tabnote-marked "salmon")

(defparameter measure-thickness 3)
(defparameter font-size 10)

(defparameter note-radius 4)
(defparameter beat-height (* 2 tabnote-width))

(defparameter tab-margin-x 50)
(defparameter tab-margin-y 10)

(defparameter thin-style "stroke-width:1;stroke:black")
(defparameter measure-style (format nil "stroke-width~A;stroke:black" measure-thickness))
(defparameter text-style (format nil "font-size:~A;fill:black" font-size))

(defun key-position (key kab)
  "Return the position of the key on the tablature, starting left."
  (let ((root-index (ceiling (keys kab) 2)))
    (if (oddp key)
        (+ root-index (floor key 2))
        (- root-index (/ key 2)))))

(defun tab-header-height (kab)
  (* tabnote-offset-y (ceiling (keys kab) 2)))

(defun full-tab-header-height (kab)
  (+ font-size (tab-header-height kab)))

(defun markedp (key)
  (zerop (mod (floor key 2) 3)))

(defun key-note (key)
  (char octave-notes (mod (+ key octave-root -1) (length octave-notes))))

(defun draw-tab (kab scene)
  (loop :for key :from 1 :upto (keys kab)
        :for x := (+ tab-margin-x (* tabnote-width (key-position key kab)))
        :for offset-height := (- (tab-header-height kab)
                                 (* tabnote-offset-y (floor key 2)))
        :for markedp := (markedp key)

        :with note-text-style := (concatenate 'string text-style ";text-anchor:middle")
        :do (progn
              (cl-svg:draw scene (:rect :x (- x (/ tabnote-width 2)) :y tab-margin-y
                                        :width tabnote-width :height offset-height)
                           :fill (if markedp
                                     tabnote-marked
                                     tabnote-color) :stroke "black")
              (cl-svg:text scene (:x x :y (+ tab-margin-y offset-height font-size) :style note-text-style)
                (string (key-note key))))))

(defun print-kab (kab &optional (stream *standard-output*))
  (let* ((tab-width (* tabnote-width (keys kab)))
         (width (+ tab-width (* 2 tab-margin-x)))
         (height (+ (full-tab-header-height kab) (* 2 tab-margin-y)))
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width width :height height)))
    (cl-svg:draw scene (:rect :x 0 :y 0 :width width :height height)
                 :fill "white")
    (draw-tab kab scene)
    (cl-svg:stream-out stream scene)))
