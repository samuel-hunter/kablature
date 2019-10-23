(defpackage #:kablature.print
  (:use #:cl #:kablature.model)
  (:export :print-kab))

(in-package #:kablature.print)

(defparameter +octave-notes+ "ABCDEFG" "Letters used in each octave")
(defparameter +octave-root+ (position #\C +octave-notes+ :test 'char-equal) "Letter at Key 1")

(defparameter +tabnote-offset-y+ 5)
(defparameter +tabnote-width+ 15)
(defparameter +tabnote-color+ "white")
(defparameter +tabnote-marked+ "salmon")

(defparameter +measure-thickness+ 3)
(defparameter +font-size+ 15)

(defparameter +note-radius+ 4)
(defparameter +beat-height+ (* 2 +tabnote-width+))

(defparameter +tab-margin-x+ 50)
(defparameter +tab-margin-y+ 10)
(defparameter +measure-text-margin+ 5)

(defparameter +thin-style+ "stroke-width:1;stroke:black")
(defparameter +measure-style+ (format nil "stroke-width~A;stroke:black" +measure-thickness+))
(defparameter +text-style+ (format nil "+font-size+:~A;fill:black" +font-size+))

(defparameter +note-text-style+ (concatenate 'string +text-style+ ";text-anchor:middle"))

(defun key-position (key num-keys)
  "Return the position of the key on the tablature, starting left."
  (let ((root-index (floor (1- num-keys) 2)))
    (if (oddp key)
        (+ root-index (floor key 2))
        (- root-index (/ key 2)))))

(defun max-tab-header-offset (num-keys)
  "Return the height the first key reaches out in the header."
  (* +tabnote-offset-y+ (floor num-keys 2)))

(defun tab-header-height (num-keys)
  (+ +font-size+ (max-tab-header-offset num-keys)))

(defun markedp (key)
  (zerop (mod (floor key 2) 3)))

(defun key-note (key)
  (char +octave-notes+ (mod (+ key +octave-root+ -1) (length +octave-notes+))))

(defun measure-height (timesig)
  "Return the height of a measure in a tablature bar."
  (* (1+ (beats-per-measure timesig)) ; 1+ for the measure bar.
     +beat-height+))

(defun tab-body-height (timesig num-measures)
  (* (measure-height timesig) num-measures))

(defun tab-bar-height (kab num-measures)
  "Return the height the tab bar  with the MEASURES will reach to."
  (+ (tab-header-height (keys kab)) (tab-body-height (timesig kab) num-measures)))

(defun tab-width (num-keys)
  (* +tabnote-width+ num-keys))

(defun draw-tab-bar (kab scene num-measures)
  "Draw bars where notes will reside and text labels to signal the keys' notes."
  (let* ((num-keys (keys kab))
         (left-x +tab-margin-x+)
         (top-y +tab-margin-y+)
         (body-height (tab-body-height (timesig kab) num-measures))
         (body-bottom-y (+ top-y body-height)))

    (loop :for key :from 1 :upto (keys kab)
          :for x := (+ left-x (* +tabnote-width+ (key-position key num-keys)))
          :for offset-height := (* +tabnote-offset-y+ (floor (- num-keys key) 2))
          :for markedp := (markedp key)

          :do (progn
                ;; Draw the key space
                (cl-svg:draw scene (:rect :x x :y +tab-margin-y+
                                          :width +tabnote-width+ :height (+ body-height offset-height))
                             :fill (if markedp
                                       +tabnote-marked+
                                       +tabnote-color+) :stroke "black")

                ;; Draw the pitch label
                (cl-svg:text scene (:x (+ x (/ +tabnote-width+ 2))
                                    :y (+ body-bottom-y offset-height +font-size+)
                                    :style +note-text-style+)
                  (string (key-note key)))))))

(defun draw-measure (scene measure num-measure tab-measures timesig num-keys)
  "Draw the measure bar."
  (let* ((measure-bottom (- (+ +tab-margin-y+ (tab-body-height timesig tab-measures))
                            (* (1- num-measure) (measure-height timesig))))
         (tab-left +tab-margin-x+)
         (tab-right (+ tab-left (tab-width num-keys))))


    ;; measure line
    (cl-svg:draw scene (:line :x1 tab-left :x2 tab-right
                              :y1 measure-bottom :y2 measure-bottom)
                 :style +measure-style+)
    ;; measure label
    (cl-svg:text scene (:x (+ +measure-text-margin+ tab-right) :y measure-bottom
                        :style +text-style+ :dominant-baseline "middle")
      (write-to-string num-measure))))

(defun print-kab (kab &optional (stream *standard-output*))
  (let* ((num-measures (length (measures kab)))
         (tab-width (* +tabnote-width+ (keys kab)))
         (width (+ tab-width (* 2 +tab-margin-x+)))
         (height (+ (tab-bar-height kab num-measures) (* 2 +tab-margin-y+)))
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width width :height height)))
    (cl-svg:draw scene (:rect :x 0 :y 0 :width width :height height)
                 :fill "white")
    (draw-tab-bar kab scene num-measures)
    (loop :for measure :in (measures kab)
          :for num-measure := 1 :then (1+ num-measure)
          :do (draw-measure scene measure num-measure num-measures (timesig kab) (keys kab)))
    (cl-svg:stream-out stream scene)))
