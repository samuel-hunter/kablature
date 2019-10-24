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
(defparameter +text-style+ (format nil "+font-size+:~A;fill:black" +font-size+))
(defparameter +measure-color+ "black")

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

(defun key-pitch (key)
  "Return the pitch associated with the key."
  (char +octave-notes+ (mod (+ key +octave-root+ -1) (length +octave-notes+))))

(defun measure-height (timesig)
  "Return the height of a measure in a tablature bar."
  (* (1+ (beats-per-measure timesig)) ; 1+ for the measure bar.
     +beat-height+))

(defun tab-left ()
  "Return the left x position of the tab."
  +tab-margin-x+)

(defun tab-width (num-keys)
  "Return the width of the tab."
  (* +tabnote-width+ num-keys))

(defun tab-right (num-keys)
  "Return the right x position of the tab."
  (+ (tab-left) (tab-width num-keys)))

(defun tab-top ()
  "Return the top y position of the tab."
  +tab-margin-x+)

(defun tab-body-height (timesig num-measures)
  "Return the height of the tab body."
  (* (measure-height timesig) num-measures))

(defun tab-body-bottom (timesig num-measures)
  "Return the bottom y position of the tab body."
  (+ (tab-top) (tab-body-height timesig num-measures)))

(defun tab-bar-height (kab num-measures)
  "Return the height the tab bar  with the MEASURES will reach to."
  (+ (tab-header-height (keys kab)) (tab-body-height (timesig kab) num-measures)))

(defun tab-bottom (kab num-measures)
  "Return the bottom y postiion of the tab."
  (+ (tab-top) (tab-bar-height kab num-measures) +font-size+))

(defun draw-tab-bar (kab scene num-measures)
  "Draw bars where notes will reside and text labels to signal the keys' notes."
  (loop :with num-keys := (keys kab)
        :with timesig := (timesig kab)

        :with tab-top := (tab-top)
        :with tab-left := (tab-left)
        :with body-height := (tab-body-height timesig num-measures)
        :with body-bottom := (tab-body-bottom timesig num-measures)

        :for key :from 1 :upto num-keys
        :for x := (+ tab-left (* +tabnote-width+ (key-position key num-keys)))
        :for offset-height := (* +tabnote-offset-y+ (floor (- num-keys key) 2))
        :for markedp := (markedp key)

        ;; Draw the key space
        :do (cl-svg:draw scene
                (:rect :x x :y tab-top
                       :width +tabnote-width+
                       :height (+ body-height offset-height))
                :fill (if markedp
                          +tabnote-marked+
                          +tabnote-color+)
                :stroke "black")

        ;; Draw the pitch label
        :do  (cl-svg:text scene (:x (+ x (/ +tabnote-width+ 2))
                                 :y (+ body-bottom offset-height +font-size+)
                                 :style +note-text-style+)
               (string (key-pitch key)))))

(defun measure-bottom (num-measure timesig tab-measures)
  "Return the y position of the bottom of the given measure."
  (- (tab-body-bottom timesig tab-measures)
     (* (1- num-measure) (measure-height timesig))))

(defun draw-measure (scene measure num-measure tab-measures timesig num-keys)
  "Draw the measure bar."
  (declare (ignore measure))

  (let ((measure-bottom (measure-bottom num-measure timesig tab-measures))
        (tab-left (tab-left))
        (tab-right (tab-right num-keys)))

    ;; measure line
    (cl-svg:draw scene (:line :x1 tab-left :x2 tab-right
                              :y1 measure-bottom :y2 measure-bottom)
                 :stroke-width +measure-thickness+
                 :stroke +measure-color+)
    ;; measure label
    (cl-svg:text scene (:x (+ +measure-text-margin+ tab-right) :y measure-bottom
                        :style +text-style+ :dominant-baseline "middle")
      (write-to-string num-measure))))

(defun print-kab (kab &optional (stream *standard-output*))
  (let* ((num-measures (length (measures kab)))
         (num-keys (keys kab))
         (width (+ (tab-right num-keys) +tab-margin-x+))
         (height (+ (tab-bottom kab num-measures) +tab-margin-y+))
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width width :height height)))
    (cl-svg:draw scene (:rect :x 0 :y 0 :width width :height height)
                 :fill "white")
    (draw-tab-bar kab scene num-measures)
    (loop :for measure :in (measures kab)
          :for num-measure := 1 :then (1+ num-measure)
          :do (draw-measure scene measure num-measure num-measures (timesig kab) (keys kab)))
    (cl-svg:stream-out stream scene)))
