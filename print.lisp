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

(defparameter +note-thickness+ 1)
(defparameter +note-radius+ 4)
(defparameter +stem-outreach+ 20)
(defparameter +beat-height+ (* 2 +tabnote-width+))
(defparameter +taper-length+ 5)

(defparameter +tab-margin-x+ 50)
(defparameter +tab-margin-y+ 10)
(defparameter +measure-text-margin+ 5)

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

(defun key-left (key num-keys)
  "Return the x position of the left portion of the key."
  (+ (tab-left) (* +tabnote-width+ (key-position key num-keys))))

(defun key-center-x (key num-keys)
  (+ (key-left key num-keys) (/ +tabnote-width+ 2)))

(defun draw-tab-bar (kab scene num-measures)
  "Draw bars where notes will reside and text labels to signal the keys' notes."
  (loop :with num-keys := (keys kab)
        :with timesig := (timesig kab)

        :with tab-top := (tab-top)
        :with body-height := (tab-body-height timesig num-measures)
        :with body-bottom := (tab-body-bottom timesig num-measures)

        :for key :from 1 :upto num-keys
        :for x := (key-left key num-keys)
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

(defun measure-bottom (measure-num timesig tab-measures)
  "Return the y position of the bottom of the given measure."
  (- (tab-body-bottom timesig tab-measures)
     (* measure-num (measure-height timesig))))

(defgeneric draw-construct (scene construct y kab))

(defmethod draw-construct (scene (beamed beamed) y kab)
  (print "No support for beamed constructs yet."))

(defun stem-left ()
  (- (tab-left) +stem-outreach+))

(defun rightmost-key (chord tab-keys)
  (loop :for key :in (keys chord)
        :with result := nil
        :with result-pos := -1
        :do (let ((pos (key-position key tab-keys)))
              (when (> pos result-pos)
                (setf result-pos pos)
                (setf result key)))
        :finally (return result)))

(defun stem-right (chord tab-keys)
  (key-center-x (rightmost-key chord tab-keys)
                tab-keys))

(defun stem-y (note-y)
  "Return the y-position of the note's stem."
  (+ (- note-y +note-radius+)))

(defun draw-untapered-stem (scene chord tab-keys note-y)
  (let ((stem-left (stem-left))
        (stem-right (stem-right chord tab-keys))
        (stem-y (stem-y note-y)))

    (cl-svg:draw scene (:line :x1 stem-left :y1 stem-y
                              :x2 stem-right :y2 stem-y)
                 :stroke "black"
                 :stroke-width +note-thickness+)))

(defun draw-tapered-stem (scene chord tab-keys note-y)
  (let ((stem-left (stem-left))
        (stem-right (stem-right chord tab-keys))
        (stem-y (stem-y note-y)))
    (cl-svg:draw scene
        (:polyline :points
                   ;; points polyline attribute expect a "x,y x,y..."
                   ;; string.
                   (format nil "~$,~$ ~$,~$ ~$,~$"
                           stem-right stem-y
                           stem-left stem-y
                           (+ stem-left +taper-length+)
                           (- stem-y +taper-length+)))
                 :stroke "black"
                 :stroke-width +note-thickness+
                 :fill "none")))

(defmethod draw-construct (scene (chord chord) y kab)
  (when (restp chord)
    (print "No support for rests, yet.")
    (return-from draw-construct))

  ;; Appropriately draw the approrpiate chord stem in the appropriate
  ;; style if so appropriate.
  (ecase (note chord)
    (1 nil) ; whole notes don't deserve a stem.
    (2 (draw-untapered-stem scene chord (keys kab) y))
    (4 (draw-untapered-stem scene chord (keys kab) y))
    (8 (draw-tapered-stem   scene chord (keys kab) y)))

  ;; Cord noteheads
  (loop :with hollow-head := (member (note chord) '(1 2))
        :for key :in (keys chord)
        :do (cl-svg:draw scene (:circle :cx (key-center-x key (keys kab))
                                        :cy y
                                        :r +note-radius+)
                         :fill (if hollow-head "none" "black")
                         :stroke "black"
                         :stroke-width +note-thickness+)))

(defun draw-measure (scene kab measure-num tab-measures)
  "Draw the measure bar."
  (let* ((timesig (timesig kab))
         (measure-bottom (measure-bottom measure-num timesig tab-measures))
         (tab-left (tab-left))
         (tab-right (tab-right (keys kab))))

    ;; measure line
    (cl-svg:draw scene (:line :x1 tab-left :x2 tab-right
                              :y1 measure-bottom :y2 measure-bottom)
                 :stroke-width +measure-thickness+
                 :stroke +measure-color+)
    ;; measure label
    (cl-svg:text scene (:x (+ +measure-text-margin+ tab-right) :y measure-bottom
                        :style +text-style+ :dominant-baseline "middle")
      (write-to-string (1+ measure-num)))

    ;; measure constructs
    (loop :for construct :in (nth measure-num (measures kab))
          :with y := (- measure-bottom +beat-height+)
          :do (draw-construct scene construct y kab)
          :do (decf y (* +beat-height+
                         (beat-length construct (beat-root timesig)))))))

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
    (loop :for measure-num :upto (1- num-measures)
          :do (draw-measure scene kab measure-num num-measures))
    (cl-svg:stream-out stream scene)))
