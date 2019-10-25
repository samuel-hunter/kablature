(defpackage #:kablature.print
  (:use #:cl #:kablature.model)
  (:export :print-kab))

(in-package #:kablature.print)

(defparameter +octave-notes+ "ABCDEFG"
  "Letters used in each octave")
(defparameter +octave-root+ (position #\C +octave-notes+ :test 'char-equal)
  "Letter at Key 1")

(defparameter +space-offset-y+ 5)
(defparameter +space-width+ 15)
(defparameter +space-color-unmarked+ "white")
(defparameter +space-color-marked+ "salmon")

(defparameter +bar-line-thickness+ 3)
(defparameter +bar-label-margin+ 5)

(defparameter +note-thickness+ 1)
(defparameter +note-radius+ 4)
(defparameter +stem-outreach+ 20)
(defparameter +stem-taper-length+ 5)

(defparameter +beat-height+ (* 2 +space-width+))

(defparameter +staff-margin-x+ 50)
(defparameter +staff-margin-y+ 10)

(defparameter +font-size+ 15)
(defparameter +text-style+
  (format nil "+font-size+:~A;fill:black" +font-size+))
(defparameter +bar-color+ "black")

(defparameter +note-text-style+
  (concatenate 'string +text-style+ ";text-anchor:middle"))

(defun key-position (key num-keys)
  "Return the position of the key on the tablature, starting left."
  (let ((root-index (floor (1- num-keys) 2)))
    (if (oddp key)
        (+ root-index (floor key 2))
        (- root-index (/ key 2)))))

(defun max-staff-header-offset (num-keys)
  "Return the height the middle key reaches out in the staff header."
  (* +space-offset-y+ (floor num-keys 2)))

(defun staff-header-height (num-keys)
  (+ +font-size+ (max-staff-header-offset num-keys)))

(defun markedp (key)
  "Return whether the key's space should be marked."
  (zerop (mod (floor key 2) 3)))

(defun key-pitch (key)
  "Return the pitch letter associated with the key."
  (char +octave-notes+ (mod (+ key +octave-root+ -1) (length +octave-notes+))))

(defun bar-height (timesig)
  "Return the height of a bar in the tablature."
  (* (1+ (beats-per-bar timesig)) ; 1+ for the bar line.
     +beat-height+))

(defun staff-left ()
  "Return the left x position of the staff."
  +staff-margin-x+)

(defun staff-width (num-keys)
  "Return the width of a staff."
  (* +space-width+ num-keys))

(defun staff-right (num-keys)
  "Return the right x position of the staff."
  (+ (staff-left) (staff-width num-keys)))

(defun staff-top ()
  "Return the top y position of the staff."
  +staff-margin-x+)

(defun staff-body-height (timesig num-bars)
  "Return the height of the staff body."
  (* (bar-height timesig) num-bars))

(defun staff-body-bottom (timesig num-bars)
  "Return the bottom y position of the staff body."
  (+ (staff-top) (staff-body-height timesig num-bars)))

(defun staff-height (tab num-bars)
  "Return the height of the whole staff."
  (+ (staff-header-height (keys tab))
     (staff-body-height (timesig tab) num-bars)))

(defun staff-bottom (tab num-bars)
  "Return the bottom y postiion of the staff."
  (+ (staff-top) (staff-height tab num-bars) +font-size+))

(defun key-left (key num-keys)
  "Return the x position of the left portion of the key."
  (+ (staff-left) (* +space-width+ (key-position key num-keys))))

(defun key-center-x (key num-keys)
  (+ (key-left key num-keys) (/ +space-width+ 2)))

(defun draw-staff (tab scene num-bars)
  "Draw the staff's spaces where notes will reside and text labels to signal the keys' notes."
  (loop :with num-keys := (keys tab)
        :with timesig := (timesig tab)

        :with staff-top := (staff-top)
        :with body-height := (staff-body-height timesig num-bars)
        :with body-bottom := (staff-body-bottom timesig num-bars)

        :for key :from 1 :upto num-keys
        :for x := (key-left key num-keys)
        :for offset-height := (* +space-offset-y+ (floor (- num-keys key) 2))
        :for markedp := (markedp key)

        ;; Draw the key space
        :do (cl-svg:draw scene
                (:rect :x x :y staff-top
                       :width +space-width+
                       :height (+ body-height offset-height))
                :fill (if markedp
                          +space-color-marked+
                          +space-color-unmarked+)
                :stroke "black")

        ;; Draw the pitch label
        :do  (cl-svg:text scene (:x (+ x (/ +space-width+ 2))
                                 :y (+ body-bottom offset-height +font-size+)
                                 :style +note-text-style+)
               (string (key-pitch key)))))

(defun bar-bottom (bar-num timesig tab-bars)
  "Return the y position of the bottom of the given bar."
  (- (staff-body-bottom timesig tab-bars)
     (* bar-num (bar-height timesig))))

(defgeneric draw-construct (scene construct y tab))

(defmethod draw-construct (scene (beamed beamed) y tab)
  (print "No support for beamed constructs yet."))

(defun stem-left ()
  (- (staff-left) +stem-outreach+))

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
                           (+ stem-left +stem-taper-length+)
                           (- stem-y +stem-taper-length+)))
                 :stroke "black"
                 :stroke-width +note-thickness+
                 :fill "none")))

(defmethod draw-construct (scene (chord chord) note-y tab)
  (when (restp chord)
    (print "No support for rests, yet.")
    (return-from draw-construct))

  ;; Appropriately draw the approrpiate chord stem in the appropriate
  ;; style if so appropriate.
  (ecase (note chord)
    (1 nil) ; whole notes don't deserve a stem.
    (2 (draw-untapered-stem scene chord (keys tab) note-y))
    (4 (draw-untapered-stem scene chord (keys tab) note-y))
    (8 (draw-tapered-stem   scene chord (keys tab) note-y)))

  ;; Cord noteheads
  (loop :with hollow-head := (member (note chord) '(1 2))
        :for key :in (keys chord)
        :do (cl-svg:draw scene (:circle :cx (key-center-x key (keys tab))
                                        :cy note-y
                                        :r +note-radius+)
                         :fill (if hollow-head "none" "black")
                         :stroke "black"
                         :stroke-width +note-thickness+)))

(defun draw-bar (scene tab bar-num tab-bars)
  "Draw the bar's starting line and its notes."
  (let* ((timesig (timesig tab))
         (bar-bottom (bar-bottom bar-num timesig tab-bars))
         (staff-left (staff-left))
         (staff-right (staff-right (keys tab))))

    ;; bar line
    (cl-svg:draw scene (:line :x1 staff-left :x2 staff-right
                              :y1 bar-bottom :y2 bar-bottom)
                 :stroke-width +bar-line-thickness+
                 :stroke +bar-color+)
    ;; bar label
    (cl-svg:text scene (:x (+ +bar-label-margin+ staff-right) :y bar-bottom
                        :style +text-style+ :dominant-baseline "middle")
      (write-to-string (1+ bar-num)))

    ;; bar constructs
    (loop :for construct :in (nth bar-num (bars tab))
          :with note-y := (- bar-bottom +beat-height+)
          :do (draw-construct scene construct note-y tab)
          :do (decf note-y (* +beat-height+
                              (beat-length construct (beat-root timesig)))))))

(defun print-kab (kab &optional (stream *standard-output*))
  (let* ((num-bars (length (bars kab)))
         (num-keys (keys kab))
         (width (+ (staff-right num-keys) +staff-margin-x+))
         (height (+ (staff-bottom kab num-bars) +staff-margin-y+))
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width width :height height)))
    (cl-svg:draw scene (:rect :x 0 :y 0 :width width :height height)
                 :fill "white")
    (draw-staff kab scene num-bars)
    (loop :for bar-num :upto (1- num-bars)
          :do (draw-bar scene kab bar-num num-bars))
    (cl-svg:stream-out stream scene)))
