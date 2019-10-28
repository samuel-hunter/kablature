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

(defparameter +whole-note-height+ (* 8 +space-width+))

(defparameter +bar-line-thickness+ 3)
(defparameter +bar-label-margin+ 5)
(defparameter +bar-line-height+ (* +whole-note-height+ 1/8))

(defparameter +note-thickness+ 1)
(defparameter +note-radius+ 4)
(defparameter +stem-outreach+ 20)
(defparameter +stem-taper-length+ 5)

(defparameter +staff-margin-x+ 50)
(defparameter +staff-margin-y+ 10)

(defparameter +font-size+ 15)
(defparameter +text-style+
  (format nil "+font-size+:~A;fill:black" +font-size+))
(defparameter +bar-color+ "black")

(defparameter +scene-height-min+ 800
  "The minimum height a singley-tabbed scene must be before being automatically split.")
(defparameter +scene-target-ratio+ 16/9
  "The target image resolution at which a scene should strive for.")
;;; (defparameter +scene-target-ratio+ 683/384) ;)

(defparameter +note-text-style+
  (concatenate 'string +text-style+ ";text-anchor:middle"))

(defclass staff ()
  ((tab :reader tab :initarg :tab)
   (scene :reader scene :initarg :scene)
   (staff-num :reader staff-num :initarg :num)
   (staff-body-bottom :reader staff-body-bottom :initarg :body-bottom)
   (bars :reader bars :initarg :bars)
   (bar-length :reader bar-length :initarg :bar-length)
   (bar-offset :reader bar-offset :initarg :bar-offset
               :documentation "Number of bars that came before this staff.")))

(defmethod timesig ((staff staff))
  (timesig (tab staff)))

(defun note-height (duration)
  "Return the height allocated to the given note's duration."
  (* duration +whole-note-height+))

(defun bar-height (timesig)
  "Return the height of a bar in the tablature."
  (+ +bar-line-height+
     (note-height (bar-duration timesig))))

(defun construct-height (construct)
  "Return the height of the construct."
  (note-height (duration construct)))

(defun staff-width* (tab)
  "Return the width of a staff."
  (* +space-width+ (keys tab)))

(defun staff-width (staff)
  "Return the width of a staff."
  (staff-width* (tab staff)))

(defun staff-body-height* (timesig tab-bars)
  "Return the height of the staff body."
  (* (bar-height timesig)
     tab-bars))

(defun staff-body-height (staff)
  "Return the height of the staff body."
  (staff-body-height* (timesig staff)
                      (bar-length staff)))

(defun staff-left (staff)
  "Return the left x position of the staff."
  (+ +staff-margin-x+ (* (staff-num staff)
                         (+ +staff-margin-x+
                            (staff-width staff)))))

(defun staff-right (staff)
  "Return the right x position of the staff."
  (* (1+ (staff-num staff))
     (+ +staff-margin-x+ (staff-width staff))))

(defun staff-top (staff)
  (- (staff-body-bottom staff)
     (staff-body-height staff)))

(defun max-staff-header-offset (tab)
  "Return the height the middle key reaches out in the staff header."
  (* +space-offset-y+ (floor (keys tab) 2)))

(defun staff-header-height (tab)
  (+ +font-size+ (max-staff-header-offset tab)))

(defun staff-height* (tab bar-length)
  (+ (staff-body-height* (timesig tab) bar-length)
     (staff-header-height tab)))

(defun scene-height (tab bars-per-staff)
  "Return the calculated height of the scene."
  (+ (* 2 +staff-margin-y+) (staff-height* tab bars-per-staff)))

(defun scene-width (tab staves-per-tab)
  "Return the calculated width of the scene."
  (+ +staff-margin-x+ (* staves-per-tab
                         (+ +staff-margin-x+ (staff-width* tab)))))

(defun make-staff (tab scene staff-num bars-per-staff)
  (let ((bar-length (min bars-per-staff (- (length (bars tab))
                                           (* bars-per-staff staff-num)))))
    (make-instance 'staff
                   :tab tab
                   :scene scene
                   :num staff-num
                   :body-bottom (- (scene-height tab bar-length)
                                   +staff-margin-y+ (staff-header-height tab))
                   :bars (nthcdr (* bars-per-staff staff-num) (bars tab))
                   :bar-length bar-length
                   :bar-offset (* bars-per-staff staff-num))))

(defun key-position (key staff)
  "Return the position of the key on the tablature, starting left."
  (let ((root-index (floor (1- (keys (tab staff))) 2)))
    (if (oddp key)
        (+ root-index (floor key 2))
        (- root-index (/ key 2)))))

(defun markedp (key)
  "Return whether the key's space should be marked."
  (zerop (mod (floor key 2) 3)))

(defun key-pitch (key)
  "Return the pitch letter associated with the key."
  (char +octave-notes+ (mod (+ key +octave-root+ -1) (length +octave-notes+))))

(defun key-left (key staff)
  "Return the x position of the left portion of the key."
  (+ (staff-left staff) (* +space-width+ (key-position key staff))))

(defun key-center-x (key staff)
  (+ (key-left key staff) (/ +space-width+ 2)))

(defun draw-staff (staff)
  "Draw the staff's spaces where notes will reside and text labels to signal the keys' notes."
  (loop :with num-keys := (keys (tab staff))
        :with scene := (scene staff)
        :with staff-top := (staff-top staff)
        :with body-height := (staff-body-height staff)
        :with body-bottom := (staff-body-bottom staff)

        :for key :from 1 :upto num-keys
        :for x := (key-left key staff)
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

(defun bar-bottom (bar-num staff)
  "Return the y position of the bottom of the given bar."
  (- (staff-body-bottom staff)
     (* bar-num (bar-height (timesig staff)))))

(defgeneric draw-construct (scene construct y staff))

(defmethod draw-construct (scene (beamed beamed) y staff)
  (print "No support for beamed constructs yet."))

(defun stem-left (staff)
  (- (staff-left staff) +stem-outreach+))

(defun rightmost-key (chord staff)
  (loop :for key :in (keys chord)
        :with result := nil
        :with result-pos := -1
        :do (let ((pos (key-position key staff)))
              (when (> pos result-pos)
                (setf result-pos pos)
                (setf result key)))
        :finally (return result)))

(defun stem-right (chord staff)
  (key-center-x (rightmost-key chord staff)
                staff))

(defun stem-y (note-y)
  "Return the y-position of the note's stem."
  (+ (- note-y +note-radius+)))

(defun draw-untapered-stem (scene chord note-y staff)
  (let ((stem-left (stem-left staff))
        (stem-right (stem-right chord staff))
        (stem-y (stem-y note-y)))

    (cl-svg:draw scene (:line :x1 stem-left :y1 stem-y
                              :x2 stem-right :y2 stem-y)
                 :stroke "black"
                 :stroke-width +note-thickness+)))

(defun draw-tapered-stem (scene chord note-y staff)
  (let ((stem-left (stem-left staff))
        (stem-right (stem-right chord staff))
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

(defmethod draw-construct (scene (chord chord) note-y staff)
  (when (restp chord)
    (print "No support for rests, yet.")
    (return-from draw-construct))

  ;; Appropriately draw the approrpiate chord stem in the appropriate
  ;; style if so appropriate.
  (ecase (note chord)
    (1 nil) ; whole notes don't deserve a stem.
    (2 (draw-untapered-stem scene chord note-y staff))
    (4 (draw-untapered-stem scene chord note-y staff))
    (8 (draw-tapered-stem   scene chord note-y staff)))

  ;; Cord noteheads
  (loop :with hollow-head := (member (note chord) '(1 2))
        :for key :in (keys chord)
        :do (cl-svg:draw scene (:circle :cx (key-center-x key staff)
                                        :cy note-y
                                        :r +note-radius+)
                         :fill (if hollow-head "none" "black")
                         :stroke "black"
                         :stroke-width +note-thickness+)))

(defun draw-bar (bar-num staff)
  "Draw the bar's starting line and its notes."
  (let ((bar-bottom (bar-bottom bar-num staff))
        (scene (scene staff))
        (staff-left (staff-left staff))
        (staff-right (staff-right staff)))

    ;; bar line
    (cl-svg:draw scene (:line :x1 staff-left :x2 staff-right
                              :y1 bar-bottom :y2 bar-bottom)
                 :stroke-width +bar-line-thickness+
                 :stroke +bar-color+)
    ;; bar label
    (cl-svg:text scene (:x (+ +bar-label-margin+ staff-right) :y bar-bottom
                        :style +text-style+ :dominant-baseline "middle")
      (write-to-string (+ 1 bar-num (bar-offset staff))))

    ;; bar constructs
    (loop :for construct :in (nth bar-num (bars staff))
          :with note-y := (- bar-bottom +bar-line-height+)
          :do (draw-construct scene construct note-y staff)
          :do (decf note-y (construct-height construct)))))

(defun staves-per-tab (tab bars-per-staff)
  (ceiling (length (bars tab)) bars-per-staff))

(defun make-scene-and-staves (kab bars-per-staff)
  (let* ((staves-per-tab (staves-per-tab kab bars-per-staff))
         (width (scene-width kab staves-per-tab))
         (height (scene-height kab bars-per-staff))
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width width
                                          :height height)))
    (cl-svg:draw scene (:rect :x 0 :y 0 :width width :height height)
                 :fill "white")
    (loop :for staff-num :upto (1- staves-per-tab)
          :collect (make-staff kab scene staff-num bars-per-staff) :into staves
          :finally (return (values scene staves)))))

(defun ideal-bars-per-staff (tab)
  (let ((bar-length (length (bars tab))))
    ;; Return early when the simple single-staff solution is short enough.
    (when (< (scene-height tab bar-length)
             +scene-height-min+)
      (return-from ideal-bars-per-staff bar-length))


    (labels ((ratio (bars-per-staff)
               (/ (scene-width tab (staves-per-tab tab bars-per-staff))
                  (scene-height tab bars-per-staff)))
             (ratio-error (bars-per-staff)
               (/ (abs (- (ratio bars-per-staff) +scene-target-ratio+))
                  +scene-target-ratio+)))
      (loop :with best-bars-per-staff := bar-length ; if something
                                                    ; goes wrong and
                                                    ; bars isn't set,
                                                    ; default to one
                                                    ; big stretch.
            :with best-error := 9999 ; absurdly high percent error.
            :for bars-per-staff :from 2 :to 12 :by 2 ; Even groupings
                                                     ; to appeal to
                                                     ; traditional
                                                     ; western music
            :for ratio-error := (ratio-error bars-per-staff)
            :do (when (< ratio-error
                         best-error)
                  (setf best-bars-per-staff bars-per-staff)
                  (setf best-error ratio-error))
            :finally (return best-bars-per-staff)))
    ))

(defun print-kab (kab &optional (stream *standard-output*))
  (multiple-value-bind (scene staves) (make-scene-and-staves kab (ideal-bars-per-staff kab))
    (loop :for staff :in staves
          :do (draw-staff staff)
          :do (loop :for bar-num :upto (1- (bar-length staff))
                    :do (draw-bar bar-num staff)))
    (cl-svg:stream-out stream scene)))
