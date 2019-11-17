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

(defparameter +whole-note-height+ (* 12 +space-width+))

(defparameter +bar-line-thickness+ 3)
(defparameter +bar-label-margin+ 5)
(defparameter +bar-line-height+ (* +whole-note-height+ 1/8))

(defparameter +conclusion-line-height+ (* +whole-note-height+ 1/8))
(defparameter +conclusion-line-thickness+ +bar-line-thickness+)

(defparameter +note-thickness+ 1)
(defparameter +note-radius+ 4)
(defparameter +note-dot-offset+ 6)
(defparameter +note-dot-radius+ 2)
(defparameter +stem-outreach+ 20)
(defparameter +stem-taper-length+ 5)
(defparameter +aux-taper-distance+ 5)
(defparameter +beam-thickness+ 2)
(defparameter +double-beam-offset+ 4
  "Distance from first beam to second beam for sixteenth notes")

(defparameter +hat-height+ 10)
(defparameter +quarter-rest-thickness+ 2)

(defparameter +staff-margin-x+ 50)
(defparameter +staff-margin-y+ 10)

(defparameter +font-size+ 15)
(defparameter +text-style+
  (format nil "+font-size+:~A;fill:black" +font-size+))
(defparameter +bar-color+ "black")
(defparameter +repeat-color+ "blue"
  "The color to mark a repeat sign and an an opening repeat sign's bar line")

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
               :documentation "Number of bars that came before this staff.")
   (last-staffp :reader last-staffp :initarg :last-staffp)))

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
  (+ (* (bar-height timesig)
        tab-bars)
     +conclusion-line-height+))

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
                                           (* bars-per-staff staff-num))))
        (bar-offset (* bars-per-staff staff-num)))
    (make-instance 'staff
                   :tab tab
                   :scene scene
                   :num staff-num
                   :body-bottom (- (scene-height tab bar-length)
                                   +staff-margin-y+ (staff-header-height tab))
                   :bars (nthcdr (* bars-per-staff staff-num) (bars tab))
                   :bar-length bar-length
                   :bar-offset bar-offset
                   :last-staffp (= (+ bar-offset bar-length)
                                   (length (bars tab))))))

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

(defun key-accidental (pitch tab)
  (char (accidentals tab)
        (- (char-code pitch) (char-code #\A))))

(defun key-string (key staff)
  "Return the string version of the key pitch being played"
  (let* ((pitch (key-pitch key))
         (accidental (key-accidental pitch (tab staff))))
    (concatenate 'string
                 (string pitch)
                 (ecase accidental
                   ;; Workaround: Some SVG implementations don't
                   ;; render the proper musical Flat and Sharp signs,
                   ;; so I'm using the ASCII pound and sharp sign.
                   (#\- "") ; Natural
                   (#\b "b") ; flat
                   (#\# "#"))))) ; sharp

(defun key-left (key staff)
  "Return the x position of the left portion of the key."
  (+ (staff-left staff) (* +space-width+ (key-position key staff))))

(defun staff-center (staff)
  (key-left 1 staff))

(defun key-center-x (key staff)
  (+ (key-left key staff) (/ +space-width+ 2)))

(defun draw-conclusion-line (staff)
  "Draw the ending lines on the staff that illustrates the end of the song."
  (let* ((scene (scene staff))
         (staff-top (staff-top staff))
         (staff-left (staff-left staff))
         (staff-right (staff-right staff))
         (double-bar-y (+ staff-top 10)))
    (cl-svg:draw scene (:line :x1 staff-left :y1 staff-top
                              :x2 staff-right :y2 staff-top)
                 :stroke-width +conclusion-line-thickness+
                 :stroke "black")
    (cl-svg:draw scene (:line :x1 staff-left :y1 double-bar-y
                              :x2 staff-right :y2 double-bar-y)
                 :stroke "black")))

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
               (key-string key staff)))

  ;; Draw thick line down middle of staff
  (cl-svg:draw (scene staff)
      (:line :x1 (staff-center staff) :y1 (staff-top staff)
             :x2 (staff-center staff) :y2 (+ (staff-body-bottom staff)
                                             (max-staff-header-offset (tab staff))))
      :stroke "black"
      :stroke-width +bar-line-thickness+)
  (when (last-staffp staff)
    (draw-conclusion-line staff)))

(defun bar-bottom (bar-num staff)
  "Return the y position of the bottom of the given bar."
  (- (staff-body-bottom staff)
     (* bar-num (bar-height (timesig staff)))))

(defun bar-top (bar-num staff)
  (bar-bottom (1+ bar-num) staff))

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

(defun draw-untapered-stem (chord note-y staff)
  (let ((stem-left (stem-left staff))
        (stem-right (stem-right chord staff))
        (stem-y (stem-y note-y)))

    (cl-svg:draw (scene staff)
        (:line :x1 stem-left :y1 stem-y
               :x2 stem-right :y2 stem-y)
        :stroke "black"
        :stroke-width +note-thickness+)))

(defun draw-tapered-stem (chord note-y staff &key (aux-tapers 0))
  (let ((stem-left (stem-left staff))
        (stem-right (stem-right chord staff))
        (stem-y (stem-y note-y)))
    (cl-svg:draw (scene staff)
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
                 :fill "none")
    (loop :for aux-taper :from 1 :to aux-tapers
          :for aux-left := (+ stem-left +aux-taper-distance+)
          :do (cl-svg:draw (scene staff)
                  (:line :x1 aux-left :y1 stem-y
                         :x2 (+ aux-left +stem-taper-length+)
                         :y2 (- stem-y +stem-taper-length+))
                  :stroke "black"
                  :stroke-width +note-thickness+))))

(defun draw-note-dot (key note-y staff)
  (let ((dot-x (+ +note-dot-offset+
                  (key-center-x key staff)))
        (dot-y (- note-y +note-dot-offset+)))
    (cl-svg:draw (scene staff) (:circle :cx dot-x
                                :cy dot-y
                                :r +note-dot-radius+)
                 :fill "black")))

(defun draw-hat (note-y staff direction key dottedp)
  (let ((x (ecase direction
             (:left (key-left key staff))
             (:right (+ (/ +space-width+ 2)
                        (key-left key staff)))))
        (scene (scene staff)))
    (cl-svg:draw scene (:rect :x x :y note-y
                                      :width (/ +space-width+ 2)
                                      :height +hat-height+))
    (when dottedp
      (draw-note-dot key note-y staff))))

(defun draw-whole-rest (note-y staff dottedp)
  (draw-hat note-y staff :left 4 dottedp)
  (draw-hat note-y staff :left 7 dottedp))

(defun draw-half-rest (note-y staff dottedp)
  (draw-hat note-y staff :right 6 dottedp)
  (draw-hat note-y staff :right 5 dottedp))

(defun draw-quarter-rest (note-y staff dottedp)
  (let* ((start-x (staff-center staff))
         ; polylines require the points formatted in an X,Y X,Y... pattern.
         (points (format nil "~$,~$ ~$,~$ ~$,~$ ~$,~$ ~$,~$ ~$,~$"
                       start-x note-y
                       (+ start-x (* 1/2 +space-width+)) (- note-y 4)
                       (+ start-x +space-width+) note-y
                       (+ start-x (* 4/3 +space-width+)) (- note-y 4)
                       (+ start-x (* 5/3 +space-width+)) note-y
                       (+ start-x (* 2 +space-width+)) (- note-y 2))))
    (cl-svg:draw (scene staff) (:polyline :points points)
                 :stroke "black"
                 :stroke-width +quarter-rest-thickness+
                 :fill "none"))
  (when dottedp
    (draw-note-dot 3 note-y staff)))

(defun draw-stemmed-rest (note-y staff dottedp &key (heads 1))
  (let ((scene (scene staff))
        (half-height (/ +space-width+ 2)))
    (cl-svg:draw scene (:line :x1 (key-center-x 4 staff)
                              :y1 (+ note-y half-height)
                              :x2 (key-center-x 6 staff)
                              :y2 (- note-y half-height))
                 :stroke "black")
    (cl-svg:draw scene (:line :x1 (key-center-x 5 staff)
                              :y1 (+ note-y half-height)
                              :x2 (key-center-x 3 staff)
                              :y2 (- note-y half-height))
                 :stroke "black")
    (loop :for head :to (1- heads)
          :for head-offset := (* 2/8 +space-width+ head)
          :do (cl-svg:draw scene (:circle :cx (+ (key-center-x 6 staff)
                                                 (* 3/16 +space-width+)
                                                 head-offset)
                                          :cy (+ note-y
                                                 (* -1/8 +space-width+)
                                                 head-offset)
                                          :r (* 1/8 +space-width+))
                           :fill "black")
          :do (cl-svg:draw scene (:circle :cx (+ (key-center-x 3 staff)
                                                 (* 3/16 +space-width+)
                                                 head-offset)
                                          :cy (+ note-y
                                                 (* -1/8 +space-width+)
                                                 head-offset)
                                          :r (* 1/8 +space-width+))
                           :fill "black")))
  (when dottedp
    (draw-note-dot 6 note-y staff)
    (draw-note-dot 3 note-y staff)))

(defun draw-note-heads (chord note-y staff)
  (loop :with hollow-head := (member (note chord) '(1 2))
        :with scene := (scene staff)
        :for key :in (keys chord)
        :do (cl-svg:draw scene (:circle :cx (key-center-x key staff)
                                        :cy note-y
                                        :r +note-radius+)
                         :fill (if hollow-head "white" "black")
                         :stroke "black"
                         :stroke-width +note-thickness+)
        :when (dottedp chord)
          :do (draw-note-dot key note-y staff)))

(defgeneric draw-construct (construct note-y staff))

(defmethod draw-construct ((chord chord) note-y staff)
  (when (restp chord)
    (with-slots (dottedp) chord
      (ecase (note chord)
        (1 (draw-whole-rest note-y staff dottedp))
        (2 (draw-half-rest note-y staff dottedp))
        (4 (draw-quarter-rest note-y staff dottedp))
        (8 (draw-stemmed-rest note-y staff dottedp))
        (16 (draw-stemmed-rest note-y staff dottedp :heads 2))))
    (return-from draw-construct))

  ;; Appropriately draw the approrpiate chord stem in the appropriate
  ;; style if so appropriate.
  (ecase (note chord)
    (1 nil) ; whole notes don't deserve a stem.
    (2  (draw-untapered-stem chord note-y staff))
    (4  (draw-untapered-stem chord note-y staff))
    (8  (draw-tapered-stem   chord note-y staff))
    (16 (draw-tapered-stem   chord note-y staff :aux-tapers 1)))

  (draw-note-heads chord note-y staff))

(defun draw-beam (staff beam-bottom beam-top beam-x)
  (cl-svg:draw (scene staff)
      (:line :x1 beam-x :y1 beam-bottom
             :x2 beam-x :y2 beam-top)
      :stroke "black"
      :stroke-width +beam-thickness+))

(defmacro dochords ((chord-sym chords chord-y-sym note-y) &body body)
  `(loop :for ,chord-sym :in ,chords
         :with ,chord-y-sym := ,note-y
         :do (progn ,@body)
         :do (decf ,chord-y-sym (construct-height ,chord-sym))))

(defmethod draw-construct ((beamed beamed) note-y staff)
  (dolist (chord (chords beamed))
    (assert (member (note chord) '(8 16)))
    (assert (not (restp chord))))

  (let ((chords (chords beamed))
        last-chord-y)
    (dochords (chord chords chord-y note-y)
      (draw-untapered-stem chord chord-y staff)
      (draw-note-heads chord chord-y staff)
      (setf last-chord-y chord-y))

    ;; beam
    (let ((beam-x (stem-left staff))
          (beam-bottom (+ (stem-y note-y) (/ +note-thickness+ 2)))
          (beam-top (- (stem-y last-chord-y) (/ +note-thickness+ 2))))
      (draw-beam staff beam-bottom beam-top beam-x))

    ;; draw sixteenth note beams
    (let (beam-start beam-end)
      (dochords (chord chords chord-y note-y)
        (when (and (null beam-start)
                   (= (note chord) 16))
          ;; record the start of the sixteenth note beam if it hasn't
          ;; been started yet.
          (setf beam-start
                (if (eq chord (first chords))
                    chord-y
                    (+ chord-y (note-height (/ 32))))))

        ;; update the end of the sixteenth beam
        (when (and beam-start
                   (= (note chord) 16))
          (setf beam-end
                (- chord-y (note-height (/ 32)))))

        ;; draw the end of the sixteenth note beam.
        (when (and beam-start
                   (= (note chord) 8))
          (draw-beam staff
                     (stem-y beam-start)
                     (stem-y beam-end)
                     (+ +double-beam-offset+
                        (stem-left staff)))
          (setf beam-start nil))

        ;; Finally, draw the sixteenth note beam if at the end of the
        ;; loop
        (when (and beam-start
                   (= (note chord) 16)
                   (eq chord (car (last chords))))
          (draw-beam staff
                     (stem-y beam-start)
                     (stem-y chord-y)
                     (+ +double-beam-offset+
                        (stem-left staff))))))))

(defun draw-bar (bar-num staff)
  "Draw the bar's starting line and its notes."
  (let ((bar-bottom (bar-bottom bar-num staff))
        (bar-top (bar-top bar-num staff ))
        (scene (scene staff))
        (tab (tab staff))
        (staff-left (staff-left staff))
        (staff-right (staff-right staff))
        (absolute-bar-num (+ 1 bar-num (bar-offset staff))))

    ;; bar line
    (cl-svg:draw scene (:line :x1 staff-left :x2 staff-right
                              :y1 bar-bottom :y2 bar-bottom)
                 :stroke-width +bar-line-thickness+
                 :stroke (if (member absolute-bar-num (repeat-starts tab))
                             +repeat-color+
                             +bar-color+))

    (when (member absolute-bar-num (repeat-starts tab))
      (cl-svg:draw scene (:circle :cx (key-center-x 4 staff)
                                  :cy (- bar-bottom
                                         (* 2 +bar-line-thickness+))
                                  :r +bar-line-thickness+)
                   :fill +repeat-color+)
      (cl-svg:draw scene (:circle :cx (key-center-x 3 staff)
                                  :cy (- bar-bottom
                                         (* 2 +bar-line-thickness+))
                                  :r +bar-line-thickness+)
                   :fill +repeat-color+))

    (when (member absolute-bar-num (repeat-ends tab))
      (cl-svg:draw scene (:circle :cx (key-center-x 4 staff)
                                  :cy (+ bar-top
                                         (* 2 +bar-line-thickness+))
                                  :r +bar-line-thickness+)
                   :fill +repeat-color+)
      (cl-svg:draw scene (:circle :cx (key-center-x 3 staff)
                                  :cy (+ bar-top
                                         (* 2 +bar-line-thickness+))
                                  :r +bar-line-thickness+)
                   :fill +repeat-color+))
    ;; bar label
    (cl-svg:text scene (:x (+ +bar-label-margin+ staff-right) :y bar-bottom
                        :style +text-style+ :dominant-baseline "middle")
      (write-to-string absolute-bar-num))

    ;; bar constructs
    (loop :for construct :in (nth bar-num (bars staff))
          :with note-y := (- bar-bottom +bar-line-height+)
          :do (draw-construct construct note-y staff)
          :do (decf note-y (construct-height construct)))))

(defun staves-per-tab (tab bars-per-staff)
  (ceiling (length (bars tab)) bars-per-staff))

(defun make-scene-and-staves (kab bars-per-staff)
  "Return the scene and a list of its staves as two values. If
`BARS-PER-STAFF' is a non-positive integer, the scene will only make
one staff."
  (let* ((bars-per-staff* (if (> bars-per-staff 0)
                              bars-per-staff
                              (length (bars kab))))
         (staves-per-tab (if (> bars-per-staff 0)
                             (staves-per-tab kab bars-per-staff)
                             1))
         (width (scene-width kab staves-per-tab))
         (height (scene-height kab bars-per-staff*))
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width width
                                          :height height)))
    (cl-svg:draw scene (:rect :x 0 :y 0 :width width :height height)
                 :fill "white")
    (loop :for staff-num :upto (1- staves-per-tab)
          :collect (make-staff kab scene staff-num bars-per-staff*) :into staves
          :finally (return (values scene staves)))))

(defun ideal-bars-per-staff (tab)
  (let ((bar-length (length (bars tab))))
    ;; Return early when the simple single-staff solution is short enough.
    (when (< (scene-height tab bar-length)
             +scene-height-min+)
      (return-from ideal-bars-per-staff bar-length))


    (labels ((ratio (bars-per-staff)
               (/ (scene-width tab (staves-per-tab tab bars-per-staff))
                  (scene-height tab bars-per-staff))))
      (loop
        ;; Even groupings to appeal to traditional western music
        :for bars-per-staff :from 2 :by 2
        ;; Err on the scene being taller than the target vs. wider.
        :until (< (ratio bars-per-staff) +scene-target-ratio+)
        :finally (return bars-per-staff)))))

(defun print-kab (kab &optional (stream *standard-output*) &key bars-per-staff)
  (multiple-value-bind (scene staves) (make-scene-and-staves kab (or bars-per-staff
                                                                     (bars-per-staff kab)
                                                                     (ideal-bars-per-staff kab)))
    (loop :for staff :in staves
          :do (draw-staff staff)
          :do (loop :for bar-num :upto (1- (bar-length staff))
                    :do (draw-bar bar-num staff)))
    (cl-svg:stream-out stream scene)))
