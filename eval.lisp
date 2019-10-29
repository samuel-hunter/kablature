(defpackage #:kablature.eval
  (:use #:cl #:kablature.model)
  (:export :eval-kab))

(in-package #:kablature.eval)

(defun make-chord (note-sexp)
  (destructuring-bind (note dottedp &rest keys) note-sexp
    (check-type note integer)
    (check-type dottedp boolean)
    (loop :for key :in keys :do (check-type key integer))
    (make-instance 'chord :note note :dottedp dottedp :keys keys)))

(defun make-beamed (beamed-sexp)
  (destructuring-bind (beamed &rest chord-sexps) beamed-sexp
    (assert (string-equal (string beamed) "BEAMED"))
    (loop :for chord-sexp :in chord-sexps
          :collect (make-chord chord-sexp) :into chords
          :finally (return (make-instance 'beamed :chords chords)))))

(defun make-construct (sexp)
  (let ((first-elem (first sexp)))
    (cond
      ((eq 'beamed first-elem) (make-beamed sexp))
      ((typep first-elem 'integer) (make-chord sexp))
      (t (error "Unexpected element ~s in ~s" first-elem sexp)))))

(defun group-constructs (constructs timesig)
  "Group constructs together into bars."
  (loop :with bars := ()
        :with bar-constructs := ()
        :with beats := 0

        :for construct :in constructs
        :do (incf beats (beat-length construct (beat-root timesig)))
        :do (push construct bar-constructs)
        :do (cond
              ((= beats (beats-per-bar timesig))
               (push (nreverse bar-constructs) bars)
               (setf bar-constructs ())
               (setf beats 0))
              ((> beats (beats-per-bar timesig))
               (error "Found ~S beats in bar ~S by construct ~S."
                      beats (length bars) construct)))
        :finally (return (nreverse bars))))

(defun eval-kab (tab-sexp)
  (destructuring-bind (deftablature title proplist
                       &body construct-sexps) tab-sexp
    (assert (string-equal (string deftablature) "DEFTABLATURE"))
    (check-type title string)

    (let ((timesig (getf proplist :timesig (cons 4 4)))
          (keys (getf proplist :keys 17)))
      (check-type timesig (cons integer integer))
      (check-type keys integer)

      (loop :for sexp :in construct-sexps
            :collect (make-construct sexp) :into constructs
            :finally (return
                       (make-instance
                        'tablature
                        :title title
                        :timesig timesig
                        :keys keys
                        :bars
                        (group-constructs constructs
                                          timesig)))))))
