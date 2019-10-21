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

(defun eval-kab (tab-sexp)
  (destructuring-bind (deftablature title proplist
                       &body construct-sexps) tab-sexp
    (assert (string-equal (string deftablature) "DEFTABLATURE"))
    (let ((timesig (getf proplist :timesig (cons 4 4)))
          (keys (getf proplist :keys 17)))
      (loop :for sexp :in construct-sexps
            :collect (make-construct sexp) :into constructs
            :finally (return
                       (make-instance 'tablature
                                      :title title
                                      :timesig timesig
                                      :keys keys
                                      :constructs constructs))))))
