(defpackage #:kablature.parse
  (:use #:cl #:kablature.model)
  (:import-from #:alexandria
                :destructuring-case)
  (:export :eval-kab))

(in-package #:kablature.parse)

(defun parse-chord (s-expression)
  (destructuring-bind (note dottedp &rest keys) s-expression
    (make-chord note dottedp keys)))

(defun parse-beamed (s-expression)
  (destructuring-bind (beamed &rest chord-s-expressions) s-expression
    (assert (eq beamed :beamed)) ; Shouldn't be logically possible,
                                 ; but just in case!
    (make-beamed (mapcar 'parse-chord (rest s-expression)))))

(defun parse-construct (s-expression)
  (let ((first-elem (first s-expression)))
    (cond
      ((eq first-elem :beamed) (parse-beamed s-expression))
      ((typep first-elem 'fixnum) (parse-chord s-expression))
      (t (error "Malformed expression ~S" s-expression)))))

(defun parse-tablature (s-expression)
  (destructuring-bind (deftablature title (&key (timesig '(4 . 4))
                                             (keys 17)
                                             bars-per-staff
                                             repeats
                                             (accidentals "-------")
                                             &allow-other-keys)
                       &body construct-s-expressions) s-expression
    (assert (eq deftablature :deftablature))
    (make-tablature title
                    keys
                    timesig
                    (mapcar 'parse-construct
                            construct-s-expressions)
                    accidentals
                    repeats
                    bars-per-staff)))

(defun eval-kab (tab-sexp)
  (parse-tablature tab-sexp))
