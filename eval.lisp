(defpackage #:kablature.eval
  (:use #:cl #:kablature.model)
  (:export :eval-kab))

(in-package #:kablature.eval)

;; FIXME: 4/4 timesignature is hardcoded in this procedure.
(defun count-measures (tab)
  "Count the measures within a tablature"
  (loop :with duration := 0
        :and measures := 0
        :for construct :in (constructs kab)
        :do (progn
              (incf duration (duration construct))
              (assert (<= duration 1))
              (when (= duration 1)
                (incf measures)
                (setf duration 0)))
        :finally (return measures)))

(defun eval-kab (kab)
  (format t "Measures: ~A~&" (count-measures kab))
  kab)
