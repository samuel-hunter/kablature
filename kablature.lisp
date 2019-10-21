(defpackage #:kablature
  (:use #:cl)
  (:import-from :kablature.read
                :read-kab)
  (:import-from :kablature.eval
                :eval-kab)
  (:import-from :kablature.print
                :print-kab)
  (:export :read-kab
           :eval-kab
           :print-kab))

(in-package #:kablature)

(defun rep-file-path (pathname)
  (with-open-file (stream pathname :direction :input)
    (print-kab (eval-kab (read-kab stream)))))
