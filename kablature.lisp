(defpackage #:kablature
  (:use #:cl)
  (:import-from :kablature.reader
                :read-kab)
  (:import-from :kablature.eval
                :eval-kab)
  (:export :read-kab
           :eval-kab
           :print-kab))

(in-package #:kablature)

(defun print-kab (kab))

(defun rep-file-path (pathname)
  (with-open-file (stream pathname :direction :input)
    (print-kab (eval-kab (read-kab stream)))))
