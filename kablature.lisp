(defpackage #:kablature
  (:use #:cl)
  (:import-from :kablature.reader
                :read-kab)
  (:export :read-kab
           :eval-kab
           :print-kab))

(in-package #:kablature)

(defun eval-kab (kab)
  kab)

(defun print-kab (kab)
  (print kab))
