(defpackage #:kablature
  (:use #:cl)
  (:export :read-kab
           :eval-kab
           :print-kab))

(in-package #:kablature)

(defun read-kab (&optional (stream *standard-input*))
  (read stream))

(defun eval-kab (kab)
  kab)

(defun print-kab (kab)
  (print kab))
