(defpackage #:kablature.read
  (:use #:cl)
  (:export :read-kab))

(in-package #:kablature.read)


(defun read-kab (&optional (stream *standard-input*))
  (read stream))
