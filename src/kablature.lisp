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

(defun rep-file-path (pathname &key output)
  (with-open-file (stream pathname :direction :input)
    (let ((kab (eval-kab (read-kab stream))))
      (if output
          (print-kab kab output)
          (print-kab kab)))))

;; This is mainly for quick-testing that features in the print module
;; is being added correctly. It assumes a POSIX filesystem and that
;; the program `xdg-open' is available.
(defun preview (&optional (pathname (merge-pathnames #P"examples/hot-cross-buns.lisp"
                                                     (asdf:system-source-directory :kablature)))
                       (out-path "/tmp/kablature.svg"))
  (with-open-file (outstream out-path :direction :output :if-exists :supersede)
    (rep-file-path pathname :output outstream)
    (sb-ext:run-program "/usr/bin/xdg-open" (list out-path))))
