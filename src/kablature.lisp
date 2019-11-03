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
           :print-kab
           :main))

(in-package #:kablature)

(defparameter +default-output+ "out.svg")

(defun rep-file-path (pathname &key output bars-per-staff)
  (with-open-file (stream pathname :direction :input)
    (let ((kab (eval-kab (read-kab stream))))
      (if output
          (print-kab kab output :bars-per-staff bars-per-staff)
          (print-kab kab :bars-per-staff bars-per-staff)))))

;; This is mainly for quick-testing that features in the print module
;; is being added correctly. It assumes a POSIX filesystem and that
;; the program `xdg-open' is available.
(defun preview (&optional (pathname (merge-pathnames #P"examples/hot-cross-buns.lisp"
                                                     (asdf:system-source-directory :kablature)))
                       (out-path "/tmp/kablature.svg"))
  (with-open-file (outstream out-path :direction :output :if-exists :supersede)
    (rep-file-path pathname :output outstream))
  (sb-ext:run-program "/usr/bin/xdg-open" (list out-path)))

(defun unknown-option (condition)
  (format *error-output* "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt &optional (result (gensym))) &body body)
  `(let ((,result (getf ,options ,opt)))
     (when ,result
       ,@body)))

(defun show-help ()
  (opts:describe
   :prefix "Convert a tablature file into a visual SVG."
   :usage-of (first (opts:argv))
   :args "FILE"))

(defun main ()
  (opts:define-opts
    (:name :help
     :description "Print this help text"
     :short #\h
     :long "help")
    (:name :output
     :description (format nil "Place output into OUTPUT (default ~A)" +default-output+)
     :short #\o
     :long "output"
     :arg-parser (lambda (x) (make-pathname :name x))
     :meta-var "OUTPUT")
    (:name :bars
     :description "Force the tablature output to a specific number of bars per staff."
     :short #\b
     :long "bars"
     :arg-parser #'parse-integer
     :meta-var "BARS"))

  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option 'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format *error-output* "fatal: option ~s needs an argument!~%"
                  (opts:option condition))
          (opts:exit 1))
        (opts:arg-parser-failed (condition)
          (format *error-output* "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition))
          (opts:exit 1))
        (opts:missing-required-option (con)
          (format *error-output* "fatal: ~a~%" con)
          (opts:exit 1)))
    (when-option (options :help)
      (show-help)
      (return-from main))
    (let ((out-path +default-output+)
          in-path)
      (when-option (options :output it)
        (setf out-path it))
      (unless (= (length free-args) 1)
        (format *error-output* "fatal: expected 1 free argument.~%")
        (show-help)
        (opts:exit 1))
      (setf in-path (make-pathname :name (first free-args)))
      (with-open-file (outstream out-path :direction :output :if-exists :supersede)
        (rep-file-path in-path :output outstream
                               :bars-per-staff (getf options :bars))))))
