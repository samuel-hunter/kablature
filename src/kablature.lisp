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

(defun rep-file-path (stream-in stream-out &optional bars-per-staff)
  (print-kab (eval-kab (read-kab stream-in))
             stream-out
             :bars-per-staff bars-per-staff))

;; This is mainly for quick-testing that features in the print module
;; is being added correctly. It assumes a POSIX filesystem and that
;; the program `xdg-open' is available.
(defun preview (&optional (pathname (merge-pathnames #P"examples/hot-cross-buns.lisp"
                                                     (asdf:system-source-directory :kablature)))
                  (out-path "/tmp/kablature.svg"))
  (with-open-file (stream-in pathname :direction :input)
    (with-open-file (stream-out out-path :direction :output :if-exists :supersede)
      (rep-file-path stream-in stream-out)))
  (sb-ext:run-program "/usr/bin/xdg-open" (list out-path)))

(defun unknown-option (condition)
  (format *error-output* "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt &optional (result (gensym))) &body body)
  `(let ((,result (getf ,options ,opt)))
     (when ,result
       ,@body)))

(defmacro with-open-file-when ((stream filespec default-stream &rest options) &body body)
  `(if ,filespec
       (with-open-file (,stream ,filespec ,@options)
         ,@body)
       (let ((,stream ,default-stream))
         ,@body)))

(defun show-help (&optional error-code)
  (opts:describe
   :prefix "Convert a tablature file into a visual SVG."
   :suffix "If FILE is not provided, kablature reads from standard input."
   :usage-of (first (opts:argv))
   :args "FILE")
  (when error-code
    (opts:exit error-code)))

(defmacro show-error-and-die (control-string &rest format-arguments)
  `(progn
     (format *error-output* ,(concatenate 'string "~A: " control-string "~%")
             (first (opts:argv))
             ,@format-arguments)
     (opts:exit 1)))

(defun main ()
  (opts:define-opts
    (:name :help
     :description "Print this help text"
     :short #\h
     :long "help")
    (:name :output
     :description "Place output into OUTPUT (default standard output)"
     :short #\o
     :long "output"
     :arg-parser #'identity
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
          (show-help 1))
        (opts:arg-parser-failed (condition)
          (format *error-output* "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition))
          (show-help 1))
        (opts:missing-required-option (con)
          (format *error-output* "fatal: ~a~%" con)
          (show-help 1)))

    (when-option (options :help)
      (show-help)
      (return-from main))

    (when (> (length free-args) 1)
        (format *error-output* "fatal: too many arguments.~%")
        (show-help 1))

    (handler-case
        (with-open-file-when (stream-out (getf options :output) *standard-output*
                                         :direction :output :if-exists :supersede)
          (with-open-file-when (stream-in (first free-args) *standard-input*
                                          :direction :input)
            (rep-file-path stream-in stream-out (getf options :bars))))
      (error (err)
        (show-error-and-die "fatal: ~A." err)))))
