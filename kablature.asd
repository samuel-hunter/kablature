;;;; kablature.asd

(asdf:defsystem #:kablature
  :description "Describe kablature here"
  :author "Sam Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:cl-svg #:unix-opts)
  :build-operation "program-op"
  :build-pathname "../kablature"
  :entry-point "kablature:main"
  :serial t
  :pathname "src"
  :components ((:file "model")
               (:file "read")
               (:file "parse")
               (:file "print")
               (:file "kablature")))
