;;;; kablature.asd

(asdf:defsystem #:kablature
  :description "Describe kablature here"
  :author "Sam Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:cl-svg)
  :serial t
  :pathname "src"
  :components ((:file "model")
               (:file "read")
               (:file "eval")
               (:file "print")
               (:file "kablature")))
