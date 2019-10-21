;;;; kablature.asd

(asdf:defsystem #:kablature
  :description "Describe kablature here"
  :author "Sam Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-svg)
  :components ((:file "model")
               (:file "read")
               (:file "eval")
               (:file "print")
               (:file "kablature")))
