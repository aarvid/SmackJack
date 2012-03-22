;;;; smackjack-demo.asd

(asdf:defsystem #:smackjack-demo
  :serial t
  :depends-on (#:smackjack
               #:local-time
               #:cl-who)
  :components ((:file "package")
               (:file "demo")
               ))

