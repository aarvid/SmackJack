;;;; smackjack-demo.asd

(asdf:defsystem #:smackjack-demo
  :serial t
  :depends-on (#:smackjack
               #:cl-who)
  :components ((:file "package")
               (:file "demo")
               ))

