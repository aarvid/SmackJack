;;;; smackjack-demo.asd

(asdf:defsystem #:smackjack-demo
  :serial t
  :depends-on (#:smackjack
               #:local-time
               #:cl-who
               #:cl-containers)
  :components ((:file "package")
               (:file "demo")
               (:file "demo-push")
               ))

