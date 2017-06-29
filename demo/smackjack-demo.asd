;;;; smackjack-demo.asd

(asdf:defsystem "smackjack-demo"
  :serial t
  :version "0.1"
  :description "A simple demo of smackjack and ajax along with a demo of ajax-push"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on ("smackjack"
               "local-time"
               "cl-who"
               "cl-containers")
  :components ((:file "package")
               (:file "demo")
               (:file "demo-push")
               ))

