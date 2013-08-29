;;;; smackjack.asd

(asdf:defsystem #:smackjack
  :serial t
  :description "A small Ajax framework for hunchentoot using parenscript"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:hunchentoot
               #:cl-json
               #:parenscript
               #:cl-containers)
  :components ((:file "package")
               (:file "smackjack")
               (:file "pusher")
               (:module "impl-dependent"
                :components ((:file
                              #+lispworks
                              "lispworks"
                             
                              #+allegro
                              "acl"

                              #+cmucl
                              "cmucl"

                              #+sbcl
                              "sbcl"

                              #+clisp
                              "clisp"

                              #+ecl
                              "ecl"

                              #+gcl
                              "gcl"
                             
                              #+abcl
                              "abcl"

                              #+clozure-common-lisp
                              "clozure-cl"

                              #+ccl
                              "ccl"
                              ))
                :depends-on ("package"))))

