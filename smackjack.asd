;;;; smackjack.asd

(asdf:defsystem #:smackjack
  :serial t
  :depends-on (#:alexandria
               #:hunchentoot
               #:parenscript)
  :components ((:file "package")
               (:file "smackjack")
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

