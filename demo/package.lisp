;;;; package.lisp

(defpackage #:smackjack-demo
  (:nicknames :sj-demo)
  (:use #:cl #:hunchentoot #:smackjack #:cl-who #:parenscript)
  (:export :*ajax-processor*))

