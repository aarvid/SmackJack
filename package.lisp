;;;; package.lisp

(defpackage #:smackjack
  (:documentation "the one and only smackjack package. Contains all code
to run the ajax framework. ")
  (:use #:cl #:hunchentoot #:parenscript #:alexandria)
  (:shadowing-import-from #:parenscript :switch)
  (:export
           ;; smackjack.lisp
           #:ajax-processor
           #:ajax-namespace
           #:ajax-functions-namespace-p
           #:ajax-function-prefix
           #:ht-simple-ajax-symbols-p
           #:json-args-p
           #:server-uri
           #:default-content-type
           #:reply-external-format
           #:ht-simple-ajax-processor
           #:defun-ajax
           #:create-ajax-dispatcher
           #:generate-prologue
           ;; pusher.lisp
           #:ajax-pusher
           #:pusher-poll-namespace
           #:pusher-poll-function-name
           #:pusher-poll-interval
           #:pusher-pushes-per-poll
           #:defun-push))

