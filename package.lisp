;;;; package.lisp

(defpackage #:smackjack
  (:use #:cl #:hunchentoot #:parenscript #:alexandria)
  (:shadowing-import-from #:parenscript :switch)
  (:export :ajax-processor :ht-simple-ajax-processor
           :defun-ajax
           :create-ajax-dispatcher :generate-prologue))

