;;;; -*- Mode: Lisp -*-

;;;; lispworks.lisp --
;;;; SmackJack implementation dependency file.

(in-package :smackjack)

(defun arglist (function-designator)
  (lw:function-lambda-list function-designator))

;;;; end of file -- lispworks.lisp --
