;;;; -*- Mode: Lisp -*-

;;;; sbcl.lisp --
;;;; SmackJack implementation dependency file.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package :smackjack)

(require :sb-introspect)

(defun arglist (function-designator)
  (sb-introspect:function-lambda-list function-designator))

;;;; end of file -- sbcl.lisp --
