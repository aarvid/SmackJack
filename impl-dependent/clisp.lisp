;;;; -*- Mode: Lisp -*-

;;;; clisp.lisp --
;;;; SmackJack implementation dependency file.

;;;; See COPYING file for copyright and licensing information.

(in-package :smackjack)

(defun arglist (function-designator)
  (ext:arglist (or (macro-function function-designator)
		   (symbol-function function-designator))))

;;;; end of file -- clisp.lisp --
