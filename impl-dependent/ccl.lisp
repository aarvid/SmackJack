;;;; -*- Mode: Lisp -*-

;;;; ccl.lisp --
;;;; SmackJack implementation dependency file.

(in-package :smackjack)

(defun arglist (function-designator)
  (ccl:arglist (or (macro-function function-designator)
		   (symbol-function function-designator))))

;;;; end of file -- ccl.lisp --
