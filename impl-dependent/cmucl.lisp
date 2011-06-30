;;;; -*- Mode: Lisp -*-

;;;; cmucl.lisp --
;;;; SmackJack implementation dependency file.

(in-package :smackjack)

(defun arglist (function-designator)
  (ext:arglist (or (macro-function function-designator)
		   (symbol-function function-designator))))

;;;; end of file -- cmucl.lisp --
