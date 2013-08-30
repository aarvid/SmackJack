(in-package :smackjack-demo)



;;; create an ajax-pusher object.
(defparameter *ajax-pusher* 
  (make-instance 'smackjack:ajax-pusher :server-uri "/ajax-push"))

;;; start hunchentoot server
(defparameter *push-server* 
  (start (make-instance 'easy-acceptor
                        :name 'push-server
                        :address "localhost"
                        :port 8080
                        :access-log-destination nil)))

(reset-session-secret)

;;; add ajax dispatcher to hunchentoot
(push (create-ajax-dispatcher *ajax-pusher*) *dispatch-table*)




;; define push function
(defun-push push-show-text (text) (*ajax-pusher*)
  (let* ((div (chain document (get-element-by-id "pushed-text"))))
              (when div
                (let* ((p (chain document (create-element "p")))
                      (tnode (chain document (create-text-node text))))
                 (chain p (append-child tnode)) 
                 (chain div (append-child p))))))




;;; define a simple web page 
(define-easy-handler (home :uri "/"
                           :acceptor-names (list 'push-server)) ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "smackjack push demo")
      (str (generate-prologue *ajax-pusher*)))
     (:body :onload (ps-inline (chain smackpusher (start-poll)))
      (:p (:b "Pushed Text"))
      (:div :id "pushed-text")))))

;;; run this code after at least one browser opens the page.
;;; this will push the text to all open pages for the *push-server*.
(let ((hunchentoot:*acceptor* *push-server*))
  (smackjack-demo::push-show-text "Four score and seven years ago"))
