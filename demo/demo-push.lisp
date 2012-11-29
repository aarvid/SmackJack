(in-package :smackjack-demo)



(defparameter *ajax-pusher* 
  (make-instance 'smackjack:ajax-pusher :server-uri "/ajax-push"))

(defparameter *push-server* 
  (start (make-instance 'easy-acceptor :address "localhost" :port 8080
                        :access-log-destination nil)))

(reset-session-secret)

(setq *dispatch-table* (list 'dispatch-easy-handlers 
                             (create-ajax-dispatcher *ajax-pusher*)))





(defun-push push-show-text (text) (*ajax-pusher*)
  (let* ((div (chain document (get-element-by-id "pushed-text"))))
              (when div
                (let* ((p (chain document (create-element "p")))
                      (tnode (chain document (create-text-node text))))
                 (chain p (append-child tnode)) 
                 (chain div (append-child p))))))





(define-easy-handler (home :uri "/") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "smackjack push demo")
      (str (generate-prologue *ajax-pusher*)))
     (:body :onload (ps-inline (chain smackpusher (start-poll)))
      (:p (:b "Pushed Text"))
      (:div :id "pushed-text")))))
