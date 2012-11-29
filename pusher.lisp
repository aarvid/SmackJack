(in-package :smackjack)

(defclass ajax-pusher (ajax-processor)
  ((poll-namespace     :accessor pusher-poll-namespace
                       :type symbol
                       :initarg :poll-namespace
                       :initform 'smackpusher)
   (poll-function-name :accessor pusher-poll-function-name
                       :type symbol
                       :initarg :poll-function-name
                       :initform 'push-poll-request)
   (session-queue-name :accessor pusher-session-queue-name
                       :initarg :session-queue-name
                       :type symbol
                       :initform :smackpush-queue)
   (poll-interval      :accessor pusher-poll-interval
                       :initarg :poll-interval
                       :type (integer 1)
                       :initform 5000)
   (push-functions     :accessor pusher-push-functions
                       :initform (make-hash-table :test #'equal)
                       :type hash-table
                       :documentation "Maps the symbol names of the remoted functions to
                                       their ajax-function object")))

(defmethod initialize-instance :after ((pusher ajax-pusher) &key)
  (remotify-function pusher (pusher-poll-function-name pusher) :method :post :callback-data :json ))

(defmethod (setf pusher-poll-function-name) :before ((new-function-name symbol) (pusher ajax-pusher))
  (un-remotify-function pusher (pusher-poll-function-name pusher)))

(defmethod (setf pusher-poll-function-name) :after ((new-function-name symbol) (pusher ajax-pusher))
  (remotify-function pusher new-function-name :method :post :callback-data :json))

(defclass push-function (remote-function)
  ((ps-code
    :accessor push-ps-code
    :initarg :ps-code
    :type list
    :documentation "parenscript code to be compiled and sent to the browser on page load")
   (default-sessions
    :accessor push-default-sessions
    :initarg :default-sessions
    :initform :acceptor-sessions
    :type '(member :current-session :acceptor-sessions :acceptor-sessions-except-current))) )


(defgeneric pushify-function (pusher function-name ps-code
                              &key remote-name))
(defmethod  pushify-function ((pusher ajax-pusher) function-name ps-code
                              &key remote-name)
  (setf (gethash (symbol-name function-name) (pusher-push-functions pusher))
        (make-instance 'push-function
                       :name function-name
                       :ps-code ps-code
                       :remote-name remote-name)))

(defgeneric push-function-sessions (push-function))
(defmethod  push-function-sessions ((push-function push-function))
  (case (push-default-sessions push-function)
    (:current-session (list *session*))
    (:acceptor-sessions
     (mapcar #'cdr (session-db *acceptor*)))
    (:acceptor-sessions-except-current
     (remove *session* (mapcar #'cdr (session-db *acceptor*))))))

(defgeneric push-function (pusher function-name arguments &optional sessions))
(defmethod  push-function ((pusher ajax-pusher) function-name arguments &optional sessions)
  (when-let* ((pf (gethash (symbol-name function-name)
                           (pusher-push-functions pusher)))
              (sessions (if sessions
                            (ensure-list sessions)
                            (push-function-sessions pf))))
    (let ((data (list (cons 'function function-name)
                      (cons 'arguments arguments))))
      (dolist (s sessions)
        (when-let ((q (session-value :smackpush-queue s)))
          (cl-containers:enqueue q data))))))

(defmacro defun-push (name lambda-list (processor &rest keys) &body body)
  "declares and defines a functions that when called in lisp causes
   the correponding javascript function to be called in the client page.
   the body should be parenscript code to be compiled into javascript "
  `(progn
     (defun ,name ,(append lambda-list '(&key sessions))
       (push-function ,processor (quote ,name) (list ,@lambda-list) sessions))
     (pushify-function ,processor
                       (quote ,name)
                       (quote ,(cons lambda-list body))
                       ,@keys)))

(defgeneric push-ps-function (pusher push-function))
(defmethod push-ps-function ((pusher ajax-pusher) (push-function push-function))
  (let ((remote-name (ajax-function-name pusher push-function)))
    (list 'progn
          (list* 'defun remote-name
                (push-ps-code push-function))
          (list 'setf
                (list '@ 'push-functions remote-name )
                remote-name))))

(defgeneric ps-push-functions (pusher))
(defmethod ps-push-functions ((pusher ajax-pusher))
  (let ((push-fns))
    (maphash-values (lambda (fn)
                     (push (push-ps-function pusher fn) push-fns))
                   (pusher-push-functions pusher))
    push-fns))

(defmethod generate-prologue-javascript ((processor ajax-pusher))
  (if (= 0 (hash-table-count (pusher-push-functions processor)))
      (call-next-method)
      (concatenate
       'string
       (call-next-method)
       (string #\Newline)
       (with-slots (poll-namespace poll-function-name poll-interval
                    push-functions ajax-namespace)
           processor
         (let ((poll-remote-name poll-function-name
                                 #|(ajax-function-name processor
                                 (gethash (symbol-name poll-function-name)
                                 (ajax-functions processor)))|#))
       (ps* `(defvar ,poll-namespace
               ((lambda ()
                  (let* ((interval-id nil)
                         (interval-time ,poll-interval)
                         (push-functions (create)))
                    ,@(ps-push-functions processor)
                    (defun poll-callback (response)
                      (when response
                        (let ((func (@ response function)))
                          (if (aref push-functions func)
                              (chain (aref push-functions func)
                                     (apply nil (@ response arguments)))
                              (chain console (log (+ "unknown push function: " func))))))
                      nil)
                    (defun interval-interrupt ()
                      (chain ,ajax-namespace (,poll-remote-name poll-callback )))
                    (defun start-poll ()
                      (setf interval-id
                            (set-interval interval-interrupt interval-time)))
                    (defun stop-poll ()
                      (when interval-id
                        (clear-interval interval-id)
                        (setf interval-id nil)))
                    (create start-poll start-poll
                            stop-poll stop-poll)))))))))))

(defun process-push-poll (arguments)
  (declare (ignore arguments)) ;; future development.
  (unless *session*
    (start-session)
    (setf (session-value :smackpush-queue)
          (make-instance 'cl-containers:basic-queue)))
  (json:encode-json-to-string
   (let ((q (session-value :smackpush-queue)))
     (if (cl-containers:empty-p q)
         nil
         (cl-containers:dequeue q)))))

(defmethod call-lisp-function ((processor ajax-pusher)
                               (func ajax-function)
                               arguments)
  (if (eq (pusher-poll-function-name processor)
          (name func))
      (process-push-poll arguments)
      (call-next-method)))

(defmethod ajax-ps-parameters ((processor ajax-pusher) (ajax-fn ajax-function))
  (if (eq (pusher-poll-function-name processor)
          (name ajax-fn))
      nil
      (call-next-method)))
