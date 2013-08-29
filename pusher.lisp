(in-package :smackjack)

(defclass ajax-pusher (ajax-processor)
  ((poll-namespace     :accessor pusher-poll-namespace
                       :type symbol
                       :initarg :poll-namespace
                       :initform 'smackpusher
                       :documentation "javascript namespace (i.e. object) to hold the polling
code and the defined push functions")
   (poll-function-name :accessor pusher-poll-function-name
                       :type symbol
                       :initarg :poll-function-name
                       :initform 'push-poll-request
                       :documentation "Name of the smackjack function called
on the client side when polling for pushes.  Note: this name is a tag.  The
function does not actually exist on the server.")
   (session-queue-name :accessor pusher-session-queue-name
                       :initarg :session-queue-name
                       :type symbol
                       :initform :smackpush-queue
                       :documentation "pretty sure this is not used.")
   (poll-interval      :accessor pusher-poll-interval
                       :initarg :poll-interval
                       :type (integer 1)
                       :initform 5000
                       :documentation "The number of milliseconds between
polls of the server for push calls.")
   (push-functions     :accessor pusher-push-functions
                       :initform (make-hash-table :test #'equal)
                       :type hash-table
                       :documentation "Maps the symbol names of the remoted
functions to their ajax-function object")
   (pushes-per-poll    :accessor pusher-pushes-per-poll
                       :initform 1
                       :initarg :pushes-per-poll
                       :type (integer 1)
                       :documentation "Maximum number of pushes per poll."))
  (:documentation "The class adds pushing capabilities to ajax-processor.
The server can push function calls to the client. That is the lisp server
code calls a function with arguments that will cause a corresponding
function call on the client side."))

(defmethod initialize-instance :after ((pusher ajax-pusher) &key)
  "add remote polling function name after creating pusher class."
  (remotify-function pusher (pusher-poll-function-name pusher)
                     :method :post :callback-data :json ))

(defmethod (setf pusher-poll-function-name) :before ((new-function-name symbol)
                                                     (pusher ajax-pusher))
  (un-remotify-function pusher (pusher-poll-function-name pusher)))

(defmethod (setf pusher-poll-function-name) :after ((new-function-name symbol)
                                                    (pusher ajax-pusher))
  (remotify-function pusher new-function-name :method :post :callback-data :json))

(defclass push-function (remote-function)
  ((ps-code
    :accessor push-ps-code
    :initarg :ps-code
    :type list
    :documentation "parenscript code to be compiled and sent to the browser
on page load")
   (default-sessions
    :accessor push-default-sessions
    :initarg :default-sessions
    :initform :acceptor-sessions
    :type '(member :current-session :acceptor-sessions
            :acceptor-sessions-except-current)
    :documentation "If a push function is called and the hunchentoot
sessions are not specified, this will determine which in sessions the push call
happens on the client side."))
  (:documentation "This class represents functions that are pushed to the client."))


(defgeneric pushify-function (pusher function-name ps-code
                              &key remote-name default-sessions)
  (:documentation "Function to create a push-function and store it in the
pusher object."))
(defmethod  pushify-function ((pusher ajax-pusher) function-name ps-code
                              &key remote-name
                                   (default-sessions :acceptor-sessions))
  (setf (gethash (symbol-name function-name) (pusher-push-functions pusher))
        (make-instance 'push-function
                       :name function-name
                       :ps-code ps-code
                       :remote-name remote-name
                       :default-sessions default-sessions)))

(defgeneric push-function-sessions (push-function)
  (:documentation "Returns a list of Hunchentoot sessions for a push-function
based on the slot default-sessions"))
(defmethod  push-function-sessions ((push-function push-function))
  (case (push-default-sessions push-function)
    (:current-session (list *session*))
    (:acceptor-sessions
     (mapcar #'cdr (session-db *acceptor*)))
    (:acceptor-sessions-except-current
     (remove *session* (mapcar #'cdr (session-db *acceptor*))))))

(defgeneric push-function (pusher function-name arguments &optional sessions)
  (:documentation "This function is called when you want to push a function
call to the client(s) as specified in the parameter sessions. Note: the
programmer should not call this directly."))
(defmethod  push-function ((pusher ajax-pusher) function-name arguments &optional sessions)
  (when-let* ((pf (gethash (symbol-name function-name)
                           (pusher-push-functions pusher)))
              (sessions (if sessions
                            (ensure-list sessions)
                            (push-function-sessions pf))))
    (let ((data (list (cons 'function function-name)
                      (cons 'arguments arguments))))
      (dolist (s sessions)
        (let ((q (session-value :smackpush-queue s)))
          (unless q
            (setf q (setf (session-value :smackpush-queue)
                          (make-instance 'cl-containers:basic-queue))))
          (cl-containers:enqueue q data))))))

(defmacro defun-push (name lambda-list (processor &rest keys) &body body)
  "declares and defines a 'push' function that when called in lisp causes
   the correponding javascript function to be called in the client page.
   the body should be parenscript code to be compiled into javascript "
  `(progn
     (defun ,name ,(append lambda-list '(&key sessions))
       (push-function ,processor (quote ,name) (list ,@lambda-list) sessions))
     (pushify-function ,processor
                       (quote ,name)
                       (quote ,(cons lambda-list body))
                       ,@keys)))

(defgeneric push-ps-function (pusher push-function)
  (:documentation "returns parenscript code for the push-function
to be defined on the client"))
(defmethod push-ps-function ((pusher ajax-pusher) (push-function push-function))
  (let ((remote-name (ajax-function-name pusher push-function)))
    (list 'progn
          (list* 'defun remote-name
                (push-ps-code push-function))
          (list 'setf
                (list '@ 'push-functions remote-name )
                remote-name))))

(defgeneric ps-push-functions (pusher)
  (:documentation "returns parenscript code to define all the push-functions
on the client"))
(defmethod ps-push-functions ((pusher ajax-pusher))
  (let ((push-fns))
    (maphash-values (lambda (fn)
                     (push (push-ps-function pusher fn) push-fns))
                   (pusher-push-functions pusher))
    push-fns))

(defmethod generate-prologue-javascript ((processor ajax-pusher))
  "returns javascript code for the entire pusher/ajax processor.
Calls the inherited method from ajax-processor and adds in code for the
pusher."
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
                       (dolist (psh response)
                         (let ((func (@ psh function)))
                           (if (aref push-functions func)
                               (chain (aref push-functions func)
                                      (apply nil (@ psh arguments)))
                               (chain console (log (+ "unknown push function: " func)))))))
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

(defun process-push-poll (pusher arguments)
  "this is the default processor of the polling from the client."
  (declare (ignore arguments)) ;; future development.
  (unless *session*
    (start-session))
  (unless (session-value :smackpush-queue)
    (setf (session-value :smackpush-queue)
          (make-instance 'cl-containers:basic-queue)))
  (json:encode-json-to-string
   (let ((q (session-value :smackpush-queue)))
     (loop repeat (pusher-pushes-per-poll pusher)
           until (cl-containers:empty-p q)
           collect (cl-containers:dequeue q)))))

(defmethod call-lisp-function ((processor ajax-pusher)
                               (func ajax-function)
                               arguments)
  "Overriding the default method for ajax-processor. First checks if the
lisp function is the name given for the ajax-pusher's poll processing
function. If so, process push polls. If not, then call the ajax-processor
method. "
  ;;   Note: this is done because we allow the programmer to give the name
  ;; for the poll processing ajax-function but do not allow the programmer
  ;; to define it.  This shortcuts to a direct call to our smackjack's own
  ;; function.
  (if (eq (pusher-poll-function-name processor)
          (name func))
      (process-push-poll processor arguments)
      (call-next-method)))

(defmethod ajax-ps-parameters ((processor ajax-pusher) (ajax-fn ajax-function))
  "Overriding the default method for ajax-processor because that method
would fail on the pusher-poll-function. First checks if the
lisp function is the name given for the ajax-pusher's poll processing
function. If so, returns nil to indicate no parameters. If not,
then call the ajax-processor method."
  ;; see comment in call-lisp-function
  (if (eq (pusher-poll-function-name processor)
          (name ajax-fn))
      nil
      (call-next-method)))


