
;;;; smackjack.lisp

(in-package #:smackjack)


(defclass remote-function ()
  ((name :reader name
         :initarg :name
         :type symbol
         :documentation "symbol name of the lisp function")
   (remote-name
    :accessor remote-name
    :initarg :remote-name
    :initform nil
    :type symbol
    :documentation "remote name of the function"))
  (:documentation "Base class for remote functions between server and browser"))

(defclass ajax-function (remote-function)
  ((method
    :accessor http-method
    :initarg :method
    :initform :get
    :type '(member :post :get)
    :documentation "http method of the lisp ajax function")
   (callback-data
    :accessor callback-data
    :initarg :callback-data
    :initform :response-xml
    :type '(member :request-object :response-xml :response-text
            :response-xml-text :json)
    :documentation "argument passed to callback function from ajax response.
                    Content-type should correspond.")
   (content-type
    :initarg :content-type
    :type string
    :accessor content-type
    :initform "text/xml;"
    :documentation "The http content type that is sent with each response"))
  (:documentation "Class for browser functions that call a server-side
lisp function using ajax"))

(defclass ajax-processor ()
  ((ajax-functions 
    :accessor ajax-functions
    :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "Hashtable maps the symbol names of the remoted functions to
                    their ajax-function object")
   (ajax-namespace
    :initarg :ajax-namespace
    :initform 'smackjack
    :accessor ajax-namespace
    :type symbol
    :documentation "Symbol or nil. Name for a namespace object for generated
                    javascript code. Nil means no namespace; code will be
                    global")
   (ajax-functions-namespace-p
    :initarg :ajax-functions-namespace-p
    :initform t
    :accessor ajax-functions-namespace-p
    :type boolean
    :documentation "Boolean. Place javascript functions corresponding
                    to lisp functions in the ajax-namespace. Default t.")   
   (ajax-function-prefixn
    :initarg :ajax-function-prefix
    :initform nil
    :accessor ajax-function-prefix
    :type symbol
    :documentation "Symbol. Prefix for javascript functions corresponding
                    to lisp functions. Default is nil for no prefix.")
   
   (ht-simple-ajax-symbols-p  ;; should be removed in the future.
    :initarg :ht-simple-ajax-symbols-p
    :accessor ht-simple-ajax-symbols-p
    :initform nil
    :type boolean
    :documentation "Boolean. use ht-simple-ajax symbol processor to generate
                    compatible ht-simple-ajax compatible code")
   (json-args-p  ;; should be removed in the future.
    :initarg :json-args-p
    :accessor json-args-p
    :initform t
    :type boolean
    :documentation "Boolean. Ajax function arguments are passed using json")
   (server-uri 
    :initarg :server-uri
    :initform "/ajax"
    :accessor server-uri
    :type string
    :documentation "String. The uri which is used to handle ajax request")
   (default-content-type
    :initarg :default-content-type
    :type string
    :accessor default-content-type
    :initform "text/xml; charset=\"utf-8\""
    :documentation "String. The http content type that is sent with each response")
   (reply-external-format 
    :initarg :reply-external-format
    :type flexi-streams::external-format
    :accessor reply-external-format
    :initform hunchentoot::+utf-8+
    :documentation "The format for the character output stream"))
  (:documentation "The principal and controlling class of this
library. The programmer will instantiate this class, define the ajax
functions of the class and link the dispatcher to hunchentoot's
dispatcher. This processor will also generate javascript code that can
be loaded to the browser."))

(defclass ht-simple-ajax-processor (ajax-processor)
  ((ajax-namespace :initform nil)
   (ajax-functions-namespace-p :initform nil)
   (ajax-function-prefix :initform 'ajax)
   (ht-simple-ajax-symbols-p :initform t)
   (json-args-p :initform nil))
  (:documentation "A sub-class designed to be compatible with ht-simple-ajax"))

(defgeneric create-ajax-dispatcher (processor)
  (:documentation "Creates a hunchentoot dispatcher for an ajax processor.
One possible usage:
  (push (create-ajax-dispatcher your-ajax-processor)
        hunchentoot:*dispatch-table*)"))
(defmethod create-ajax-dispatcher ((processor ajax-processor))
  (create-prefix-dispatcher (server-uri processor)
                            #'(lambda () (process-ajax-request processor))))



(defun make-simple-js-symbol (symbol)
  "this functions generates converts lisps symbols to format used
   by ht-simple-ajax and will be transformed by parenscript so that.
   they are compatibile with ht-simple-ajax.
   helper function for making 'foo_bar_ out of 'foo-bar? "
  (symbolicate
   (string-upcase
    (loop with string = (string-downcase symbol)
     for c across "?-<>"
     do (setf string (substitute #\_ c string))
     finally (return string)))))



(defgeneric remotify-function (processor function-name
                               &key method remote-name content-type callback-data)
  (:documentation "Creates a remote ajax-function identified by its symbol and symbol-name
   and attaches it to the ajax-processor"))
(defmethod  remotify-function ((processor ajax-processor) function-name
                               &key (method :get) remote-name content-type callback-data)
  (setf (gethash (symbol-name function-name) (ajax-functions processor))
        (make-instance 'ajax-function
                       :name function-name
                       :method method
                       :remote-name remote-name
                       :content-type content-type
                       :callback-data callback-data)))

(defgeneric un-remotify-function (processor function-name)
  (:documentation "remove a remote function identified by its symbol or
name from the ajax-processor"))

(defmethod un-remotify-function ((processor ajax-processor) symbol-or-name)
  (let ((func-name (if (symbolp symbol-or-name)
                       (symbol-name symbol-or-name)
                       symbol-or-name)))
    (unless (and func-name (stringp func-name))
      (error "Invalid name ~S in UN-REMOTIFY-FUNCTION" symbol-or-name))
    (remhash (string-upcase func-name) (ajax-functions processor))))


(defmacro defun-ajax (name lambda-list (processor &rest remote-keys) &body body)
  "Macro to defun a server-side function that can be called from a client page.
Example: (defun-ajax func1 (arg1 arg2) (*ajax-processor*)
   (do-stuff))"
  `(progn
     (defun ,name ,lambda-list ,@body)
     (remotify-function ,processor ',name ,@remote-keys)))

(defgeneric ajax-function-name (processor ajax-fn)
  (:documentation "returns a string for the remote function name
before parenscript processing"))
(defmethod ajax-function-name ((processor ajax-processor)
                               (ajax-fn remote-function))
  (let ((compat (ht-simple-ajax-symbols-p processor))
        (prefix (ajax-function-prefix processor))
        (name   (or (remote-name ajax-fn)
                    (name ajax-fn))))
    (funcall (if compat #'make-simple-js-symbol #'identity)
             (if prefix
                 (symbolicate prefix '- name)
                 name))))

(defgeneric ajax-ps-parameters (processor ajax-fn)
  (:documentation "returns a list of the arguments of the ajax-function
ready for parenscript processing."))
(defmethod ajax-ps-parameters ((processor ajax-processor) (ajax-fn ajax-function))
  (mapcar (if (ht-simple-ajax-symbols-p processor)
              #'make-simple-js-symbol
              #'identity)
          (arglist (name ajax-fn))))

(defgeneric ajax-ps-function (processor ajax-fn)
  (:documentation "returns parenscript code to define a function on the
client that will call the corresponding server-side lisp function via ajax."))
(defmethod ajax-ps-function ((processor ajax-processor) (ajax-fn ajax-function))
  (let* ((namespace (ajax-namespace processor))
         (ajax-fns-in-ns (and namespace (ajax-functions-namespace-p processor)))
         (ajax-name (ajax-function-name processor ajax-fn))
         (lisp-name (name ajax-fn))
         (ajax-params (ajax-ps-parameters processor ajax-fn)  )
         (ajax-call (if (and namespace (not ajax-fns-in-ns))
                        `(@ ,namespace ajax-call)
                        'ajax-call)))
    `(defun ,ajax-name ,(append ajax-params '(callback error-handler))
       (,ajax-call ,(string lisp-name)
                   (array ,@ajax-params)
                   ,(string (http-method ajax-fn))
                   callback
                   error-handler
                   ,(ajax-response-process ajax-fn)))))

(defun ps-http-request ()
  "returns parenscript (client) code to set up ajax request."
  '(progn
    (defvar http-factory nil)
    (defvar http-factories
      (array
       (lambda () (new (*x-m-l-http-request)))
       (lambda () (new (*active-x-object "Msxml2.XMLHTTP")))
       (lambda () (new (*active-x-object "Microsoft.XMLHTTP")))))
    (defun http-new-request ()
      (if http-factory
          (http-factory)
          (let ((request nil))
            (do* ((i 0 (1+ i))
                  (l (length http-factories))
                  (factory (aref http-factories i) (aref http-factories i)))
                 ((or (/= request null) (>= i l)))
              (try
               (setf request (factory))
               (unless (= request null)
                 (setf http-factory factory))
               (:catch (e) )))
            (if (= request null)
                (progn
                  (setf http-factory
                        (lambda ()
                          (throw (new (*error "XMLHttpRequest not supported")))))
                  (http-factory))
                request))))))

(defgeneric ps-ajax-response-processes (processor)
  (:documentation "returns client parenscript code to define functions to
handle the various types of ajax responses"))
(defmethod ps-ajax-response-processes ((processor ajax-processor))
  (declare (ignore processor))
  '(progn
    (defun identity (x) x)
    (defun response-xml (request)
      (@ request response-x-m-l))
    (defun response-text (request)
      (@ request response-text))
    (defun response-xml-text (request)
      (let ((result "")
            (n (@ request response-x-m-l first-child)))
        (when n
          (setf n (@ n first-child))
          (when n
            (setf result (@ n node-value))))
        result))
    (defun response-json (request)
      (funcall (@ *json* parse) (@ request response-text)))))

(defgeneric ajax-response-process (ajax-fn)
  (:documentation "returns the symbol of the parenscript function that will
handle the ajax response of the ajax-function"))
(defmethod ajax-response-process ((ajax-fn ajax-function))
  (case (callback-data ajax-fn)
    (:request-object 'identity)
    (:response-xml 'response-xml)
    (:response-text 'response-text)
    (:response-xml-text 'response-xml-text)
    (:json 'response-json)
    (otherwise 'identity)))
       
  
(defgeneric ps-fetch-uri (processor)
  (:documentation "Return parenscript code to define client-side fetch-uri
which initiates ajax request and sets up callback/error treatment."))
(defmethod ps-fetch-uri ((processor ajax-processor))
  (declare (ignore processor))
  '(defun fetch-uri (uri callback &optional (method "GET") (body nil) error-handler process)
    (let ((request (http-new-request)))
      (unless request
        (chain console (log "Browser couldn't make a request object.")))
      (with-slots (open ready-state status status-text response-x-m-l
                   onreadystatechange send set-request-header) request 
        (funcall open method uri t)
        (setf onreadystatechange
              (lambda ()
                (when (equal 4 ready-state)
                  (if (or (and (>= status 200) (< status 300))
                          (equal status 304))
                      (unless (equal callback nil)
                        (callback (process request)))
                      (if (equal error-handler nil)
                          (chain console (log (+ "Error while fetching URI " uri " " status " " status-text)))
                          (error-handler request))))
                nil))
        (when (equal method "POST")
          (funcall set-request-header
                   "Content-Type"
                   "application/x-www-form-urlencoded"))
        (funcall send body))
      nil)))

(defgeneric ps-encode-args (processor)
  (:documentation "returns parenscript code to define a client-side function that
encodes the arguments to be passed along with an ajax/http request."))
(defmethod ps-encode-args ((processor ajax-processor))
  `(defun ajax-encode-args (args)
     (let ((s ""))
       (dotimes (i (length args) s)
         (when (> i 0)
           (incf s "&"))
         (incf s (+ "arg" i "="
                    (encode-u-r-i-component
                     ,(if (json-args-p processor)
                          '(chain *json* (stringify (aref args i)))
                          '(aref args i)))))))))

(defgeneric ps-ajax-call (processor)
  (:documentation "returns parenscript code to define a client-side function
for the ajax-processor that initiates an ajax call of a server-side lisp
function "))
(defmethod ps-ajax-call ((processor ajax-processor))
  `(defun ajax-call (func args &optional (method "GET") callback error-handler process)
     (let ((uri (+ ,(server-uri processor) "/"
                   (encode-u-r-i-component func) "/"))
           (ajax-args (ajax-encode-args args))
           (body nil))
       (when (and (equal method "GET")
                  (> (length args) 0))
         (incf uri (+ "?" ajax-args)))
       (when (equal method "POST")
         (setf body ajax-args))
       (fetch-uri uri callback method body error-handler process))))




(defgeneric generate-prologue-javascript (processor)
  (:documentation "generates a string of raw javascript code that needs to be
loaded on the client side for the ajax-processor to function correctly.
This code can be embedded in a file, a virtual file, or generated html."))
(defmethod generate-prologue-javascript ((processor ajax-processor))
  (let* ((namespace (ajax-namespace processor))
         (ajax-fns-in-ns (and namespace (ajax-functions-namespace-p processor)))
         (ajax-fns nil)
         (ajax-globals nil))
    (maphash-values (lambda (fn)
                      (push (ajax-ps-function processor fn) ajax-fns)
                      (when ajax-fns-in-ns
                        (let ((ajax-name (ajax-function-name processor fn)))
                          (push `(setf (@ ,namespace ,ajax-name) ,ajax-name)
                                ajax-globals))))
                    (ajax-functions processor))
    (ps*
     (if namespace
         `(progn
            (var ,namespace (create))
            (funcall
             (lambda ()
               ,(ps-http-request)
               ,(ps-ajax-response-processes processor)
               ,(ps-fetch-uri processor)
               ,(ps-encode-args processor)
               ,(ps-ajax-call processor)
               ,(if ajax-fns-in-ns
                    `(progn ,@ajax-fns ,@ajax-globals)
                    `(progn
                       (setf (@ ,namespace ajax-call) ajax-call)))
               nil))
            ,(unless ajax-fns-in-ns
               `(progn ,@ajax-fns)))
         (list* 'progn
                (ps-http-request)
                (ps-ajax-response-processes processor)
                (ps-fetch-uri processor)
                (ps-encode-args processor)
                (ps-ajax-call processor)
                ajax-fns)))))



;; in the future possibly generate with a html generator.
;; right now exists to hide ugly html.
(defun html-script-cdata (js &key (newlines t))
  "html script/cdata wrapper for javascript
   wraps javascript in a <script> ... </script> html element"
  (let ((newline (if newlines (string #\newline) "")))
    (concatenate 'string
                 "<script type='text/javascript'>" newline
                 "//<![CDATA[ " newline
                 js newline
                 "//]]>" newline
                 "</script>")))
  
    


(defgeneric generate-prologue (processor &key wrapper)
  (:documentation   "Creates a string that contains all the client-side
javascript code for the ajax communication. Optionally include
<script> ... </script> html element wrapper.  If including the wrapper,
include this script in the <head> </head> of each html page.
Without the wrapper, a virtual file can be done like:
 (define-easy-handler (js-ajax-code :uri \"/ajax-code.js\") ()
   (when (boundp 'hunchentoot:*request*)
     (setf (content-type*) \"text/javascript\"))
  (generate-prologue *ajax-processor* :wrapper nil))"))

(defmethod generate-prologue ((processor ajax-processor) &key (wrapper t))
  (funcall (if wrapper #'html-script-cdata #'identity)
           (generate-prologue-javascript processor)))

(defgeneric get-content-type (processor ajax-fn)
  (:documentation "return the content type string for ajax-function response."))
(defmethod get-content-type ((processor ajax-processor) (ajax-fn ajax-function))
  (case (callback-data ajax-fn)
    ((:response-xml :response-xml-text) "text/xml")
    (:response-text "text/plain")
    (:json "application/json")
    (otherwise (or (content-type ajax-fn)
                   (default-content-type processor)))))

;; function stolen from cl-who
;;; Copyright (c) 2003-2008, Dr. Edmund Weitz. All rights reserved.
(defun escape-string (string)
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (flet ((escape-char-p (char)
           (or (find char "<>&'\"")
               (> (char-code char) 127))))
    (let ((first-pos (position-if #'escape-char-p string))
          (format-string "&#x~x;" ))
      (if (not first-pos)
          ;; nothing to do, just return STRING
          string
          (with-output-to-string (s)
            (loop with len = (length string)
                  for old-pos = 0 then (1+ pos)
                  for pos = first-pos
                    then (position-if #'escape-char-p string :start old-pos)
                  ;; now the characters from OLD-POS to (excluding) POS
                  ;; don't have to be escaped while the next character has to
                  for char = (and pos (char string pos))
                  while pos
                  do (write-sequence string s :start old-pos :end pos)
                     (case char
                       ((#\<)
                        (write-sequence "&lt;" s))
                       ((#\>)
                        (write-sequence "&gt;" s))
                       ((#\&)
                        (write-sequence "&amp;" s))
                       ((#\')
                        (write-sequence "&#039;" s))
                       ((#\")
                        (write-sequence "&quot;" s))
                       (otherwise
                        (format s format-string (char-code char))))
                  while (< (1+ pos) len)
                  finally (unless pos
                            (write-sequence string s :start old-pos))))))))


(defun xml-wrapper (string)
  "wrapper for xml ajax response"
   (concatenate 'string
                 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                 (string #\newline)
                 "<response xmlns='http://www.w3.org/1999/xhtml'>"
                 (escape-string string)
                 "</response>"))

(defgeneric process-ajax-request (processor)
  (:documentation "This should called on each ajax request. That is the
ajax uri is identified and dispatched by hunchentoot.  See
CREATE-AJAX-DISPATCHER. It  parses the parameters from the http request, calls
the lisp function and returns the response."))
(defmethod process-ajax-request ((processor ajax-processor))
  (let* ((fn-name (string-trim "/" (subseq (script-name* *request*)
                                           (length (server-uri processor)))))
         (fn  (gethash fn-name (ajax-functions processor))))
    (unless fn
      (error "Error in process-ajax-request: no such function: ~S ~S"
             fn-name
             (alexandria:hash-table-alist  (ajax-functions processor))))
    (let ((args (mapcar (compose (if (json-args-p processor)
                                     #'json:decode-json-from-string
                                     #'identity)
                                 #'cdr)
                        (funcall (if (eq :post (http-method fn))
                                     #'post-parameters*
                                     #'get-parameters*)
                                 *request*))))
      (unless (= (length (ajax-ps-parameters processor fn)) (length args))
        (error "Error in process-ajax-request: wrong number args: ~A ~A" fn-name args))
      (setf (reply-external-format*) (reply-external-format processor))
      (setf (content-type*) (get-content-type processor fn))
      (no-cache)
      (funcall (if (search "xml" (content-type*))
                   #'xml-wrapper
                   #'identity)
               (call-lisp-function processor fn args)))))

(defgeneric call-lisp-function (processor ajax-function arguments)
  (:documentation "This calls does the actual call of the ajax-lisp function.
Note: this is separate because it is overridden in the class ajax-pusher."))
(defmethod call-lisp-function ((processor ajax-processor)
                               (func ajax-function)
                               arguments)
  (declare (ignore processor))
  (apply (name func) arguments))
