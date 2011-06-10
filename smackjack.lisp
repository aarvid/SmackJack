
;;;; smackjack.lisp

(in-package #:smackjack)

;;; "smackjack" goes here. Hacks and glory await!

(defclass ajax-function ()
  ((name :reader name
         :initarg :name
         :type symbol
         :documentation "symbol name of the lisp ajax function")
   (parameters :reader parameters
               :initarg :paramaters
               :type list
               :documentation "parameters of the lisp ajax function")
   )
  )

(defclass ajax-processor ()
  ((ajax-functions 
    :accessor ajax-functions :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "Maps the symbol names of the remoted functions to
                    their ajax-function object")
   (ajax-namespace
    :initarg :ajax-function-prefix :initform 'smackjack
    :accessor ajax-namespace
    :type symbol
    :documentation "Create a namespace object for generated javascript code")
   (ajax-functions-namespace-p
    :initarg :ajax-functions-namespace-p :initform t
    :accessor ajax-functions-namespace-p
    :type boolean
    :documentation "Place javascript functions corresponding
                    to lisp functions in the ajax-namespace")   
   (ajax-function-prefix
    :initarg :ajax-function-prefix :initform nil
    :accessor ajax-function-prefix
    :type symbol
    :documentation "Prefix for javascript functions corresponding
                    to lisp functions")
   
   (ht-simple-ajax-symbols-p  ;; should be removed in the future.
    :initarg :ht-simple-ajax-symbols-p
    :accessor ht-simple-ajax-symbols-p
    :initform nil
    :type boolean
    :documentation "use ht-simple-ajax symbol processor to generate
                    compatible ht-simple-ajax compatible code")
   (server-uri 
    :initarg :server-uri :initform "/ajax" :accessor server-uri
    :type string
    :documentation "The uri which is used to handle ajax request")
   (content-type
    :initarg :content-type :type string
    :accessor content-type :initform "text/xml; charset=\"utf-8\""
    :documentation "The http content type that is sent with each response")
   (reply-external-format 
    :initarg :reply-external-format :type flexi-streams::external-format
    :accessor reply-external-format :initform hunchentoot::+utf-8+
    :documentation "The format for the character output stream")))

(defclass ht-simple-ajax-processor (ajax-processor)
  ((ajax-namespace :initform nil)
   (ajax-functions-namespace-p :initform nil)
   (ajax-function-prefix :initform 'ajax)
   (ht-simple-ajax-symbols-p :initform t)))

(defgeneric create-ajax-dispatcher (processor))
(defmethod create-ajax-dispatcher ((processor ajax-processor))
  "Creates a hunchentoot dispatcher for an ajax processor"
  (create-prefix-dispatcher (server-uri processor)
                            #'(lambda () (call-lisp-function processor))))


(defun make-js-symbol (symbol)
  "helper function for making 'foo_bar_' out of 'foo-bar?' "
  (loop with string = (string-downcase symbol)
     for c across "?-<>"
     do (setf string (substitute #\_ c string))
     finally (return string)))

(defun make-ps-symbol (symbol)
  (symbolicate (string-upcase (make-js-symbol symbol))))



(defgeneric remote-function-via-ajax (processor function-name))
(defmethod  remote-function-via-ajax ((processor ajax-processor)
                                      function-name)
  (setf (gethash (symbol-name function-name) (ajax-functions processor))
        (make-instance 'ajax-function :name function-name)))


(defmacro defun-ajax (name params (processor) &body body)
  "Declares a defun that can be called from a client page.
Example: (defun-ajax func1 (arg1 arg2) (*ajax-processor*)
   (do-stuff))"
  `(progn
     (defun ,name ,params ,@body)
     (remote-function-via-ajax ,processor ',name)))

(defgeneric ajax-function-name (processor name))
(defmethod ajax-function-name ((processor ajax-processor) name)
  (let ((compat (ht-simple-ajax-symbols-p processor))
        (prefix (ajax-function-prefix processor)))
    (funcall (if compat #'make-ps-symbol #'identity)
             (if prefix
                 (symbolicate prefix '- name)
                 name))))

(defgeneric ajax-ps-function (processor name))
(defmethod ajax-ps-function ((processor ajax-processor) name)
  (let* ((namespace (ajax-namespace processor))
         (ajax-fns-in-ns (and namespace (ajax-functions-namespace-p processor)))
         (ajax-name (ajax-function-name processor name))
         (ajax-params  (mapcar (if (ht-simple-ajax-symbols-p processor)
                                   #'make-ps-symbol
                                   #'identity)
                               (arglist name)))
         (ajax-call (if (and namespace (not ajax-fns-in-ns))
                        `(@ ,namespace ajax-call)
                        'ajax-call)))
    `(defun ,ajax-name ,ajax-params
       (,ajax-call ,(string name) callback (array ,@ajax-params)))))

(defgeneric ps-fetch-uri (processor))
(defmethod ps-fetch-uri ((processor ajax-processor))
  (declare (ignore processor))
  '(defun fetch-uri (uri callback)
    (let ((request))
      (if -x-m-l-http-request
          (setf request (new (funcall -x-m-l-http-request)))
          (try
           (setf request (new (-active-x-object "Msxml2.XMLHTTP")))
           (:catch (e)
             (try
              (setf request (new (-active-x-object "Microsoft.XMLHTTP")))
              (:catch (ee)
                (setf request nil))))))
      (unless request
        (alert "Browser couldn't make a request object."))
      (with-slots (open ready-state status response-x-m-l
                        onreadystatechange send) request 
        (funcall open "GET" uri t)
        (setf onreadystatechange
              (lambda ()
                (when (/= 4 ready-state)
                  (return))
                (if (or (and (>= status 200) (< status 300))
                        (== status 304))
                    (unless (== callback null)
                      (callback response-x-m-l))
                    (alert (+ "Error while fetching URI " uri)))
                (return)))
        (funcall send null))
      (delete request)
      (return))))

(defgeneric ps-ajax-call (processor))
(defmethod ps-ajax-call ((processor ajax-processor))  
  `(defun ajax-call (func callback args)
     (let ((uri (+ ,(server-uri processor) "/"
                   (encode-u-r-i-component func) "/")))
       (when (> (length args) 0)
         (incf uri "?")
         (dotimes (i (length args))
           (when (> i 0)
             (incf uri "&"))
           (incf uri (+ "arg" i "=" (encode-u-r-i-component (aref args i))))))
       (fetch-uri uri callback))))


(defgeneric generate-prologue-javascript (processor))
(defmethod generate-prologue-javascript ((processor ajax-processor))
  (let* ((namespace (ajax-namespace processor))
        (ajax-fns-in-ns (and namespace (ajax-functions-namespace-p processor)))
        (ajax-fns nil)
        (ajax-globals nil))
    (maphash-values (lambda (fn)
                      (with-slots (name) fn  
                        (push (ajax-ps-function processor name) ajax-fns)
                        (when ajax-fns-in-ns
                          (let ((ajax-name (ajax-function-name processor name)))
                            (push `(setf (@ ,namespace ,ajax-name) ,ajax-name)
                                  ajax-globals)))))
                    (ajax-functions processor))
    (ps*
     (if namespace
          `(progn
             (var ,namespace (create))
             (funcall
              (lambda ()
                ,(ps-fetch-uri processor)
                ,(ps-ajax-call processor)
                ,(if ajax-fns-in-ns
                   `(progn ,@ajax-fns ,@ajax-globals)
                   `(setf (@ ,namespace ajax-call) ajax-call))
                (return)))
             ,(unless ajax-fns-in-ns
                   `(progn ,@ajax-fns)))
          (list* 'progn
                 (ps-fetch-uri processor)
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
  
    


(defgeneric generate-prologue (processor))
(defmethod generate-prologue ((processor ajax-processor))
  "Creates a <script> ... </script> html element that contains all the
   client-side javascript code for the ajax communication. Include this 
   script in the <head> </head> of each html page"
  (html-script-cdata (generate-prologue-javascript processor)))



(defun call-lisp-function (processor)
  "This is called from hunchentoot on each ajax request. It parses the 
   parameters from the http request, calls the lisp function and returns
   the response."
  (let* ((fn-name (string-trim "/" (subseq (script-name* *request*)
                                           (length (server-uri processor)))))
         (fn (name (gethash fn-name (ajax-functions processor))))
         (args (mapcar #'cdr (get-parameters* *request*))))
    (unless fn
      (error "Error in call-lisp-function: no such function: ~A" fn-name))
    
    (setf (reply-external-format*) (reply-external-format processor))
    (setf (content-type*) (content-type processor))
    (no-cache)
    (concatenate 'string
                 "<?xml version=\"1.0\"?>"
                 (string #\newline)
                 "<response xmlns='http://www.w3.org/1999/xhtml'>"
                 (apply fn args)
                 "</response>")))