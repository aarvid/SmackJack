;;;;; Copyright (c) 2010, Martin Loetzsch
;;;;; All rights reserved.

;;;;; Redistribution and use in source and binary forms, with or
;;;;; without modification, are permitted provided that the following
;;;;; conditions are met:

;;;;;  Redistributions of source code must retain the above copyright
;;;;;  notice, this list of conditions and the following disclaimer.

;;;;;  Redistributions in binary form must reproduce the above
;;;;;  copyright notice, this list of conditions and the following
;;;;;  disclaimer in the documentation and/or other materials provided
;;;;;  with the distribution.

;;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;;;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;;;;; THE POSSIBILITY OF SUCH DAMAGE.

;;;;; 
;;;;; This file provides a brief demo of how to use ht-simple-ajax
;;;;;
;;;;; For more information, go to http://martin-loetzsch.de/ht-simple-ajax
;;;;;

;;(asdf:operate 'asdf:load-op :smackjack)

(in-package :smackjack)



;;;;; First we create an ajax processor that will handle our function calls
(defparameter *ajax-processor* 
  (make-instance 'ajax-processor :server-uri "/ajax"))


;;;;; Now we can define a function that we want to call from a web
;;;;; page. This function will take 'name' as an argument and return a
;;;;; string with a greeting.
(defun-ajax say-hi (name) (*ajax-processor*)
  (concatenate 'string "Hi " name ", nice to meet you."))


;;;;; We can call this function from Lisp, for example if we want to
;;;;; test it:
(print (say-hi "Martin"))


;;;;; Next, we setup and start a hunchentoot web server:
(defparameter *my-server* 
  (start (make-instance 'acceptor :address "localhost" :port 8000)))


;;;;; We add our ajax processor to the hunchentoot dispatch table
(setq *dispatch-table* (list 'dispatch-easy-handlers 
                             (create-ajax-dispatcher *ajax-processor*)))


;;;;; Now we can already call the function from a http client:
;;;;; $ curl localhost:8000/ajax/SAY-HI?name=Martin
;;;;; will return
;;;;; <?xml version="1.0"?>
;;;;; <response xmlns='http://www.w3.org/1999/xhtml'>Hi Martin, nice to meet you.</response>
;;;;; Alternatively, you can also paste the url above in a web browser


;;;;; To conveniently call our function from within javascript, the
;;;;; ajax processor can create a html script element with generated
;;;;; javascript functions for each Lisp function:
(print (generate-prologue *ajax-processor*))

;;;;; Together with some helper code, this will also create this:
;;;;;
;;;;; function ajax_say_hi (name, callback) {
;;;;;    ajax_call('SAY-HI', callback, [name]);
;;;;; }
;;;;; 
;;;;; 'name' is the parameter of our Lisp function (if there are
;;;;; multiple parameters, then they will also appear here). Callback
;;;;; is a function that will be asynchronously called when the
;;;;; response comes back from the web server. That function takes 
;;;;; 1 argument, which is the xml DOM object of the response.


;;;;; Finally, we can put everything together and create a page that
;;;;; calls our function.  For rendering html, we will use cl-who in
;;;;; this example (http://weitz.de/cl-who/). Note that smackjack
;;;;; can be used with any other template/ rendering system
(asdf:operate 'asdf:load-op :cl-who)
(use-package :cl-who)

(define-easy-handler (main-page :uri "/") ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html :xmlns "http://www.w3.org/1999/xhtml"
     (:head
      (:title "smackjack demo")
      (princ (generate-prologue *ajax-processor*))
      (:script :type "text/javascript" "
var saveResponse;
// will show the greeting in a message box
function callback(response) {
  saveResponse = response;
  alert(response.firstChild.firstChild.nodeValue);
}

// calls our Lisp function with the value of the text field
function sayHi() {
  smackjack.sayHi(document.getElementById('name').value, callback);
}
"))
     (:body
      (:p "Please enter your name: " 
          (:input :id "name" :type "text"))
      (:p (:a :href "javascript:sayHi()" "Say Hi!"))))))


;;;;; Direct your web browser to http://localhost:8000 and try it out!


