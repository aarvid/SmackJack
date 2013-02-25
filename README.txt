This is the stub README.txt for the "smackjack" project.

Smackjack is a small Ajax framework written in Common Lisp.  Basically
Smackjack allows browser code call lisp functions via ajax and get a
return value.

Currently runs under Hunchentoot web server but there are plans
to allow other web servers. It was a fork of ht-simple-ajax and
inspires to be a replacement to the unmaintained and out of date
ht-ajax.

Differently than those two libraries, this one depends on parenscript
to generate client-side javascript and allows greater flexibility in
generated javascript.

Current version is compatible with ht-simple-ajax but it lacks many
features of ht-ajax.

The following features are addition to those available in ht-simple-ajax:

  * AJAX via Post as well as Get

  * response can be text, xml or json.

  * optional javascript namespaces to encapsulate generated javascript
    functions.
  
  * The arguments of the functions can be treated by cl-json before
    calling the function.


The code also contains a "pusher" subclass.  This allows a lisp
function call indirectly a javascript function defined in your lisp
code by parenscript but runs in the browser.  Basically the opposite
of SmackJack.  This is achieved by adding a simple polling mechanism.
Future development may include long polling
