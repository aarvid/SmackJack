This is the stub README.txt for the "smackjack" project.

Smackjack is small Ajax framework written in Common Lisp.
Currently runs under Hunchentoot web server but there are plans
to allow other web servers. It is a fork of ht-simple-ajax and
inspires to be a replacement to the unmaintained and out of date
ht-ajax.

Differently than those two libraries, this one depends on parenscript
to generate client-side javascript and allows greater flexibility in
generated javascript.

Current version is compatible with ht-simple-ajax but it lacks many features
of ht-ajax.

The following features are addition to those available in ht-simple-ajax:

*  AJAX via Post as well as Get
*  response can be text, xml or json.
*  optional javascript namespaces to encapsulate generated javascript functions.

