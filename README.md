vindinium-starter-cl
====================

Common Lisp starter bot for Vindinium (http://vindinium.org)

# Dependencies

* [Drakma](http://weitz.de/drakma/)
* [jsown](https://github.com/madnificent/jsown)
* [CL-PPCRE](http://weitz.de/cl-ppcre/)

CL-PPCRE is only used for the example board/tile functions, not the client.

# Usage

Example session on the REPL using [Quicklisp](http://www.quicklisp.org/):
```
> (load "~/quicklisp/setup.lisp")
> (ql:quickload 'drakma)
> (ql:quickload 'jsown)
> (ql:quickload 'cl-ppcre)
> (load "vindinium.lisp")
> (in-package :vindinium)
> (setq +secret-key+ "abcdefgh")
> (run-game #'my-bot :mode 'training :turns 30)
> (run-game #'my-bot :mode 'arena)
```
