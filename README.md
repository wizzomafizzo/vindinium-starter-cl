vindinium-starter-cl
====================

Common Lisp starter bot for Vindinium (http://vindinium.org)

(load "~/quicklisp/setup.lisp")
(ql:quickload 'drakma)
(ql:quickload 'jsown)
(ql:quickload 'cl-ppcre)
(in-package :vindinium)
(setq +secret-key+ "abdcdefg")
(run-game #'my-bot :mode 'training :turns 30)
(run-game #'my-bot :mode 'arena)
