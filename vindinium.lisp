;;;; vindinium starter pack
;;;; http://vindinium.org/starters

;;; NOTE: +secret-key+ must be set to your own key (a string) before running
;;;
;;; run-game uses a supplied function to handle server responses.
;;; each loop the supplied function is given the server response as a jsown
;;; object, it expects a return value of a command symbol listed in the
;;; +command-map+ alist
;;;
;;; there are also example functions to parse the board tiles string and
;;; individual tiles

(defpackage #:vindinium (:use :common-lisp))
(in-package :vindinium)

;; make drakma return a string instead of octect list for json
(setq drakma:*text-content-types*
	  (cons '("application" . "json") drakma:*text-content-types*))

(defvar +secret-key+ nil)
(defvar +server-url+ "http://vindinium.org")

(defvar +training-url+ (format nil "~a/api/training" +server-url+))
(defvar +arena-url+ (format nil "~a/api/arena" +server-url+))

;; internal command symbol -> server formatted command
(defvar +command-map+
  '((stay . "Stay")
	(north . "North")
	(south . "South")
	(east . "East")
	(west . "West")))

(defun to-command (name)
  "Convert command symbol to its server parameter."
  (cdr (assoc name +command-map+)))

(defun command-list ()
  "Return list of available command symbols."
  (loop for x in +command-map+ collect (car x)))

(defun do-request (url params)
  "Make a POST request with supplied parameters, return jsown object on success."
  (let* ((response (multiple-value-list (drakma:http-request url :method :post
															 :parameters params)))
		 (body (car response))
		 (status (cadr response)))
	(if (= status 200)
		(jsown:parse body)
		(format t "HTTP ERROR: ~a~%~a" status body))))

(defun make-start-args (turns &optional map)
  "Create POST parameters to start a new game. Map is random if nil supplied."
  (let ((params (list (cons "key" +secret-key+)
					  (cons "turns" (write-to-string turns)))))
	(if map (cons (cons "map" map) params) params)))

(defun view-url (game)
  "Return view/replay url from game object."
  (jsown:val game "viewUrl"))

(defun next-url (game)
  "Return url to send next command from game object."
  (jsown:val game "playUrl"))

(defun start-training (turns map)
  "Start a new training mode session and return game object."
  (let ((game (do-request +training-url+ (make-start-args turns map))))
	(if game (format t "Started training game: ~a~%" (view-url game)))
	game))

(defun start-arena ()
  "Start a new arena mode session and return game object."
  (format t "Waiting for pairing...~%")
  (let ((game (do-request +arena-url+ (make-start-args 300 "m1")))) ; TODO: don't send turns/map
	(if game (format t "Started arena game: ~a~%" (view-url game)))
	game))

(defun make-next-args (command)
  "Create POST parameters for sending next command."
  (list (cons "key" +secret-key+)
		(cons "dir" (to-command command))))

(defun next-turn (game command)
  "Send command choice and return next game object."
  (let ((game (do-request (next-url game) (make-next-args command))))
	(format t ".")
	game))

(defun game-finished? (game)
  "Return true if server says game completed or if there was an error."
  (if (not game) t (jsown:val (jsown:val game "game") "finished")))

(defun run-game (bot-f &key (mode 'training) (turns 300) (map "m1"))
  "Start a new game session and loop until complete. Use bot-f to compute command."
  (if +secret-key+
	  (let ((game (if (eq mode 'arena)
					  (start-arena)
					  (start-training turns map))))
		(loop until (game-finished? game)
		   do (setq game (next-turn game (funcall bot-f game))))
		(format t "~%Finished game")
		game)
	  (format t "No secret key is set")))

;;; board/tiles handling

;; tile string regex -> tile symbol
;; NOTE: these don't identify *who* is associated with the tile, just what it is
(defvar +tile-map+
  '(("  " . floor)
	("##" . wall)
	("\\[\\]" . tavern)
	("@[1234]" . hero)
	("\\$[1234-]" . mine)))

;; (split-seq-n (a b c d) 2) => ((a b) (c d))
(defun split-seq-n (seq n)
  "Split a sequence every n values, drop any remaining."
  (if (>= (length seq) n)
	  (cons (subseq seq 0 n)
			(split-seq-n (subseq seq n) n))))

(defun board-tiles (game)
  "Return board tiles string from game object."
  (jsown:val (jsown:val (jsown:val game "game") "board") "tiles"))

(defun board-size (game)
  "Return board size from game object."
  (jsown:val (jsown:val (jsown:val game "game") "board") "size"))

(defun make-board-map (game)
  "Create a 2d array of the tiles string from game object."
  (let ((size (board-size game))
		(tiles (board-tiles game)))
	(make-array (list size size)
				:initial-contents (split-seq-n (split-seq-n tiles 2) size))))

(defun parse-tile (tile-string)
  "Convert a single tile string to symbol using +tile-map+ definitions."
  (cdar (loop for x in +tile-map+
		   when (cl-ppcre:scan (car x) tile-string)
		   collect x)))

(defun tile-at (board-map x y)
  "From a board map, lookup tile at given coords and return parsed tile symbol."
  (if (array-in-bounds-p board-map y x)
	  (parse-tile (aref board-map y x))))

;; NOTE: x and y swap places
(defun my-location (game)
  "Get your hero's current location."
  (let ((position (jsown:val (jsown:val game "hero") "pos")))
	(list (jsown:val position "y") (jsown:val position "x"))))

;;; bot
;;; simple example bot, just moves randomly each turn

(defun random-move ()
  "Return a random command from +command-map+ keys."
  (let ((cmds (command-list)))
	(nth (random (length cmds)) cmds)))

(defun my-bot (game)
  (if game (random-move)))
