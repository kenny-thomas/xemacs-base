;;; easy-mmode.el --- easy definition for major and minor modes

;; Copyright (C) 1997, 2000, 2001 Free Software Foundation, Inc.

;; Author: Georges Brun-Cottan <Georges.Brun-Cottan@inria.fr>
;; Maintainer: Stefan Monnier <monnier@gnu.org>

;; Keywords: extensions lisp

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: GNU Emacs 20.7, partially with 21.2.

;;; Commentary:

;; Minor modes are useful and common.  This package makes defining a
;; minor mode easy, by focusing on the writing of the minor mode
;; functionalities themselves.  Moreover, this package enforces a
;; conventional naming of user interface primitives, making things
;; natural for the minor-mode end-users.

;; For each mode, easy-mmode defines the following:
;; <mode>      : The minor mode predicate. A buffer-local variable.
;; <mode>-map  : The keymap possibly associated to <mode>.
;; <mode>-hook,<mode>-on-hook,<mode>-off-hook and <mode>-mode:
;;       see `define-minor-mode' documentation
;;
;; eval
;;  (pp (macroexpand '(define-minor-mode <your-mode> <doc>)))
;; to check the result before using it.

;; The order in which minor modes are installed is important.  Keymap
;; lookup proceeds down minor-mode-map-alist, and the order there
;; tends to be the reverse of the order in which the modes were
;; installed.  Perhaps there should be a feature to let you specify
;; orderings.

;;; Code:

(eval-when-compile (require 'cl))

(defmacro easy-mmode-define-toggle (mode &optional doc)
  "Define a one arg toggle mode MODE function and associated hooks.
MODE-mode is the so defined function that toggle the mode.
optional DOC is its associated documentation.

Hooks are checked for run, each time MODE-mode is called.
They run under the followings conditions:
MODE-hook: if the mode is toggled.
MODE-on-hook: if the mode is on.
MODE-off-hook: if the mode is off.

When the mode is effectively toggled, two hooks may run.
If so MODE-hook is guaranteed to be the first.

\(defmacro easy-mmode-define-toggle (MODE &optional DOC)"
  (let* ((mode-name
	  (if (string-match "-mode\\'" (symbol-name mode))
	      (symbol-name mode)
	    (concat (symbol-name mode) "-mode")))
	 (hook (intern (concat mode-name "-hook")))
	 (hook-on (intern (concat mode-name "-on-hook")))
	 (hook-off (intern (concat mode-name "-off-hook")))
	 (toggle (intern mode-name))
	 (mode toggle)
	 (toggle-doc (or doc
			 (format "With no argument, toggle %s mode.
With arg turn mode on.
With zero or negative arg turn mode off"
				 mode-name))))
    `(progn
       (defvar ,hook  nil	     
	 ,(format "Hook called when %s mode is toggled" mode-name))

       (defvar ,hook-on  nil	     
	 ,(format "Hook called when %s mode is turned on" mode-name))

       (defvar ,hook-off nil
	 ,(format "Hook called when %s mode is turned off" mode-name))

       (defun ,toggle (&optional arg)
	 ,toggle-doc
	 (interactive "P")
	 (let ((old-mode ,mode))
	   (setq ,mode
		 (if arg
		     (or (listp arg);; C-u alone
			 (> (prefix-numeric-value arg) 0))
		   (not ,mode)))
	   (and ,hook
		(not (equal old-mode ,mode))
	        (run-hooks ',hook))
	   (and ,hook-on
		,mode
		(run-hooks ',hook-on))
	   (and ,hook-off
		(not ,mode)
		(run-hooks ',hook-off)))))))

;;;###autoload
(defalias 'easy-mmode-define-minor-mode 'define-minor-mode)
;;;###autoload
(defmacro define-minor-mode (mode doc &optional init-value lighter keymap)
  "Define a new minor mode MODE.
This function defines the associated control variable MODE, keymap MODE-map,
toggle command MODE, and hook MODE-hook.

DOC is the documentation for the mode toggle command.
Optional INIT-VALUE is the initial value of the mode's variable.
Optional LIGHTER is displayed in the modeline when the mode is on.
Optional KEYMAP is the default (defvar) keymap bound to the mode keymap.
  If it is a list, it is passed to `easy-mmode-define-keymap'
  in order to build a valid keymap.  It's generally better to use
  a separate MODE-map variable than to use this argument.
 
\(defmacro easy-mmode-define-minor-mode
  (MODE DOC &optional INIT-VALUE LIGHTER KEYMAP)...\)" 
  (let* ((mode-name (symbol-name mode))
	 (mode-doc (format "Non-nil if %s mode is enabled." mode-name))
	 (keymap-name (concat mode-name "-map"))
	 (keymap-doc (format "Keymap for %s mode." mode-name)))
    `(progn
       ;; define the switch
       (defvar ,mode ,init-value ,mode-doc)
       (make-variable-buffer-local ',mode)

       ;; define the minor-mode keymap
       (defvar ,(intern keymap-name)
	 (cond ((and ,keymap (keymapp ,keymap))
		,keymap)
	       ((listp ,keymap)
		(easy-mmode-define-keymap ,keymap))
	       (t (error "Invalid keymap %S" ,keymap)))
	 ,keymap-doc)

       ;; define the toggle and the hooks
       ,(macroexpand `(easy-mmode-define-toggle ,mode ,doc)) ; toggle and hooks

       ;; update the mode-bar
       (or (assq ',mode minor-mode-alist)
	   (setq minor-mode-alist
		 (cons (list ',mode ,lighter) minor-mode-alist)))

       ;; update the minor-mode-map
       (or (assq ',mode minor-mode-map-alist)
	   (setq minor-mode-map-alist 
		 (cons (cons ',mode ,(intern keymap-name)) minor-mode-map-alist)))) ))

;;;
;;; easy-mmode-defmap
;;;

(if (fboundp 'set-keymap-parents)
    (defalias 'easy-mmode-set-keymap-parents 'set-keymap-parents)
  (defun easy-mmode-set-keymap-parents (m parents)
    (set-keymap-parent
     m
     (cond
      ((not (consp parents)) parents)
      ((not (cdr parents)) (car parents))
      (t (let ((m (copy-keymap (pop parents))))
	   (easy-mmode-set-keymap-parents m parents)
	   m))))))

;;;###autoload
(defun easy-mmode-define-keymap (bs &optional name m args)
  "Return a keymap built from bindings BS.
BS must be a list of (KEY . BINDING) where
KEY and BINDINGS are suitable for `define-key'.
Optional NAME is passed to `make-sparse-keymap'.
Optional map M can be used to modify an existing map.
ARGS is a list of additional keyword arguments."
  (let (inherit dense suppress)
    (while args
      (let ((key (pop args))
	    (val (pop args)))
	(case key
	 (:name (setq name val))
	 (:dense (setq dense val))
	 (:inherit (setq inherit val))
	 (:group)
	 ;;((eq key :suppress) (setq suppress val))
	 (t (message "Unknown argument %s in defmap" key)))))
    (unless (keymapp m)
      (setq bs (append m bs))
      (setq m (if dense (make-keymap name) (make-sparse-keymap name))))
    (dolist (b bs)
      (let ((keys (car b))
	    (binding (cdr b)))
	(dolist (key (if (consp keys) keys (list keys)))
	  (cond
	   ((symbolp key)
	    (substitute-key-definition key binding m global-map))
	   ((null binding)
	    (unless (keymapp (lookup-key m key)) (define-key m key binding)))
	   ((let ((o (lookup-key m key)))
	      (or (null o) (numberp o) (eq o 'undefined)))
	    (define-key m key binding))))))
    (cond
     ((keymapp inherit) (set-keymap-parent m inherit))
     ((consp inherit) (easy-mmode-set-keymap-parents m inherit)))
    m))

;;;###autoload
(defmacro easy-mmode-defmap (m bs doc &rest args)
  `(defconst ,m
     (easy-mmode-define-keymap ,bs nil (if (boundp ',m) ,m) ,(cons 'list args))
     ,doc))


;;;
;;; easy-mmode-define-navigation
;;;

;; XEmacs change: autoload
;;;###autoload
(defmacro easy-mmode-define-navigation (base re &optional name endfun)
  "Define BASE-next and BASE-prev to navigate in the buffer.
RE determines the places the commands should move point to.
NAME should describe the entities matched by RE.  It is used to build
  the docstrings of the two functions.
BASE-next also tries to make sure that the whole entry is visible by
  searching for its end (by calling ENDFUN if provided or by looking for
  the next entry) and recentering if necessary.
ENDFUN should return the end position (with or without moving point)."
  (let* ((base-name (symbol-name base))
	 (prev-sym (intern (concat base-name "-prev")))
	 (next-sym (intern (concat base-name "-next"))))
    (unless name (setq name (symbol-name base-name)))
    `(progn
       (add-to-list 'debug-ignored-errors
		    ,(concat "^No \\(previous\\|next\\) " (regexp-quote name)))
       (defun ,next-sym (&optional count)
	 ,(format "Go to the next COUNT'th %s." name)
	 (interactive)
	 (unless count (setq count 1))
	 (if (< count 0) (,prev-sym (- count))
	   (if (looking-at ,re) (incf count))
	   (if (not (re-search-forward ,re nil t count))
	       (if (looking-at ,re)
		   (goto-char (or ,(if endfun `(,endfun)) (point-max)))
		 (error ,(format "No next %s" name)))
	     (goto-char (match-beginning 0))
	     (when (and (eq (current-buffer) (window-buffer (selected-window)))
			(interactive-p))
	       (let ((endpt (or (save-excursion
				  ,(if endfun `(,endfun)
				     `(re-search-forward ,re nil t 2)))
				(point-max))))
		 ;; XEmacs change: only 2 args to pos-visible-in-window-p
		 (unless (pos-visible-in-window-p endpt nil)
		   (recenter '(0))))))))
       (defun ,prev-sym (&optional count)
	 ,(format "Go to the previous COUNT'th %s" (or name base-name))
	 (interactive)
	 (unless count (setq count 1))
	 (if (< count 0) (,next-sym (- count))
	   (unless (re-search-backward ,re nil t count)
	     (error ,(format "No previous %s" name))))))))

(provide 'easy-mmode)

;;; easy-mmode.el ends here
