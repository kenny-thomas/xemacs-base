;;; electric.el --- window maker and Command loop for `electric' modes.

;; Copyright (C) 1985, 1986, 1995 Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: extensions

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 19.34.

;;; Commentary:

; zaaaaaaap

;;; Code:

;; This loop is the guts for non-standard modes which retain control
;; until some event occurs.  It is a `do-forever', the only way out is
;; to throw.  It assumes that you have set up the keymap, window, and
;; everything else: all it does is read commands and execute them -
;; providing error messages should one occur (if there is no loop
;; function - which see).  The required argument is a tag which should
;; expect a value of nil if the user decides to punt. The second
;; argument is the prompt to be used: if nil, use "->", if 'noprompt,
;; don't use a prompt, if a string, use that string as prompt, and if
;; a function of no variable, it will be evaluated in every iteration
;; of the loop and its return value, which can be nil, 'noprompt or a
;; string, will be used as prompt.  Given third argument non-nil, it
;; INHIBITS quitting unless the user types C-g at toplevel.  This is
;; so user can do things like C-u C-g and not get thrown out.  Fourth
;; argument, if non-nil, should be a function of two arguments which
;; is called after every command is executed.  The fifth argument, if
;; provided, is the state variable for the function.  If the
;; loop-function gets an error, the loop will abort WITHOUT throwing
;; (moral: use unwind-protect around call to this function for any
;; critical stuff).  The second argument for the loop function is the
;; conditions for any error that occurred or nil if none.

(defun Electric-command-loop (return-tag
			      &optional prompt inhibit-quit
					loop-function loop-state)

  (let (cmd 
        (err nil) 
	(electrified-buffer (current-buffer)) ; XEmacs -
        (prompt-string prompt))
    (while t
      (if (not (or (stringp prompt) (eq prompt nil) (eq prompt 'noprompt)))
          (setq prompt-string (funcall prompt)))
      (if (not (stringp prompt-string))
          (if (eq prompt-string 'noprompt)
              (setq prompt-string nil)
            (setq prompt-string "->")))
      (setq cmd (read-key-sequence prompt-string))
      ;; XEmacs
      (or prefix-arg (setq last-command this-command))
      (setq last-command-event (aref cmd (1- (length cmd)))
	    current-mouse-event
	      (and (or (button-press-event-p last-command-event)
		       (button-release-event-p last-command-event)
		       (misc-user-event-p last-command-event))
		   last-command-event)
	    this-command (if (misc-user-event-p last-command-event)
			     last-command-event
                             (key-binding cmd t))
	    cmd this-command)
      ;; This makes universal-argument-other-key work.
      (setq universal-argument-num-events 0)
      (if (or (prog1 quit-flag (setq quit-flag nil))
	      ;; XEmacs
	      (eq (event-to-character last-input-event) (quit-char)))
	  (progn (setq unread-command-events nil
		       prefix-arg nil)
		 ;; If it wasn't cancelling a prefix character, then quit.
		 (if (or (= (length (this-command-keys)) 1)
			 (not inhibit-quit)) ; safety
		     (progn (ding nil 'quit) ; XEmacs - 
			    (message "Quit")
			    (throw return-tag nil))
		   (setq cmd nil))))
      (setq current-prefix-arg prefix-arg)
      (if cmd
	  (condition-case conditions
	      ;; XEmacs
	      (progn (if (eventp cmd)
			 (progn
			   (let ((b (current-buffer)))
			     (dispatch-event cmd)
			     (if (not (eq b (current-buffer)))
				 (throw return-tag (current-buffer)))))
		       (command-execute cmd))
		     (setq last-command this-command)
		     (if (or (prog1 quit-flag (setq quit-flag nil))
			     ;; XEmacs
			     (eq (event-to-character last-input-event)
				 (quit-char)))
			 (progn (setq unread-command-events nil)
				(if (not inhibit-quit)
				    ;; XEmacs
				    (progn (ding nil 'quit)
					   (message "Quit")
					   (throw return-tag nil))
				  (message "Quit inhibited")
				  (ding)))))
	    (buffer-read-only (if loop-function
				  (setq err conditions)
				(ding)
				(message "Buffer is read-only")
				(sit-for 2)))
	    (beginning-of-buffer (if loop-function
				     (setq err conditions)
				   (ding)
				   (message "Beginning of Buffer")
				   (sit-for 2)))
	    (end-of-buffer (if loop-function
			       (setq err conditions)
			     (ding)
			     (message "End of Buffer")
			     (sit-for 2)))
	    (error (if loop-function
		       (setq err conditions)
		     (ding)
		     (message "Error: %s"
			      (if (eq (car conditions) 'error)
				  (car (cdr conditions))
				(prin1-to-string conditions)))
		     (sit-for 2))))
	(ding))
      (and (not (eq (current-buffer) electrified-buffer)) ; XEmacs -
	   (not (eq (selected-window) (minibuffer-window)))
	   (progn (ding nil 'quit)
		  (message "Leaving electric command loop %s."
			   "because buffer has changed")
		  (sit-for 2)
		  (throw return-tag nil)))
      (if loop-function (funcall loop-function loop-state err))))
  ;; XEmacs - huh?  It should be impossible to ever get here...
  (ding nil 'alarm)
  (throw return-tag nil))

;; This function is like pop-to-buffer, sort of. 
;; The algorithm is
;; If there is a window displaying buffer
;; 	Select it
;; Else if there is only one window
;; 	Split it, selecting the window on the bottom with height being
;; 	the lesser of max-height (if non-nil) and the number of lines in
;;      the buffer to be displayed subject to window-min-height constraint.
;; Else
;; 	Switch to buffer in the current window.
;;
;; Then if max-height is nil, and not all of the lines in the buffer
;; are displayed, grab the whole frame.
;;
;; Returns selected window on buffer positioned at point-min.

(defun Electric-pop-up-window (buffer &optional max-height)
  (let* ((win (or (get-buffer-window buffer) (selected-window)))
	 (buf (get-buffer buffer))
	 (one-window (one-window-p t))
	 (pop-up-windows t)
	 (target-height)
	 (lines))
    (if (not buf)
	(error "Buffer %s does not exist" buffer)
      (save-excursion
	(set-buffer buf)
	(setq lines (count-lines (point-min) (point-max)))
	(setq target-height
	      (min (max (if max-height (min max-height (1+ lines)) (1+ lines))
			window-min-height)
		   (save-window-excursion
		     (delete-other-windows)
		     (1- (window-height (selected-window)))))))
      (cond ((and (eq (window-buffer win) buf))
	     (select-window win))
	    (one-window
	     (goto-char (window-start win))
	     (pop-to-buffer buffer)
	     (setq win (selected-window))
	     (enlarge-window (- target-height (window-height win))))
	    (t
	     (switch-to-buffer buf)))
      (if (and (not max-height)
	       (> target-height (window-height (selected-window))))
	  (progn (goto-char (window-start win))
		 (enlarge-window (- target-height (window-height win)))))
      (goto-char (point-min))
      win)))

(provide 'electric)

;;; electric.el ends here
