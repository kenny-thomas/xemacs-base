;;; passwd.el --- Prompting for passwords semi-securely

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Maintainer: XEmacs Development Team
;; Keywords: comm, extensions

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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not in FSF

;; Commentary:

;;; Change Log:

;;  Sun Jun 12 04:19:30 1994 by sandy on ibm550.sissa.it
;;    Added support for password histories and (provide 'passwd)
;;    (jwz says: this "history" thing is completely undocumented, you loser!)
;; 2-Jan-95 (mon); 4:13 AM by jwz@jwz.org
;;    Fixed Sandy's extreme keymap bogosity.  Made it invert the screen when
;;    reading securely (this could be better; maybe use red text or something
;;    instead...)
;; 9-Jul-95 (fri); 4:55 AM by jwz@jwz.org
;;    Made it work with XEmacs 19.12.
;; 7-Jul-95 by cthomp@cs.uiuc.edu
;;    Added variable to control inverting frame when keyboard grabbed

;;; Code:

(defgroup passwd nil
  "Prompting for passwords semi-securely"
  :group 'processes)


(defcustom passwd-invert-frame-when-keyboard-grabbed (not (featurep 'infodock))
  "*If non-nil swap the foreground and background colors of all faces.
This is done while the keyboard is grabbed in order to give a visual
clue that a grab is in effect."
  :type 'boolean
  :group 'passwd)

(defcustom passwd-echo ?.
  "*The character which should be echoed when typing a password,
or nil, meaning echo nothing."
  :type 'sexp
  :group 'passwd)

(defvar read-passwd-map
  (let ((i 0)
	(s (make-string 1 0))
	map)
    (cond ((fboundp 'set-keymap-parent)
	   (setq map (make-keymap))
	   (set-keymap-parent map minibuffer-local-map))
	  (t  ; v18/FSFmacs compatibility
	   (setq map (copy-keymap minibuffer-local-map))))
    (if (fboundp 'set-keymap-name)
	(set-keymap-name map 'read-passwd-map))

    (while (< i 127)
      (aset s 0 i)
      (or (and (boundp 'meta-prefix-char) (eq (int-char i) meta-prefix-char))
	  (define-key map s 'self-insert-command))
      (setq i (1+ i)))

    (define-key map "\C-g" 'keyboard-quit)
    (define-key map "\C-h" 'delete-backward-char)
    (define-key map "\r" 'exit-minibuffer)
    (define-key map "\n" 'exit-minibuffer)
    (define-key map "\C-u" 'passwd-erase-buffer)
    (define-key map "\C-q" 'quoted-insert)
    (define-key map "\177" 'delete-backward-char)
    (define-key map "\M-n" 'passwd-next-history-element)
    (define-key map "\M-p" 'passwd-previous-history-element)
    map)
  "Keymap used for reading passwords in the minibuffer.
The \"bindings\" in this map are not real commands; only a limited
number of commands are understood.  The important bindings are:
\\<read-passwd-map>
	\\[passwd-erase-buffer]	Erase all input.
	\\[quoted-insert]	Insert the next character literally.
	\\[delete-backward-char]	Delete the previous character.
	\\[exit-minibuffer]	Accept what you have typed.
	\\[keyboard-quit]	Abort the command.

All other characters insert themselves (but do not echo.)")

(define-function-when-void 'clear-string
    (if (not (featurep 'mule))
        #'(lambda (string)
            "Fill STRING with ?\\x00 characters.  Return nil."
            (prog1 nil (fillarray string ?\x00)))
      #'(lambda (string)
          "Fill STRING with ?\\x00 characters.  Return nil.            

This differs from `fill' with a ?\\x00 argument in that it attempts to ensure
that STRING's existing contents are discarded, even in the event of
reallocation due to a change in the byte length of STRING.

This Lisp implementation achieves this by replacing each character with a
character of the same octet length, chosen at (pseudo-)random, and only then
filling STRING with ?\\x00 characters. The information about the individual
octet lengths of the characters can still be leaked to anyone motivated who
has access to a core dump, but this is still a big improvement on having the
entire string around in memory.

In this implementation, the Lisp-visible character length of STRING is not
changed."
          (prog1
              nil
            (save-match-data
              (let ((start 0) (last 0) charset charset-id
                    (string-char-byte-conversion-info
                     (and (fboundp 'string-char-byte-conversion-info)
                          (string-char-byte-conversion-info string))))
                (while (setq start (string-match "[^\x00-\x7f]" string start))
                  ;; Fill the ASCII area with pseudorandom ASCII values.
                  (loop for fixnum from last below start
                        do (aset string fixnum (int-char (random #x80))))
                  (setq charset (char-charset (aref string start))
                        charset-id (charset-id charset))
                  ;; Replace each non-ASCII character with pseudorandom values
                  ;; that have the same octet length. All the following
                  ;; information on the association between a charset's ID and
                  ;; its octet length is from charset.h on 21.5 and
                  ;; mule-charset.h on 21.4.
                  (cond
                    ((<= #x80 charset-id #x8f)
                     ;; "Dimension-1 official" charsets, two octets. Stick
                     ;; with control-1 and latin-1 for filling, so it looks
                     ;; more like data from a no-conversion coding system.
                     (aset string start (int-char (+ #x80 (random #x80)))))
                    ;; This range includes #x9e and #x9f which will never be
                    ;; returned by #'charset-id.
                    ((or (<= #x90 charset-id #xc0)
                         ;; MAX_LEADING_BYTE_PRIVATE_1 changed in the course
                         ;; of 21.5, charset ids in this range can reflect
                         ;; either three octets or four.
                         (and (<= #xc1 charset-id #xef)
                              (eql 1 (charset-dimension charset))))
                     ;; "Dimension-2 official" charsets, three octets. Also
                     ;; "dimension 1 private" charsets, three octets. Choose
                     ;; pseudorandom from among a list of seven predictable 94
                     ;; x 94 official charsets.
                     (symbol-macrolet
                         ((three-octet-charsets
                           [japanese-jisx0208-1978 chinese-gb2312
                            japanese-jisx0208 korean-ksc5601 japanese-jisx0212
                            chinese-cns11643-1 chinese-cns11643-2]))
                       (aset string start
                             (make-char
                              (aref three-octet-charsets
                                    (random (length three-octet-charsets)))
                              (+ 33 (random 94))
                              (+ 33 (random 94))))))
                    ((and (>= charset-id #xc1)
                          (eql 2 (charset-dimension charset)))
                     ;; "Dimension-2 private characters", four octets in
                     ;; length.
                     (symbol-macrolet
                         ((four-octet-charsets
                           [ethiopic chinese-cns11643-3 chinese-cns11643-4
                            chinese-cns11643-5 chinese-cns11643-6
                            chinese-cns11643-7]))
                       (aset string start
                             (make-char
                              (aref four-octet-charsets
                                    (random (length four-octet-charsets)))
                              (+ 33 (random 94))
                              (+ 33 (random 94))))))
                    (t
                     (error 'invalid-argument
                            "Not known how long this character is"
                            (prog1
                                (aref string start)
                              (fillarray string ?\x00)))))
                  (setq start (1+ start)
                        last start))
                (when (> last 0)
                  (loop for fixnum from last below (length string)
                        do (aset string fixnum (int-char (random #x80))))
                  (unless (or (not string-char-byte-conversion-info)
                              (equal string-char-byte-conversion-info
                                     (string-char-byte-conversion-info string)))
                    (fillarray string ?\x00)
                    (error 'internal-error
                           "String octet length altered unintentionally")))
                (fillarray string ?\x00)))))))

;;; internal variables

(defvar passwd-history nil)
(defvar passwd-history-posn 0)

;;;###autoload
(defun read-passwd (prompt &optional confirm default)
  "Prompts for a password in the minibuffer, and returns it as a string.
If PROMPT may be a prompt string or an alist of elements 
'\(prompt . default\).
If optional arg CONFIRM is true, then ask the user to type the password
again to confirm that they typed it correctly.
If optional arg DEFAULT is provided, then it is a string to insert as
the default choice (it is not, of course, displayed.)

If running under X, the keyboard will be grabbed (with XGrabKeyboard())
to reduce the possibility that eavesdropping is occuring.

When reading a password, all keys self-insert, except for:
\\<read-passwd-map>
	\\[read-passwd-erase-line]	Erase the entire line.
	\\[quoted-insert]	Insert the next character literally.
	\\[delete-backward-char]	Delete the previous character.
	\\[exit-minibuffer]	Accept what you have typed.
	\\[keyboard-quit]	Abort the command.

The returned value is always a newly-created string.  No additional copies
of the password remain after this function has returned.

NOTE: unless great care is taken, the typed password will exist in plaintext
form in the running image for an arbitrarily long time.  Priveleged users may
be able to extract it from memory.  If emacs crashes, it may appear in the
resultant core file.

Some steps you can take to prevent the password from being copied around:

 - as soon as you are done with the returned string, destroy it with
   (clear-string string).  The same goes for any default passwords
   or password histories.

 - do not copy the string, as with concat or substring - if you do, be
   sure to keep track of and destroy all copies.

 - do not insert the password into a buffer - if you do, be sure to 
   overwrite the buffer text before killing it, as with the functions 
   `passwd-erase-buffer' or `passwd-kill-buffer'.  Note that deleting
   the text from the buffer does NOT necessarily remove the text from
   memory.

 - be careful of the undo history - if you insert the password into a 
   buffer which has undo recording turned on, the password will be 
   copied onto the undo list, and thus recoverable.

 - do not pass it as an argument to a shell command - anyone will be
   able to see it if they run `ps' at the right time.

"
  (save-excursion
    (let ((input (get-buffer-create " *password*"))
	  (passwd-history-posn 0)
	  (read-passwd nil)
	  passwd-history)
      (if (listp prompt)
	  (setq passwd-history prompt
		default (cdr (car passwd-history))))
      (if (should-use-dialog-box-p)
	  (condition-case ()
	      (popup-dialog-box (list 'built-in 'password
				      default
				      (lambda (pass)
					(setq read-passwd pass))
				      :prompt (or (car-safe prompt) prompt)
				      :verify confirm))
	    (error nil)))
      (if read-passwd
	  read-passwd
	(set-buffer input)
	(buffer-disable-undo input)
	(use-local-map read-passwd-map)
	(unwind-protect
	    (progn
	      (if (passwd-grab-keyboard)
		  (passwd-secure-display))
	      (read-passwd-1 input prompt nil default)
	      (set-buffer input)

	      (if (not confirm)
		  (buffer-string)
		(let ((ok nil)
		      passwd)
		  (while (not ok)
		    (set-buffer input)
		    (setq passwd (buffer-string))
		    (read-passwd-1 input prompt "[Retype to confirm]")
		    (if (passwd-compare-string-to-buffer passwd input)
			(setq ok t)
		      (clear-string passwd)
		      (setq passwd nil)
		      (beep)
		      (read-passwd-1 input prompt "[Mismatch. Start over]")
		      ))
		  passwd)))
	  ;; protected
	  (passwd-ungrab-keyboard)
	  (passwd-insecure-display)
	  (passwd-kill-buffer input)
	  (message "")
          (sit-for 0)
	  )))))


(defun read-passwd-1 (buffer prompt &optional prompt2 default)
  (set-buffer buffer)
  (passwd-erase-buffer)
  (if default (insert default))
  (catch 'exit ; exit-minibuffer throws here
    (while t
      (set-buffer buffer)
      (let* ((minibuffer-completion-table nil)
	     (cursor-in-echo-area t)
	     (echo-keystrokes 0)
	     (inhibit-input-event-recording t)
	     (key (passwd-read-key-sequence
		   (concat (if (listp prompt)
			       (car (nth passwd-history-posn passwd-history))
			     prompt)
			   prompt2
			   (if passwd-echo
			       (make-string (buffer-size) passwd-echo)))))
	     (binding (key-binding key)))
	(setq prompt2 nil)
	(set-buffer buffer)		; just in case...
	(if (fboundp 'event-to-character) ;; lemacs
	    (setq last-command-event (aref key (1- (length key)))
		  last-command-char (event-to-character last-command-event))
	  ;; v18/FSFmacs compatibility
	  (setq last-command-char (aref key (1- (length key)))))
	(setq this-command binding)
	(condition-case c
	    (command-execute binding)
	  (error
	   (beep)
	   (if (fboundp 'display-error)
	       (display-error c t)
	     ;; v18/FSFmacs compatibility
	     (message (concat (or (get (car-safe c) 'error-message) "???")
			      (if (cdr-safe c) ": ")
			      (mapconcat 
			       (function (lambda (x) (format "%s" x)))
			       (cdr-safe c) ", "))))
	   (sit-for 2)))
	))))

(defun passwd-previous-history-element (n)
  (interactive "p")
  (or passwd-history
      (error "Password history is empty."))
  (let ((l (length passwd-history)))
    (setq passwd-history-posn
	  (% (+ n passwd-history-posn) l))
    (if (< passwd-history-posn 0)
	(setq passwd-history-posn (+ passwd-history-posn l))))
  (let ((obuff (current-buffer))) ; want to move point in passwd buffer
    (unwind-protect
	(progn
	  (set-buffer " *password*")
	  (passwd-erase-buffer)
	  (insert (cdr (nth passwd-history-posn passwd-history))))
      (set-buffer obuff))))

(defun passwd-next-history-element (n)
  (interactive "p")
  (passwd-previous-history-element (- n)))

(defun passwd-erase-buffer ()
  ;; First erase the buffer, which will simply enlarge the gap.
  ;; Then insert null characters until the gap is filled with them
  ;; to prevent the old text from being visible in core files or kmem.
  ;; (Actually use 3x the size of the buffer just to be safe - a longer
  ;; passwd might have been typed and backspaced over.)
  (interactive)
  (widen)
  (let ((s (* (buffer-size) 3)))
    (erase-buffer)
    (while (> s 0)
      (insert ?\000)
      (setq s (1- s)))
    (erase-buffer)))

(defun passwd-kill-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (buffer-disable-undo buffer)
    (passwd-erase-buffer)
    (set-buffer-modified-p nil))
  (kill-buffer buffer))


(defun passwd-compare-string-to-buffer (string buffer)
  ;; same as (equal string (buffer-string)) but with no dangerous consing.
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((L (length string))
	  (i 0))
      (if (/= L (- (point-max) (point-min)))
	  nil
	(while (not (eobp))
	  (if (/= (following-char) (aref string i))
	      (goto-char (point-max))
	    (setq i (1+ i))
	    (forward-char)))
	(= (point) (+ i (point-min)))))))


(defvar passwd-face-data nil)
(defun passwd-secure-display ()
  ;; Inverts the screen - used to indicate secure input, like xterm.
  (when passwd-invert-frame-when-keyboard-grabbed
    (setq passwd-face-data
	  (delq nil (mapcar
		     (lambda (face)
		       (let ((fg (face-foreground-instance 
				  face (selected-frame) nil 
				  'no-fallback))
			     (bg (face-background-instance
				  face (selected-frame) nil
				  'no-fallback)))
			 (if (and fg bg)
			     (list face fg bg)
			   nil)))
		     (face-list))))
    (let ((rest passwd-face-data))
      (while rest
	(set-face-foreground (nth 0 (car rest)) (nth 2 (car rest)) (selected-frame))
	(set-face-background (nth 0 (car rest)) (nth 1 (car rest)) (selected-frame))
	(setq rest (cdr rest))))))

(defun passwd-insecure-display ()
  ;; Undoes the effect of `passwd-secure-display'.
  (when passwd-invert-frame-when-keyboard-grabbed
    (while passwd-face-data
      (remove-face-property (nth 0 (car passwd-face-data))
			    'foreground (selected-frame))
      (remove-face-property (nth 0 (car passwd-face-data))
			    'background (selected-frame))
      (setq passwd-face-data (cdr passwd-face-data)))))

(defun passwd-grab-keyboard ()
  ;; It is officially time to give up on lemacs 19.10
  ;; and just deal with device types.
  (let ((lock-func (case (frame-type)
		     (x
		      'x-grab-keyboard)
		     (gtk
		      'gtk-grab-keyboard)
		     (otherwise
		      nil))))
    (if (not lock-func)
	;; There is nothing we can do...
	nil
      (if (funcall lock-func)
	  ;; Grabbed it, hooray!
	  t
	(message "Unable to grab keyboard - waiting a second...")
	(sleep-for 1)
	(if (funcall lock-func)
	    (progn
	      (message "Keyboard grabbed on second try.")
	      t)
	  (beep)
	  (message "WARNING: keyboard is insecure (unable to grab!)")
	  (sleep-for 3)
	  nil)))))

(defun passwd-ungrab-keyboard ()
  (case (frame-type)
    (x (x-ungrab-keyboard))
    (gtk (gtk-ungrab-keyboard))
    (otherwise nil)))

;; v18 compatibility
(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo 'buffer-flush-undo))

;; read-key-sequence echoes the key sequence in Emacs 18.
(defun passwd-read-key-sequence (prompt)
  (let ((inhibit-quit t)
	str)
    (while (or (null str) (keymapp (key-binding str)))
      (message prompt)
      (setq str (concat str (char-to-string (read-char)))))
    (setq quit-flag nil)
    str))

(or (string-match "^18" emacs-version)
    (fset 'passwd-read-key-sequence 'read-key-sequence))

(provide 'passwd)

;;; passwd.el ends here
