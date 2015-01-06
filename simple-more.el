;;; subr-more.el --- Complement the basic editing commands for XEmacs

;; Copyright (C) 1985-1987, 1993-2012 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'field)

;;;###autoload
(defun move-beginning-of-line (arg)
  "Move point to beginning of current line as displayed.
\(If there's an image in the line, this disregards newlines
which are part of the text that the image rests on.)

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
  (interactive "p")
  (or arg (setq arg 1))

  (let ((orig (point))
	first-vis first-vis-field-value)

    ;; Move by lines, if ARG is not 1 (the default).
    (if (/= arg 1)
	(line-move (1- arg) 'noerror))

    ;; Move to beginning-of-line, ignoring fields and invisible text.
    (skip-chars-backward "^\n")
    (while (and (not (bobp)) (invisible-p (1- (point))))
      (goto-char (previous-single-char-property-change (point)))
      (skip-chars-backward "^\n"))

    ;; Now find first visible char in the line
    (while (and (not (eobp)) (invisible-p (point)))
      (goto-char (next-single-char-property-change (point))))
    (setq first-vis (point))

    ;; See if fields would stop us from reaching FIRST-VIS.
    (setq first-vis-field-value
	  (constrain-to-field first-vis orig (/= arg 1) t nil))

    (goto-char (if (/= first-vis-field-value first-vis)
		   ;; If yes, obey them.
		   first-vis-field-value
		 ;; Otherwise, move to START with attention to fields.
		 ;; (It is possible that fields never matter in this case.)
		 (constrain-to-field (point) orig
				     (/= arg 1) t nil)))))


;;;###autoload
(defun move-end-of-line (arg)
  "Move point to end of current line as displayed.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there."
  (interactive "p")
  (or arg (setq arg 1))
  (let (done)
    (while (not done)
      (let ((newpos
	     (save-excursion
	       (let ((goal-column 0))
		 (line-move arg 'noerror)
		 (and
		  ;; With bidi reordering, we may not be at bol,
		  ;; so make sure we are.
		  (skip-chars-backward "^\n")
		  (not (bobp))
		  (progn
		    (while (and (not (bobp)) (invisible-p (1- (point))))
		      (goto-char (previous-single-char-property-change
				  (point) 'invisible)))
		    (backward-char 1)))
		 (point)))))
	(goto-char newpos)
	(if (and (> (point) newpos)
		 (eq (preceding-char) ?\n))
	    (backward-char 1)
	  (if (and (> (point) newpos) (not (eobp))
		   (not (eq (following-char) ?\n)))
	      ;; If we skipped something intangible and now we're not
	      ;; really at eol, keep going.
	      (setq arg 1)
	    (setq done t)))))))

(defun invisible-p (pos-or-prop)
  "Non-nil if the property makes the text invisible.
POS-OR-PROP can be a marker or number, in which case it is taken to be
a position in the current buffer and the value of the `invisible' property
is checked; or it can be some other value, which is then presumed to be the
value of the `invisible' property of the text of interest.
The non-nil value returned can be t for truly invisible text or something
else if the text is replaced by an ellipsis."
  (cond
   ((null pos-or-prop) nil)
   ((or (integerp pos-or-prop)
	(markerp pos-or-prop))
    (get-char-property (point) 'invisible))
   ((eq t buffer-invisibility-spec)
    t)
   (t
    ;; see the C function invisible_p
    (let ((rest buffer-invisibility-spec)
	  (done nil))
      (while (and rest (not done))
	(let ((e (car rest)))
	  (cond
	   ((eq pos-or-prop e)
	    (setq done t))
	   ((and (consp e) (eq pos-or-prop (car e)))
	    (setq done (or (cdr e) t)))))
	(setq rest (cdr rest)))
      done))))

;;;###autoload
(defun start-file-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.

Similar to `start-process', but may invoke a file handler based on
`default-directory'.  See Info node `(elisp)Magic File Names'.

This handler ought to run PROGRAM, perhaps on the local host,
perhaps on a remote host that corresponds to `default-directory'.
In the latter case, the local part of `default-directory' becomes
the working directory of the process.

PROGRAM and PROGRAM-ARGS might be file names."
  (let ((fh (find-file-name-handler default-directory 'start-file-process)))
    (if fh (apply fh 'start-file-process name buffer program program-args)
      (apply 'start-process name buffer program program-args))))


;;; simple-more.el ends here
