;;; subr-more.el --- Complement the core basic lisp subroutines for XEmacs

;; Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2012
;;   Free Software Foundation, Inc.

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

;; FIXME: `history-length' should probably be defined and used in the
;; core.

;;;###autoload
(defmacro while-no-input (&rest body)
  "Execute BODY only as long as there's no pending input.
If input arrives, that ends the execution of BODY,
and `while-no-input' returns t.  Quitting makes it return nil.
If BODY finishes, `while-no-input' returns whatever value BODY produced."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input")))
    `(with-local-quit
       (catch ',catch-sym
	 (let ((throw-on-input ',catch-sym))
	   (or (input-pending-p)
	       (progn ,@body)))))))


;;;###autoload
(defvar history-length 30
  "Maximum length of history lists before truncation takes place.
A number means truncate to that length; truncation deletes old
elements, and is done just after inserting a new element.
A value of t means no truncation.

This variable only affects history lists that don't specify their own
maximum lengths.  Setting the `history-length' property of a history
variable overrides this default.")


;;;###autoload
(defun add-to-history (history-var newelt &optional maxelt keep-all)
  "Add NEWELT to the history list stored in the variable HISTORY-VAR.
Return the new history list.
If MAXELT is non-nil, it specifies the maximum length of the history.
Otherwise, the maximum history length is the value of the `history-length'
property on symbol HISTORY-VAR, if set, or the value of the `history-length'
variable.
Remove duplicates of NEWELT if `minibuffer-history-uniquify' is non-nil.
If optional fourth arg KEEP-ALL is non-nil, add NEWELT to history even
if it is empty or a duplicate."
  (unless maxelt
    (setq maxelt (or (get history-var 'history-length)
		     history-length)))
  (let ((history (symbol-value history-var))
	tail)
    (when (and (listp history)
	       (or keep-all
		   (not (stringp newelt))
		   (> (length newelt) 0))
	       (or keep-all
		   (not (equal (car history) newelt))))
      (if minibuffer-history-uniquify
	  (delete newelt history))
      (setq history (cons newelt history))
      (when (integerp maxelt)
	(if (= 0 maxelt)
	    (setq history nil)
	  (setq tail (nthcdr (1- maxelt) history))
	  (when (consp tail)
	    (setcdr tail nil)))))
    (set history-var history)))

;;;###autoload
(defun string-match-p (regexp string &optional start buffer)
  "\
Same as `string-match' except this function does not change the match data."
  (let* ((md (match-data))
	 (res (string-match regexp string start buffer)))
    (store-match-data md)
    res))
    
;;; subr-more.el ends here
