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

;;;###autoload
(defun number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is (+ FROM (* N INC)) where N counts from
zero.  TO is only included if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return (FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list (0.4),
whereas (number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as (+ FROM (* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative)."
  (if (or (not to) (= from to))
      (list from)
    (or inc (setq inc 1))
    (when (zerop inc) (error "The increment can not be zero"))
    (let (seq (n 0) (next from))
      (if (> inc 0)
	  (while (<= next to)
	    (setq seq (cons next seq)
		  n (1+ n)
		  next (+ from (* n inc))))
	(while (>= next to)
	  (setq seq (cons next seq)
		n (1+ n)
		next (+ from (* n inc)))))
      (nreverse seq))))

;;;###autoload
(defun string-or-null-p (object)
  "Return t if OBJECT is a string or nil.
Otherwise, return nil."
  (or (stringp object) (null object)))

;;;###autoload
(defun booleanp (object)
  "Return t if OBJECT is one of the two canonical boolean values: t or nil.
Otherwise, return nil."
  (or (null object) (eq object t)))

;;;###autoload
(defmacro condition-case-unless-debug (var bodyform &rest handlers)
  "Like `condition-case' except that it does not catch anything when debugging.
More specifically if `debug-on-error' is set, then it does not catch any signal."
  (declare (debug condition-case) (indent 2))
  (let ((bodysym (make-symbol "body")))
    `(let ((,bodysym (lambda () ,bodyform)))
       (if debug-on-error
           (funcall ,bodysym)
         (condition-case ,var
             (funcall ,bodysym)
           ,@handlers)))))

;;;###autoload
(defmacro with-demoted-errors (&rest body)
  "Run BODY and demote any errors to simple messages.
If `debug-on-error' is non-nil, run BODY without catching its errors.
This is to be used around code which is not expected to signal an error
but which should be robust in the unexpected case that an error is signaled."
  (declare (debug t) (indent 0))
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         (progn ,@body)
       (error (message "Error: %S" ,err) nil))))

;;;###autoload
(defun assoc-string (key list &optional case-fold)
  "Like `assoc' but specifically for strings (and symbols).

This returns the first element of LIST whose car matches the string or
symbol KEY, or NIL if no match exists.  When performing the comparison,
symbols are first converted to strings.  If the optional arg CASE-FOLD
is non-nil, case is ignored.

Unlike `assoc', KEY can also match an entry in LIST consisting of a
single string, rather than a cons cell whose car is a string."
  (when (symbolp key)
    (setq key (symbol-name key)))
  (let* ((tail list) elt elt2 elt3 result)
    (while (and (null result) tail)
      (setq elt (car tail)
	    elt2 (if (consp elt) (car elt) elt)
	    elt3 (if (symbolp elt2) (symbol-name elt2) elt2)
	    result (and (stringp elt3)
			(eq (compare-strings elt3 0 nil key 0 nil case-fold) t)
			elt)
	    tail (cdr tail)))
    result))

;;;###autoload
(defun start-file-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
Similar to `start-process-shell-command', but calls `start-file-process'."
  (start-file-process
   name buffer
   (if (file-remote-p default-directory) "/bin/sh" shell-file-name)
   (if (file-remote-p default-directory) "-c" shell-command-switch)
   (mapconcat 'identity args " ")))

;;; subr-more.el ends here
