;;; bytedecl.el --- declarations to manage bytecompiler warnings

;; Copyright (C) 2002 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Maintainer: XEmacs Development Team
;; Adapted-By: Stephen J. Turnbull <stephen@xemacs.org>
;; Created: 2007-03-06
;; Keywords: internal, bytecompiler

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Synched up with: Not in GNU.

;; Moved to xemacs-base package by Stephen Turnbull on 2007-03-06.
;; Please keep these macros synched with src/bytecomp-runtime.el.

;;; Commentary:

;; Macros to cleanly eliminate warnings about undefined functions
;; or variables when the code knows what it's doing.  These macros DO
;; NOT rely on any byte-compiler changes, and thus can be copied into
;; a package and used within it.

;; NOTE: As a result of the above requirement, the macros rely on
;; "tricks" to get the warnings suppressed.  A cleaner way, of course,
;; would be to extend the byte compiler to provide a proper interface.

;; To make these macros available in a package, simply `(require 'bytedecl)'.
;; This library should contain no autoloads, as these macros are dumped
;; into 21.5.  (Eventually the `package-suppress' mechanism could be used.)

;;; Code:

;; #### Should we require an unquoted symbol rather than a quoted one,
;; as we currently do?  The quoting gets no generality, as `eval' is
;; called at compile time.  But most functions and macros want quoted
;; arguments, and I find it extremely confusing to deal with cases
;; such as `throw' requiring a quoted argument but `block' an unquoted
;; one.

(put 'with-boundp 'lisp-indent-function 1)
(defmacro with-boundp (variables &rest body)
  "Evaluate BODY, but do not issue bytecomp warnings about VARIABLES undefined.
VARIABLES can be a symbol or a list of symbols and must be quoted.  When
compiling this file, the warnings `reference to free variable VARIABLE' and
`assignment to free variable VARIABLE' will not occur anywhere in BODY, for
any of the listed variables.  This is a clean way to avoid such warnings.

See also `if-boundp', `when-boundp', and `and-boundp' (ways to
conditionalize on a variable being bound and avoid warnings),
`declare-boundp' (issue a variable call without warnings), and
`globally-declare-boundp' (avoid warnings throughout a file about a
variable)."
  (setq variables (eval variables))
  (unless (consp variables)
      (setq variables (list variables)))
  `(progn
     (declare (special ,@variables))
     ,@body))

(put 'if-boundp 'lisp-indent-function 2)
(defmacro if-boundp (variable then &rest else)
  "Equivalent to (if (boundp VARIABLE) THEN ELSE) but handles bytecomp warnings.
VARIABLE should be a quoted symbol.  When compiling this file, the warnings
`reference to free variable VARIABLE' and `assignment to free variable
VARIABLE' will not occur anywhere in the if-statement.  This is a clean way
to avoid such warnings.  See also `with-boundp' and friends."
  `(with-boundp ,variable
     (if (boundp ,variable) ,then ,@else)))

(put 'when-boundp 'lisp-indent-function 1)
(defmacro when-boundp (variable &rest body)
  "Equivalent to (when (boundp VARIABLE) BODY) but handles bytecomp warnings.
VARIABLE should be a quoted symbol.  When compiling this file, the warnings
`reference to free variable VARIABLE' and `assignment to free variable
VARIABLE' will not occur anywhere in the when-statement.  This is a clean
way to avoid such warnings.  See also `with-boundp' and friends."
  `(with-boundp ,variable
     (when (boundp ,variable) ,@body)))

(put 'and-boundp 'lisp-indent-function 1)
(defmacro and-boundp (variable &rest args)
  "Equivalent to (and (boundp VARIABLE) ARGS) but handles bytecomp warnings.
VARIABLE should be a quoted symbol.  When compiling this file, the warnings
`reference to free variable VARIABLE' and `assignment to free variable
VARIABLE' will not occur anywhere in the and-statement.  This is a clean
way to avoid such warnings.  See also `with-boundp' and friends."
  `(with-boundp ,variable
     (and (boundp ,variable) ,@args)))

(defmacro declare-boundp (variable)
  "Evaluate VARIABLE without bytecomp warnings about the symbol.

Sample usage is

  (declare-boundp gpm-minor-mode)

which is equivalent to

  (with-boundp 'gpm-minor-mode
    gpm-minor-mode)

See also `with-boundp' and friends."
  `(with-boundp ',variable ,variable))

(defmacro globally-declare-boundp (variables)
  "Declare that all free uses of VARIABLES in this file are valid.
VARIABLES can be a symbol or a list of symbols and must be quoted.

When compiling this file, the warnings `reference to free variable
VARIABLE' and `assignment to free variable VARIABLE' will not occur
regardless of where references to VARIABLE occur in the file.

In general, you should *NOT* use this; use `with-boundp' or its friends to
wrap individual uses, as necessary.  That way, you're more likely to
remember to put in the explicit checks for the variable's existence that
are usually necessary.  However, `globally-declare-boundp' is better in
some circumstances, such as when writing an ELisp package that makes
integral use of optionally-compiled-in functionality (typically, an
interface onto a system library) and checks for the existence of the
functionality at some entry point to the package.  See
`globally-declare-fboundp' for more information."
  (setq variables (eval variables))
  (if (not (consp variables))
      (setq variables (list variables)))
  `(progn
     ;; (defvar FOO) has no side effects.
     ,@(mapcar #'(lambda (sym) `(defvar ,sym)) variables)))

(defun byte-compile-with-fboundp (form)
  (byte-compile-form (cons 'progn (cdr (cdr form))))
  ;; Unfortunately, byte-compile-unresolved-functions is used not only
  ;; for unresolved-function warnings, but also in connection with the
  ;; following warnings:

  ;; "defsubst %s was used before it was defined"
  ;; "%s being defined to take %s%s, but was previously called with %s"

  ;; By hacking byte-compile-unresolved-functions like this, we
  ;; effectively disable these warnings.  But code should not be using
  ;; `with-fboundp' with a function defined later on in the same
  ;; file, so this is not a big deal.

  (let ((symbols (eval (car (cdr form)))))
    (unless (consp symbols)
      (setq symbols (list symbols)))
    (setq symbols (mapcar #'(lambda (sym) (cons sym nil)) symbols))
    (setq byte-compile-unresolved-functions
	  (set-difference byte-compile-unresolved-functions symbols
			  :key #'car))
    ))

;; EEEEEEEEVIL hack.  We need to create our own byte-compilation
;; method so that the proper variables are bound while compilation
;; takes place (which is when the warnings get noticed and batched
;; up).  What we really want to do is make `with-fboundp' a macro
;; that simply `progn's its BODY; but GOD DAMN IT, macros can't have
;; their own byte-compilation methods!  So we make `with-fboundp' a
;; macro calling `with-fboundp-1', which is cleverly aliased to
;; progn.  This way we can put a byte-compilation method on
;; `with-fboundp-1', and when interpreting, progn will duly skip
;; the first, quoted argument, i.e. the symbol name. (We could make
;; `with-fboundp-1' a regular function, but then we'd have to thunk
;; BODY and eval it at runtime.  We could probably just do this using
;; (apply 'progn BODY), but the existing method is more obviously
;; guaranteed to work.)
;;
;; In defense, cl-macs.el does a very similar thing with
;; `cl-block-wrapper'.

(put 'with-fboundp-1 'byte-compile 'byte-compile-with-fboundp)
(defalias 'with-fboundp-1 'progn)

(put 'with-fboundp 'lisp-indent-function 1)
(defmacro with-fboundp (functions &rest body)
  "Evaluate BODY, but do not issue bytecomp warnings about FUNCTIONS undefined.
FUNCTIONS can be a symbol or a list of symbols and must be quoted.  When
compiling this file, the warning `the function FUNCTION is not known to be
defined' will not occur anywhere in BODY, for any of the listed functions.
This is a clean way to avoid such warnings.

See also `if-fboundp', `when-fboundp', and `and-fboundp' (ways to
conditionalize on a function being bound and avoid warnings),
`declare-fboundp' (issue a function call without warnings), and
`globally-declare-fboundp' (avoid warnings throughout a file about a
function)."
  `(with-fboundp-1 ,functions ,@body))

(put 'if-fboundp 'lisp-indent-function 2)
(defmacro if-fboundp (function then &rest else)
  "Equivalent to (if (fboundp FUNCTION) THEN ELSE) but handles bytecomp warnings.
FUNCTION should be a quoted symbol.  When compiling this file, the warning
`the function FUNCTION is not known to be defined' will not occur anywhere
in the if-statement.  This is a clean way to avoid such warnings.  See also
`with-fboundp' and friends."
  `(with-fboundp ,function
     (if (fboundp ,function) ,then ,@else)))

(put 'when-fboundp 'lisp-indent-function 1)
(defmacro when-fboundp (function &rest body)
  "Equivalent to (when (fboundp FUNCTION) BODY) but handles bytecomp warnings.
FUNCTION should be a quoted symbol.  When compiling this file, the warning
`the function FUNCTION is not known to be defined' will not occur anywhere
in the when-statement.  This is a clean way to avoid such warnings.  See also
`with-fboundp' and friends."
  `(with-fboundp ,function
     (when (fboundp ,function) ,@body)))

(put 'and-fboundp 'lisp-indent-function 1)
(defmacro and-fboundp (function &rest args)
  "Equivalent to (and (fboundp FUNCTION) ARGS) but handles bytecomp warnings.
FUNCTION should be a quoted symbol.  When compiling this file, the warning
`the function FUNCTION is not known to be defined' will not occur anywhere
in the and-statement.  This is a clean way to avoid such warnings.  See also
`with-fboundp' and friends."
  `(with-fboundp ,function
     (and (fboundp ,function) ,@args)))

(defmacro declare-fboundp (form)
  "Execute FORM (a function call) without bytecomp warnings about the call.
Sample usage is

  (declare-fboundp (x-keysym-on-keyboard-sans-modifiers-p 'backspace))

which is equivalent to

  (with-fboundp 'x-keysym-on-keyboard-sans-modifiers-p
    (x-keysym-on-keyboard-sans-modifiers-p 'backspace))

See also `with-fboundp' and friends."
  `(with-fboundp ',(car form) ,form))

(defmacro globally-declare-fboundp (functions)
  "Declare that all calls to function FUNCTIONS in this file are valid.
FUNCTIONS can be a symbol or a list of symbols and must be quoted.

When compiling this file, the warning `the function FUNCTION is not known
to be defined' will not occur regardless of where calls to FUNCTION occur
in the file.

In general, you should *NOT* use this; use `with-fboundp' or its friends to
wrap individual uses, as necessary.  That way, you're more likely to
remember to put in the explicit checks for the function's existence that
are usually necessary.  However, `globally-declare-fboundp' is better in
some circumstances, such as when writing an ELisp package that makes
integral use of optionally-compiled-in functionality (typically, an
interface onto a system library) and checks for the existence of the
functionality at some entry point to the package.  The file `ldap.el' is a
good example: It provides a layer on top of the optional LDAP ELisp
primitives, makes calls to them throughout its code, and verifies the
presence of LDAP support at load time.  Putting calls to `declare-fboundp'
throughout the code would be a major annoyance."
  (when (cl-compiling-file)
    (setq functions (eval functions))
    (if (not (consp functions))
	(setq functions (list functions)))
    ;; Another hack.  This works because the autoload environment is
    ;; currently used ONLY to suppress warnings, and the actual
    ;; autoload definition is not used. (NOTE: With this definition,
    ;; we will get spurious "multiple autoloads for %s" warnings if we
    ;; have an autoload later in the file for any functions in FUNCTIONS.
    ;; This is not something that code should ever do, though.)
    (setq byte-compile-autoload-environment
	  (append (mapcar #'(lambda (sym) (cons sym nil)) functions)
		  byte-compile-autoload-environment)))
  nil)

(defun byte-compile-with-byte-compiler-warnings-suppressed (form)
  (let ((byte-compile-warnings byte-compile-warnings)
	(types (car (cdr form))))
    (unless (consp types)
      (setq types (list types)))
    (if (eq byte-compile-warnings t)
	(setq byte-compile-warnings byte-compile-default-warnings))
    (setq byte-compile-warnings (set-difference byte-compile-warnings types))
    (byte-compile-form (cons 'progn (cdr (cdr form))))))

;; Same hack here as with `with-fboundp'.
(put 'with-byte-compiler-warnings-suppressed-1 'byte-compile
     'byte-compile-with-byte-compiler-warnings-suppressed)
(defalias 'with-byte-compiler-warnings-suppressed-1 'progn)

(put 'with-byte-compiler-warnings-suppressed 'lisp-indent-function 1)
(defmacro with-byte-compiler-warnings-suppressed (type &rest body)
  "Evaluate BODY, but do not issue bytecomp warnings TYPE.
TYPE should be one of `redefine', `callargs', `subr-callargs',
`free-vars', `unresolved', `unused-vars', `obsolete', or `pedantic',
or a list of one or more of these symbols. (See `byte-compile-warnings'.)
TYPE must be quoted.

NOTE: You should *NOT* under normal circumstances be using this!
There are better ways of avoiding most of these warnings.  In particular:

-- use (declare (special ...)) if you are making use of
   dynamically-scoped variables.
-- use `with-fboundp' and friends to avoid warnings about undefined functions
   when you know the function actually exists.
-- use `with-boundp' and friends to avoid warnings about undefined variables
   when you know the variable actually exists.
-- use `with-obsolete-variable' or `with-obsolete-function' if you
   are purposely using such a variable or function."
  `(with-byte-compiler-warnings-suppressed-1 ,type ,@body))

;; #### These should be more clever.  You could (e.g.) try fletting
;; `byte-compile-obsolete' or temporarily removing the obsolete info
;; from the symbol and putting it back with an unwind-protect. (Or
;; better, modify the byte-compiler to provide a proper solution, and
;; fix these macros to use it if available, or fall back on the way
;; below.  Remember, these definitions need to work with an unchanged
;; byte compiler so that they can be copied and used in packages.)

(put 'with-obsolete-variable 'lisp-indent-function 1)
(defmacro with-obsolete-variable (symbol &rest body)
  "Evaluate BODY but do not warn about usage of obsolete variable SYMBOL.
SYMBOL must be quoted and can be a list of SYMBOLS.  See also
`with-obsolete-function'."
  `(with-byte-compiler-warnings-suppressed 'obsolete ,@body))

(put 'with-obsolete-function 'lisp-indent-function 1)
(defmacro with-obsolete-function (symbol &rest body)
  "Evaluate BODY but do not warn about usage of obsolete function SYMBOL.
SYMBOL must be quoted and can be a list of SYMBOLS.  See also
`with-obsolete-variable'."
  `(with-byte-compiler-warnings-suppressed 'obsolete ,@body))

(provide 'bytedecl)

;;; bytedecl.el ends here
