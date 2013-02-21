;;; file-util.el --- utilities for dealing with files

;; Copyright (C) 1985-1987, 1992-2012 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;###autoload
(defvar locate-dominating-stop-dir-regexp
  "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"
  "Regexp of directory names which stop the search in `locate-dominating-file'.
Any directory whose name matches this regexp will be treated like
a kind of root directory by `locate-dominating-file' which will stop its search
when it bumps into it.
The default regexp prevents fruitless and time-consuming attempts to find
special files in directories in which filenames are interpreted as hostnames,
or mount points potentially requiring authentication as a different user.")

;; (defun locate-dominating-files (file regexp)
;;   "Look up the directory hierarchy from FILE for a file matching REGEXP.
;; Stop at the first parent where a matching file is found and return the list
;; of files that that match in this directory."
;;   (catch 'found
;;     ;; `user' is not initialized yet because `file' may not exist, so we may
;;     ;; have to walk up part of the hierarchy before we find the "initial UID".
;;     (let ((user nil)
;;           ;; Abbreviate, so as to stop when we cross ~/.
;;           (dir (abbreviate-file-name (file-name-as-directory file)))
;;           files)
;;       (while (and dir
;;                   ;; As a heuristic, we stop looking up the hierarchy of
;;                   ;; directories as soon as we find a directory belonging to
;;                   ;; another user.  This should save us from looking in
;;                   ;; things like /net and /afs.  This assumes that all the
;;                   ;; files inside a project belong to the same user.
;;                   (let ((prev-user user))
;;                     (setq user (nth 2 (file-attributes dir)))
;;                     (or (null prev-user) (equal user prev-user))))
;;         (if (setq files (condition-case nil
;; 			    (directory-files dir 'full regexp 'nosort)
;; 			  (error nil)))
;;             (throw 'found files)
;;           (if (equal dir
;;                      (setq dir (file-name-directory
;;                                 (directory-file-name dir))))
;;               (setq dir nil))))
;;       nil)))

;;;###autoload
(defun locate-dominating-file (file name)
  "Look up the directory hierarchy from FILE for a file named NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.

This function only tests if FILE exists.  If you care about whether
it is readable, regular, etc., you should test the result."
  ;; We used to use the above locate-dominating-files code, but the
  ;; directory-files call is very costly, so we're much better off doing
  ;; multiple calls using the code in here.
  ;;
  ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
  ;; `name' in /home or in /.
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        ;; `user' is not initialized outside the loop because
        ;; `file' may not exist, so we may have to walk up part of the
        ;; hierarchy before we find the "initial UID".  Note: currently unused
        ;; (user nil)
        try)
    (while (not (or root
                    (null file)
                    ;; FIXME: Disabled this heuristic because it is sometimes
                    ;; inappropriate.
                    ;; As a heuristic, we stop looking up the hierarchy of
                    ;; directories as soon as we find a directory belonging
                    ;; to another user.  This should save us from looking in
                    ;; things like /net and /afs.  This assumes that all the
                    ;; files inside a project belong to the same user.
                    ;; (let ((prev-user user))
                    ;;   (setq user (nth 2 (file-attributes file)))
                    ;;   (and prev-user (not (equal user prev-user))))
                    (string-match locate-dominating-stop-dir-regexp file)))
      ;; FIXME? maybe this function should (optionally?)
      ;; use file-readable-p instead.  In many cases, an unreadable
      ;; FILE is no better than a non-existent one.
      ;; See eg dir-locals-find-file.
      (setq try (file-exists-p (expand-file-name name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    root))

(defcustom confirm-nonexistent-file-or-buffer 'after-completion
  "Whether confirmation is requested before visiting a new file or buffer.
If nil, confirmation is not requested.
If the value is `after-completion', confirmation is only
 requested if the user called `minibuffer-complete' right before
 `minibuffer-complete-and-exit'.
Any other non-nil value means to request confirmation.

This affects commands like `switch-to-buffer' and `find-file'."
  :group 'find-file
  :version "23.1"
  :type '(choice (const :tag "After completion" after-completion)
		 (const :tag "Never" nil)
		 (other :tag "Always" t)))

(defun confirm-nonexistent-file-or-buffer ()
  "Whether to request confirmation before visiting a new file or buffer.
The variable `confirm-nonexistent-file-or-buffer' determines the
return value, which may be passed as the REQUIRE-MATCH arg to
`read-buffer' or `find-file-read-args'."
  (cond ((eq confirm-nonexistent-file-or-buffer 'after-completion)
	 'confirm-after-completion)
	(confirm-nonexistent-file-or-buffer
	 'confirm)
	(t nil)))

;;; file-util.el ends here
