;;; chasinglogic-utils.el --- my utility functions stolen or otherwise

;; Copyright (C) 2018 Mathew Robinson

;; Author: Mathew Robinson <chasinglogic@gmail.com>
;; Created: 24 Aug 2018

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:


;; Indent the buffer
;;
;;     This function uses Emacs built in indent facilities to indent the
;;     entire buffer. It doesn't work so great on languages where
;;     whitespace has semantic meaning, like Python, but it is a godsend
;;     for structured languages that are commonly poorly formatted, like
;;     HTML.
(defun chasinglogic-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region-line-by-line (point-min) (point-max)))

;; Finding org files
;;
;;     I keep all of my org files in `org-directory' and some of them are
;;     encrypted. This macro lets me easily define functions for quickly
;;     finding them. It's a macro because [[https://www.jamesporter.me/2013/06/14/emacs-lisp-closures-exposed.html][Emacs has crazy scoping rules]]
;;     that make returning lambdas from functions difficult.
(defmacro chasinglogic-find-org-file (name)
  "Create a function to find the org file NAME."
  `(defun ,(intern (format "chasinglogic-find-org-file-%s" name)) ()
     (interactive)
     (let ((file-name (expand-file-name ,(format "%s.org" name) org-directory)))
       (find-file (if (file-exists-p (concat file-name ".gpg"))
                      (concat file-name ".gpg")
                    file-name)))))
(chasinglogic-find-org-file notes)
(chasinglogic-find-org-file ideas)
(chasinglogic-find-org-file todo)

(defun chasinglogic-select-frame-by-name ()
  "Search and select frames by name."
  (interactive)
  (select-frame-by-name
   (completing-read
    "Frame: "
    (mapcar 'get-frame-name (frame-list)))))

;; Open the shell with a good default buffer name
;;
;;     This function opens a shell with a buffer name that indicates what
;;     project it was opened in. If you run it again in that project it
;;     will instead just switch to the buffer.
(defun chasinglogic-shell ()
  "Open my shell in 'ansi-term'."
  (interactive)
  (let* ((project-name (if (projectile-project-name)
                           (projectile-project-name)
                         "main"))
         (shell-buf-name (concat project-name "-shell"))
         (shell-buf-asterisks (concat "*" shell-buf-name "*")))
    (if (get-buffer shell-buf-asterisks)
        (switch-to-buffer shell-buf-asterisks)
      (ansi-term (executable-find "bash") shell-buf-name))))

;; GDB/LLDB Debugging
;;     I maintain a developer toolchain that means I have to frequently
;;     interact with GDB *and* LLDB. Since LLDB does not have Emacs
;;     integration this function allow me to easily get breakpoints for
;;     wherever I am.
;;
;;     It checks for the `projectile-project-root' and if found will make
;;     the filename relative to this directory. Otherwise the full path
;;     of the `buffer-file-name' will be used. It grabs the line number
;;     the point is currently at then simply concatenates the generated
;;     filename, a colon, and the line number. When called interactively
;;     it will add it to the kill ring effectively "copying" the
;;     breakpoint for easy pasting.


(defun chasinglogic-copy-breakpoint-for-here (&optional copy)
  "Return a filename:linenumber pair for point for use with LLDB/GDB.

If COPY is provided copy the value to kill ring instead of returning."
  (interactive (list t))
  (let* ((line-number (format "%d" (line-number-at-pos)))
         (file-name (if (projectile-project-root)
                        (file-relative-name (buffer-file-name) (projectile-project-root))
                      (file-name-nondirectory (buffer-file-name))))
         (breakpoint (concat file-name ":" line-number)))
    (if copy
        (progn
          (kill-new breakpoint)
          (message "%s" breakpoint))
      breakpoint)))

;; Edit current buffer with sudo
;;
;;     The title here is self explanatory. It uses Emacs [[https://www.emacswiki.org/emacs/TrampMode][TRAMP Mode]] to
;;     open the file as root on localhost. It does not require SSH and
;;     instead uses a special TRAMP protocol that just calls `sudo' to
;;     make the user change.
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

(defun chasinglogic-find-thing ()
  "Find the thing I want."
  (interactive)
  (let* ((buffers (buffer-list))
         (buffer-names (mapc 'buffer-name buffers))
         (project-files (when (projectile-project-root)
                          (projectile-files-in-project-directory (projectile-project-root))))
         (collection (apply 'append (remove nil (list buffer-names project-files))))
         (selection (ivy-read
                     "Thing: "
                     collection)))

    (message "%s" selection)))

(defun chasinglogic-raza ()
  "TRAMP into my remote Mongo repository on my desktop."
  (interactive)
  (find-file "/ssh:chasinglogic@raza:/home/chasinglogic/Work/mongo"))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun chasinglogic-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(provide 'chasinglogic-utils)

;;; chasinglogic-utils.el ends here
