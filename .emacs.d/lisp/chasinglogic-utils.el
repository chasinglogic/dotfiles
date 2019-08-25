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

;; Delete current buffer and file
;;     I will never understand why this isn't baked into Emacs. I've
;;     stolen this from Spacemacs who stole it from Magnars. Now you can
;;     steal it from me. In short it will delete the buffer and the file
;;     it's visiting.
;; from spacemacs-core
;; from magnars
(defun chasinglogic-delete-current-buffer-file ()
  "Remove file connected to current buffer and kill the related buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (projectile-project-p)
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

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

;; Rename file and buffer
;;
;;     Similar to [[Delete file and buffer]] I'm not sure why this isn't
;;     built into Emacs. This does a rename using `default-directory' and
;;     relative paths to the file do work. I took this from
;;     [[http://steve.yegge.googlepages.com/my-dot-emacs-file][Steve Yegge's dot Emacs]].
(defun chasinglogic-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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


(provide 'chasinglogic-utils)

;;; chasinglogic-utils.el ends here
