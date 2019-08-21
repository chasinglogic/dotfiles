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

(require 'dired)

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

(defun chasinglogic-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region-line-by-line (point-min) (point-max)))

(defun chasinglogic-dotfile-location ()
  "Return the init file path."
  (concat (getenv "HOME") "/.emacs.d/init.el"))

(defun chasinglogic-find-dotfile ()
  "Find my init file."
  (interactive)
  (find-file (chasinglogic-dotfile-location)))

(defmacro chasinglogic-find-org-file (name)
  "Return a function to find the org file NAME."
  `(defun ,(intern (format "chasinglogic-find-org-file-%s" name)) ()
     (interactive)
     (find-file (expand-file-name ,(format "%s.org" name) org-directory))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
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

(defun chasinglogic-add-projector-projects-to-projectile ()
  "Add projector projects to projectile."
  (interactive)
  (setq
   projectile-known-projects
   (delete ""
           (split-string
            (shell-command-to-string "projector list") "\n"))))

(defun chasinglogic-projectile-command (cmd &optional sync)
  "Run CMD at the projectile project root.

If SYNC provided will run make command synchronously"
  (interactive "s")
  (let (
        (default-directory (projectile-project-root)))
    (shell-command cmd
     (format "*%s %s output*" (projectile-project-name) cmd))))

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

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(provide 'chasinglogic-utils)

;;; chasinglogic-utils.el ends here
