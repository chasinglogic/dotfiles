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

(defun chasinglogic-dired-current-file ()
  "Dired the directory of the current file."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (dired-up-directory)
    (dired (file-name-directory (buffer-file-name)))))

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

(defun chasinglogic-switch-to-scratch-buffer ()
  "Switch to scratch buffer quickly."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun chasinglogic-reload-config ()
  "Reload my configuration file."
  (interactive)
  (load-file (chasinglogic-dotfile-location)))

(defun chasinglogic-projectile-command (cmd &optional sync)
  "Run CMD at the projectile project root.

If SYNC provided will run make command synchronously"
  (interactive "s")
  (let (
        (default-directory (projectile-project-root)))
    (shell-command cmd
     (format "*%s %s output*" (projectile-project-name) cmd))))

(defun chasinglogic-scons (&optional args)
  "Run scons at root of project, only useful for MongoDB, passes ARGS to scons."
  (interactive "s")
  (chasinglogic-projectile-command
   (concat "python3 ./buildscripts/scons.py " args)))

(defun chasinglogic-prq ()
  "Open a pull request on github."
  (interactive)
  (shell-command "hub pull-request -m \"$(git log -n 1 --format='%s')\" --browse"))

(defun chasinglogic-shell ()
  "Open my shell in 'ansi-term'."
  (interactive)
  (let* (
        (shell-buf-name
         (concat
          (if (projectile-project-name)
              (projectile-project-name)
            "main")
          "-shell"))
        (shell-buf-asterisks
         (concat "*" shell-buf-name "*"))
        )
    (if (get-buffer shell-buf-asterisks)
        (switch-to-buffer shell-buf-asterisks)
      (ansi-term
       (executable-find "zsh")
       shell-buf-name))))

;; when switching to a buffer if it's a shell automatically go to
;; insert mode
(defvar previous-buffer-name (buffer-name))
(defun insert-mode-in-term-hook (&rest _args)
  "When going into a term mode set evil to insert mode."
  (when (and
         (string-match-p
          (regexp-quote "-shell") (buffer-name))
         (not (eq (buffer-name) previous-buffer-name)))
    (setq previous-buffer-name (buffer-name))
    (evil-insert-state)))
(add-hook 'buffer-list-update-hook 'insert-mode-in-term-hook)


(defun chasinglogic-raza ()
  "Tramp into my MongoDB repo on my work dev machine."
  (interactive)
  (set-frame-name "raza")
  (dired "/ssh:dev:/home/chasinglogic/Code/mongodb/mongo"))

(defun chasinglogic-work ()
  "Project switch to Mongo."
  (interactive)
  (projectile-switch-project-by-name "mongo"))

(defun chasinglogic-sync-mongo ()
  "Run my sync mongo script."
  (interactive)
  (chasinglogic-projectile-command "sync_mongo"))

(defun chasinglogic-make-and-select-frame (&optional name)
  "Create and select a new frame.

Optionally set frame NAME on creation."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame frame)
    (when name
      (set-frame-name name))
    frame))

(defun chasinglogic-email ()
  "Open my email frame or create it."
  (interactive)
  (if (get-a-frame "email")
      (select-frame-by-name "email")
    (progn
      (chasinglogic-make-and-select-frame "email")
      (mu4e))))

(defun chasinglogic-enable-sync-mongo-on-save ()
  "Enable sync_mongo on save for this buffer."
  (interactive)
  (add-hook 'after-save-hook 'chasinglogic-sync-mongo nil t))

(defun chasinglogic-disable-sync-mongo-on-save ()
  "Disable sync_mongo on save for this buffer."
  (interactive)
  (remove-hook 'after-save-hook 'chasinglogic-sync-mongo nil t))

(defun chasinglogic-evergreen-patch-toolchain (variants)
  "Patch the toolchain-builder project"
  (interactive "sVariants: ")
  (evergreen-patch
   ;; project
   "toolchain-builder"
   ;; finalize
   t
   ;; browse
   t
   ;; alias
   nil
   ;; variants
   (split-string variants ",")
   ;; tasks
   (list "all")
   ;; description
   (evergreen--generate-description)
   ;; large
   nil))

(defun chasinglogic-evergreen-patch-required ()
  "Run the MongoDB required patch build alias."
  (interactive)
  (evergreen-patch
   ;; project
   (cond
    ((string-equal
      (regexp-quote (projectile-project-name)) "mongo")
     "mongodb-mongo-master")
    (t (projectile-project-name)))
   ;; finalize
   t
   ;; browse
   t
   ;; alias
   "required"
   ;; variants
   nil 
   ;; tasks
   nil 
   ;; description
   (evergreen--generate-description)
   ;; large
   nil))

(defun chasinglogic-copy-current-file-name (&optional copy)
  "Return a filename:linenumber pair for point for use with LLDB/GDB.

If COPY is provided copy the value to kill ring instead of returning."
  (interactive (list t))
  (let ((st
         (concat
          (if (projectile-project-root)
              (file-relative-name (buffer-file-name) (projectile-project-root))
            (file-name-nondirectory (buffer-file-name))))))
    (if copy
        (progn
          (kill-new st)
          (message "%s" st))
      st)))

(defun chasinglogic-copy-breakpoint-for-here (&optional copy)
  "Return a filename:linenumber pair for point for use with LLDB/GDB.

If COPY is provided copy the value to kill ring instead of returning."
  (interactive (list t))
  (let ((st
         (concat
          (chasinglogic-copy-current-file-name)
          ":"
          (format "%d" (line-number-at-pos)))))
    (if copy
        (progn
          (kill-new st)
          (message "%s" st))
      st)))

(defun chasinglogic-move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun chasinglogic-move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Don't show the DFM sync window
(add-to-list 'display-buffer-alist
             (cons "\\*dfm output\\*.*" (cons #'display-buffer-no-window nil)))
(defun chasinglogic-sync-dotfiles (msg)
  "Run dfm sync"
  (interactive "sCommit message: ")
  (shell-command
   (format "dfm sync --message='%s' &" msg)
   "*dfm output*"))

(provide 'chasinglogic-utils)

;;; chasinglogic-utils.el ends here
