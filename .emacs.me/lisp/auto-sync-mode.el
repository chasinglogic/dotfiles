;;; auto-sync-mode.el --- Automatically rsync projects to remote servers on save

;; Copyright (C) 2018 Mathew Robinson

;; Author: Mathew Robinson <chasinglogic@gmail.com>
;; Created: 10 Oct 2018

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

;; This is a minor mode that when enabled will automagically sync your
;; code to some remote server. This is really handy when TRAMP is
;; either too slow or keeping local copies of source changes is useful
;; for some other reason.

;; The default mode `auto-sync-mode' is local by default. This is the
;; recommended way to use this mode as it adds an `after-save-hook'
;; which could lead to weird results when switching to a new buffer. I
;; recommend using a major mode hook that inspects
;; `projectile-project-name' to enable `auto-sync-mode' if you want it
;; to be automatic.

;; If for some reason you would like to sync all projects to your
;; remote host all the time `auto-sync-global-mode' is provided. But
;; don't email me when stuff goes crazy in global mode.

;; It defaults to using -a to sync files from project root to project
;; root. If you would like to add more options or to change this
;; behavior see the documentation for the variable auto-sync-flags
;; with `C-h v auto-sync-flags`

;; You **must** set auto-sync-target-host to some value. Either as a
;; local variable via `.dir-locals.el', or some other mechanism, or
;; globally via customize / init.el. auto-sync-mode will refuse to
;; sync if it's not set.

;;; Code:

(defgroup auto-sync nil
  "Sync projectile directories to remote servers."
  :group 'external)

(defcustom auto-sync-binary (executable-find "rsync")
  "Binary to use when syncing. It must be take rsync compatible
flags. Defaults to rsync in the $PATH"
  :type 'string
  :group 'auto-sync)

(defcustom auto-sync-ignore
  (list "/build" "__pycache__" "*.pyc" "/.ccls_cache" "/.mypy_cache")
  "List of patterns that should be ignored by the sync command."
  :type 'list
  :group 'auto-sync)

(defcustom auto-sync-flags (list "-a")
  "Flags to pass to rsync. 

Note that this defaults to -a which is usually want you want, if
you want to add extra flags it's recommended you use
`'(add-host-list 'auto-sync-flags \"--myflags\")`' or understand
that rsync might exit with errors if you overwrite this and do
not know all of the flags you're using."
  :type 'list
  :group 'auto-sync)

(defcustom auto-sync-target-host nil
  "The host to sync to. This is safe as a buffer local variable."
  :type 'string
  :group 'auto-sync)

(defcustom auto-sync-target-dir nil
  "The target directory to sync to. If nil it will be assumed to
  be the same path on the parent and the host."
  :type 'string
  :group 'auto-sync)


(defun auto-sync--exclude-flags ()
  "Return the exclude flags as a list"
  (mapcar
   (lambda (pat)
     (format "--exclude=%s" pat))
   auto-sync-ignore))

(defun auto-sync--sync-target ()
  "Return the host and path to pass to rsync"
  (let* ((host auto-sync-target-host)
         (dir (if auto-sync-target-dir
                  auto-sync-target-dir
                (string-trim-right (projectile-project-root) "/"))))
    (if host
        (concat host ":" dir)
      (error "auto-sync-target-host cannot be nil"))))

(defun auto-sync--get-rsync-args ()
  "Return the rsync command based on customized variables."
  (remove nil
          (append (auto-sync--exclude-flags)
                  auto-sync-flags
                  (list
                   (string-trim-right (projectile-project-root) "/")
                   (auto-sync--sync-target)))))

(defun auto-sync-sync ()
  "Sync the current project to the remote."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (name (format "*%s sync*" default-directory))
         (args (auto-sync--get-rsync-args))
         (proc-args (append (list name name auto-sync-binary) args)))
    (apply 'start-process proc-args)))

(define-minor-mode auto-sync-mode
  "Automatically sync your project on file save"
  :lighter " auto-sync"
  :group 'auto-sync
  (if auto-sync-mode
      ;; Add hook locally
      (add-hook 'after-save-hook 'auto-sync-sync nil t)
    ;; remove-hook has no local version
    (remove-hook 'after-save-hook 'auto-sync-sync)))

(define-minor-mode auto-sync-global-mode
  "Automatically sync any project your in on file save"
  :lighter " auto-sync-global"
  :group 'auto-sync
  :global t
  (if auto-sync-global-mode
      (add-hook 'after-save-hook 'auto-sync-sync)
    (remove-hook 'after-save-hook 'auto-sync-sync)))

(provide 'auto-sync-mode)

;;; auto-sync-mode.el ends here
