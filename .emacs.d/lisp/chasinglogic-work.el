;;; chasinglogic-work.el --- utilities for use at work

;; Copyright (C) 2020 Mathew Robinson

;; Author: Mathew Robinson <mathew@chasinglogic.io>
;; Created: 04 Jun 2020

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

  ;;; Copy the test path for Django manage.py test
(defun chasinglogic-copy-mpbx-test-path-for-here (&optional copy)
  (interactive (list t))
  (let* ((file-name (if (projectile-project-root)
                        (file-relative-name (buffer-file-name) (projectile-project-root))
                      (file-name-nondirectory (buffer-file-name))))
         (current-point (point))
         (test-name (progn
                      (re-search-backward "def ")
                      (forward-word)
                      (forward-char)
                      (thing-at-point 'sexp)))
         (test-class-name (progn
                            (re-search-backward "class ")
                            (forward-word)
                            (forward-char)
                            (thing-at-point 'sexp)))
         (test-path (concat
                     (string-remove-prefix "mpb/" file-name)
                     ":" test-class-name
                     "." test-name)))
    (message "Copied: %s" test-path)
    (kill-new test-path)
    (goto-char current-point)))

(defun chasinglogic-run-mpbx-test ()
  "Run the test under point"
  (interactive)
  (chasinglogic-copy-mpbx-test-path-for-here t)
  (let ((default-directory (projectile-project-root))
        (compilation-command (concat "cd mpb && python ./manage.py test " (current-kill 0))))
    (puthash default-directory compilation-command projectile-compilation-cmd-map)
    (compile compilation-command)))

  ;;; Copy the test path for Django manage.py test
(defun chasinglogic-copy-test-path-for-here (&optional full-file)
  "Copy the test path of the test under cursor for pytest.

FULL-FILE indicates the full file-name should be copied instead of
just the singular test."
  (interactive)
  (let* ((file-name (if (projectile-project-root)
                        (file-relative-name (buffer-file-name) (projectile-project-root))
                      (file-name-nondirectory (buffer-file-name))))
         (current-point (point))
         (test-name (progn
                      (re-search-backward "def ")
                      (forward-word)
                      (forward-char)
                      (thing-at-point 'sexp)))
         (test-class-name (progn
                            (re-search-backward "class ")
                            (forward-word)
                            (forward-char)
                            (thing-at-point 'sexp)))
         (test-path (concat
                     file-name
                     (unless full-file
                       (concat 
                        "::" test-class-name
                        "::" test-name)))))
    (message "Copied: %s" test-path)
    (kill-new test-path)
    (goto-char current-point)))

(defun chasinglogic-run-test ()
  "Run test under point."
  (interactive)
  (chasinglogic-copy-test-path-for-here)
  (let ((default-directory (projectile-project-root))
        (compilation-command (concat "./common/scripts/tests " (current-kill 0))))
    (puthash default-directory compilation-command projectile-compilation-cmd-map)
    (compile compilation-command)))

(defun chasinglogic-run-test-file ()
  "Run the currently visited test file."
  (interactive)
  (chasinglogic-copy-test-path-for-here t)
  (let ((default-directory (projectile-project-root))
        (compilation-command (concat "./common/scripts/tests " (current-kill 0))))
    (puthash default-directory compilation-command projectile-compilation-cmd-map)
    (compile compilation-command)))


(provide 'chasinglogic-work)

;;; chasinglogic-work.el ends here
