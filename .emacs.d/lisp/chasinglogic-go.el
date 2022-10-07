;;; chasinglogic-go.el --- Go language setup

;; Copyright (C) 2020 Mathew Robinson

;; Author: Mathew Robinson <mathew@chasinglogic.io>
;; Created: 24 Feb 2020

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

(use-package go-mode
  :mode ("\\.go\\'")
  :init
  (add-hook 'go-mode-hook 'chasinglogic-enable-lsp)
  :config
  (setq gofmt-command "goimports")
  (defun chasinglogic-go-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save))
  (add-hook 'go-mode-hook 'chasinglogic-go-hook))

(defun chasinglogic-go-run-test-command (&optional full-file)
  "Copy the test path of the test under cursor for pytest.

FULL-FILE indicates the full file-name should be copied instead of
just the singular test."
  (interactive)
  (let* ((file-name (if (projectile-project-root)
                        (file-relative-name (buffer-file-name) (projectile-project-root))
                      (buffer-file-name)))
         (test-path (concat "./" (file-name-directory file-name)))
         (current-point (point))
         (test-name (progn
                      (re-search-backward "func ")
                      (forward-word)
                      (forward-char)
                      (thing-at-point 'sexp)))
         (test-command
          (concat "go test " (if full-file
                                 test-path
                               (concat "-run " test-name " " test-path)))))
    (kill-new test-command)
    (goto-char current-point)
    test-command))

(provide 'chasinglogic-go)

;;; chasinglogic-go.el ends here
