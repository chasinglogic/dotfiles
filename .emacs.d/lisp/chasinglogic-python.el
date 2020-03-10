;;; chasinglogic-python.el --- Python language setup

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

;; Python
;;
;; I write a lot of Python code. Luckily I only write Python 3 code
;; nowadays so the first thing to do is set the
;; `python-shell-interpreter' variable to Python 3. Additionally tell
;; Flycheck to always use this variable for the various Python linters
;; it runs.

;; Use correct Python3
(setq-default python-shell-interpreter (if (eq system-type 'darwin)
                                           "/usr/local/bin/python3"
                                         "python3"))
(setq-default flycheck-python-flake8-executable python-shell-interpreter
              flycheck-python-pylint-executable python-shell-interpreter
              flycheck-python-pycompile-executable python-shell-interpreter)

;; Next I use the Black Python formatter for my code. This package
;; integrates it into Emacs and lets me run it as an after save
;; hook. My hook has to be a little smarter however because my work
;; projects do not use this formatter so define a "black list" for
;; Black and only add the hook if we aren't in one of those projects.
(use-package blacken
  :commands 'blacken-buffer
  :init
  (setq-default chasinglogic-blacken-black-list
                '("scons"
                  "Work"))

  (defun chasinglogic-python-format-hook ()
    "Set up blacken-buffer on save if appropriate."
    (unless (or
             (member (projectile-project-name) chasinglogic-blacken-black-list)
             (seq-some '(lambda (item)
                          (string-match-p (regexp-quote item) (buffer-file-name)))
                       chasinglogic-blacken-black-list))
      (message "Not in a blacklisted project, enabling format on save.")
      (add-hook 'before-save-hook 'blacken-buffer nil t)))
  (add-hook 'python-mode-hook 'chasinglogic-python-format-hook))

;; Making Emacs and virtualenvs work together has been one of the most
;; frustrating things about my time with Emacs. After literal years of
;; tweaking and testing I finally have a solution that I like. I use
;; `virtualenvwrapper' to create my virtualenvs with names that match
;; the names returned by =(projectile-project-name)=, essentially this
;; is just the basename of the project directory. Then whenever I run
;; `projectile-switch-project' check for a matching virtualenv if so
;; activate it with the `pyvenv' package.
(use-package pyvenv
  :config
  (defun chasinglogic-auto-venv ()
    "Automatically setup the venv when entering a project"
    (when (file-exists-p (concat "~/.virtualenvs/" (projectile-project-name)))
      (pyvenv-workon (projectile-project-name))))
  (add-hook 'projectile-after-switch-project-hook 'chasinglogic-auto-venv))

;; Load SCons files as Python
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vars\\'" . python-mode))

(provide 'chasinglogic-python)

;;; chasinglogic-python.el ends here
