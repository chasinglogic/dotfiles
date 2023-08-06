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

(setq-default python-shell-interpreter (executable-find "python3")
              flycheck-python-pycompile-executable python-shell-interpreter)

(use-package pyvenv
  :commands (pyvenv-mode pyvenv-activate))

;; Sort imports
(use-package py-isort
  :commands 'py-isort-buffer
  :init
  (add-hook 'python-mode-hook 'py-isort-before-save))

;; Next I use the Black Python formatter for my code. This package
;; integrates it into Emacs and lets me run it as an after save
;; hook. My hook has to be a little smarter however because my work
;; projects do not use this formatter so define a "black list" for
;; Black and only add the hook if we aren't in one of those projects.
(use-package blacken
  :hook (python-mode . blacken-mode)
  :commands 'blacken-buffer)

(use-package pipenv
  :commands (pipenv-mode pipenv-activate)
  :hook (python-mode . pipenv-mode))

(add-hook 'python-mode-hook 'chasinglogic-enable-lsp)

(defun chasinglogic-auto-activate-venv ()
    (let ((venv-dir (concat (projectile-project-root) "env")))
      (cond
       ((file-exists-p venv-dir)
        (progn
          (pyvenv-mode 1)
          (pyvenv-activate venv-dir)))
       ((file-exists-p "Pipfile")
        (progn
          (pipenv-mode 1)
          (pipenv-activate))))))

(use-package flymake-ruff
  :hook (eglot-managed-mode . flymake-ruff-load))

(provide 'chasinglogic-python)

;;; chasinglogic-python.el ends here
