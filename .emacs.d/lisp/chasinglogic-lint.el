;;; chasinglogic-lint.el --- Set up linters

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

;; Flycheck
;;
;; Every good editor has syntax checking and Emacs is no different. I
;; use Flycheck for this since it's the most consistent, best
;; defaults, and functional package I've found for it. Flymake
;; recently got a rewrite in Emacs core but I still prefer Flycheck. I
;; do not install many =flycheck-*= packages as I use `lsp-mode' which
;; integrates with Flycheck and for everything else the Flycheck
;; defaults work great.
;;
;; I diminish Flycheck because it's almost always enabled so no reason
;; to pollute the mode-line. Additionally I set the variable
;; `flycheck-sh-posix-dash-executable' to an empty string. Most
;; people, I certainly didn't, don't know that there is a minimalistic
;; bash alternative called `dash' that is on a lot of Debian
;; systems. It's an awful shell IMO but Flycheck supports linting for
;; it. I use Dash.app on my Macbook and so Flycheck constanstly opens
;; Dash.app and freaks out whenever I'm in a `sh-mode' buffer. Setting
;; this to an empty string prevents Flycheck from trying to test Dash
;; shell syntax.
;;
;; I enable Flycheck for all text modes.
(use-package flycheck
  :diminish ""
  :commands flycheck-mode
  :init
  (defun chasinglogic-enable-flycheck ()
    (flycheck-mode 1))

  (add-hook 'text-mode-hook 'chasinglogic-enable-flycheck)
  (add-hook 'prog-mode-hook 'chasinglogic-enable-flycheck)
  :general (leader!
             "el" 'flycheck-list-errors
             "ev" 'flycheck-verify-setup
             "ep" 'flycheck-previous-error
             "en" 'flycheck-next-error)
  :bind (("C-c e l" . flycheck-list-errors)
         ("C-c e v" . flycheck-verify-setup)
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error))
  :config
  ;; this trys to run the dash shell which I don't use but instead
  ;; opens the Dash.app program which I do use.
  (setq flycheck-sh-posix-dash-executable ""))

(provide 'chasinglogic-lint)

;;; chasinglogic-lint.el ends here
