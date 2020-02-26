;;; chasinglogic-lsp.el --- Language Server Protocol in Emacs

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

;; LSP Mode
;;
;; LSP mode attempts to make Emacs as featureful as VSCode when it
;; comes to "IDE-esque" features. I would say it gets almost all the
;; way there. However I disable a lot of these features for
;; performance or visual disruption reasons. Even with most of these
;; UI elements disabled it provides the best completion and linting of
;; any package in the Emacs ecosystem. The best part is that it's a
;; single package so I don't have to maintain a million =company-*=
;; and =flycheck-*= packages. It consists of two packages `lsp-mode'
;; itself that provides the Language Server interaction and `lsp-ui'
;; that provides the bulk of interactive features for the Language
;; Server. I only install `lsp-ui' for the Flycheck integration.
(use-package lsp-mode
  :init (setq-default lsp-auto-guess-root t
                      lsp-prefer-flymake nil)
  :commands 'lsp
  :config
  (use-package lsp-python-ms
    :demand
    :config
    (require 'lsp-python-ms)))

;; Increase the amount of data which Emacs reads from the
;; process. Again the emacs default is too low 4k considering that the
;; some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Enable LSP for all prog modes
(defun chasinglogic-enable-lsp ()
  "Enable LSP mode."
  (lsp)
  (eldoc-mode 1))

(add-hook 'prog-mode-hook 'chasinglogic-enable-lsp)

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
  :general (leader!
             "jw" 'lsp-ivy-workspace-symbol))

(provide 'chasinglogic-lsp)

;;; chasinglogic-lsp.el ends here
