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

;; https://github.com/emacs-lsp/lsp-mode/issues/4054#issuecomment-1558173037
(add-to-list 'image-types 'svg)

;; LSP Mode
(use-package eglot
  :commands (eglot eglot-ensure))

;; Increase the amount of data which Emacs reads from the
;; process. Again the emacs default is too low 4k considering that the
;; some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* (* 1024 1024) 3)) ;; 3mb
;; Garbage collect less often
(setq gc-cons-threshold 100000000)

;; Enable LSP hook
(defun chasinglogic-enable-lsp ()
  "Enable LSP mode."
  (eglot-ensure)
  (flymake-mode 1)
  (eldoc-mode 1))

(provide 'chasinglogic-lsp)

;;; chasinglogic-lsp.el ends here
