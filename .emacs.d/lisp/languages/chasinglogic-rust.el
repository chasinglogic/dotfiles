;;; chasinglogic-rust.el --- Rust language setup

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

;; Rust is my go to programming language outside of work. It has
;; excellent Emacs support but most of the features I need are
;; actually provided by LSP mode. This simply installs and attaches
;; the Rust major mode to =.rs= files, enables format on save, and
;; sets a better default compile command. Finally it loads `lsp' on
;; `rust-mode' startup.
(use-package rust-mode
  :mode ("\\.rs\\'")
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'chasinglogic-enable-lsp))

(provide 'chasinglogic-rust)

;;; chasinglogic-rust.el ends here
