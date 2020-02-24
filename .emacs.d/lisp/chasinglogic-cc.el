;;; chasinglogic-cc.el --- C/C++ language setup

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


;; C / C++
;;
;; I have to read more C++ than I have to write but for those times
;; when I do this configuration ensures that my code is formatted,
;; correct, and ready to commit.
;;
;; First we install the `clang-format' package and point it at the
;; MongoDB toolchain binary since it's always the right version.
(use-package clang-format
  :commands (clang-format-buffer)
  :config
  (setq clang-format-binary "/opt/mongodbtoolchain/v3/bin/clang-format"))

;; Use clang-tidy for flycheck on top of the compilation checking
;; provided by LSP. I use clangd as my language server which does
;; support clang-tidy but unfortunately it ignores the configuration
;; files
(use-package flycheck-clang-tidy)

(use-package ccls)

;; Next create a C++ mode hook that makes Emacs format / indent things
;; correctly according to MongoDB's style guide. Additionally make it
;; so Flycheck will pass ~-std=c++17~ when doing syntax checking and
;; to allow `src' directory relative =#includes=. Finally make it such
;; that header files are treated as C++ and not C.
(defun chasinglogic-cpp-mode-hook ()
  "Set up various C++ tools and options."
  (require 'ccls)
  ;; Don't indent namespaces
  (c-set-offset 'innamespace [0])
  (setq-local c-basic-offset 4)
  ;; Tell Flycheck I write modern C++ and use src-relative includes
  (setq flycheck-clang-language-standard "c++17"
        flycheck-clang-include-path (list (concat (projectile-project-root) "src")))

  ;; Auto format C/C++ buffers
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(add-hook 'c++-mode-hook 'chasinglogic-cpp-mode-hook)
(add-hook 'c-mode-hook 'chasinglogic-cpp-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'chasinglogic-cc)

;;; chasinglogic-cc.el ends here
