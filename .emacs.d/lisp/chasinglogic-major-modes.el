;;; chasinglogic-major-modes.el --- Configuration for all my major modes

;; Copyright (C) 2019 Mathew Robinson

;; Author: Mathew Robinson <mathew@chasinglogic.io>
;; Created: 25 Aug 2019

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

;; Dired
;;     I use dired as my primary file manager for anything that isn't
;;     multimedia content (videos, photos, music). I really love it and
;;     some kinds of file operations are simply not possible without it.
;;     First we require `dired-x'. Dired-X provides many extra features
;;     to Dired that take it from nice to unparalleled. See [[info:dired-x#Features][Dired-X
;;     Features]] for a full list with more info.
(require 'dired-x)

;; Now we set the variable `dired-dwim-target' to `t'. This makes it
;; such that when operating on files in Dired the target of the
;; operation will automatically suggest other Dired buffers as the
;; target preferring buffers that are visible. It's super handy.
(setq-default dired-dwim-target t)

;; Ediff
;;     Ediff is a handy tool I don't use often enough. However I really
;;     hate the default layout. This makes Ediff less eggregious about
;;     upsetting my window manager when I load it.
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; Evergreen CI integration
;;
;;     This is one of my personal packages. At MongoDB we run our in house
;;     CI system and this package integrates it into Emacs. We don't
;;     maintain an in-house ELPA repository so I recommend, and myself do,
;;     installing it with Quelpa.
(use-package evergreen
  :quelpa (evergreen :repo "evergreen-ci/evergreen.el" :fetcher github)
  :commands (evergreen-patch evergreen-list-spawn-hosts)
  :config
  (setq-default evergreen-generate-description t
                evergreen-finalize-when-patching t
                evergreen-browse-when-patching t
                evergreen-default-project "mongodb-mongo-master"
                evergreen-assume-yes t))

;; Ruby
;;
;; I don't do much writing of Ruby so I find the built in `ruby-mode'
;; pretty much adequate with one exception: automatically adding `end'
;; where needed.
;;
;; This package extends `electric-pair-mode' to handle languages like
;; Ruby where the closing pair can sometimes be a word or other
;; stranger set of symbols. In short it automatically adds `end' for
;; `if''s, `functions''s, and loops in Ruby.
(use-package ruby-electric
  :diminish ""
  :hook 'ruby-mode)

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
                  "mongo"
                  "enterprise"
                  "mongo_modules_enterprise"
                  "toolchain-builder"
                  "kernel-tools"))

  (defun chasinglogic-python-format-hook ()
    "Set up blacken-buffer on save if appropriate."
    (unless (member (projectile-project-name) chasinglogic-blacken-black-list) 
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
  :commands 'pyvenv-workon
  :after 'projectile
  :init
  (defun chasinglogic-auto-venv ()
    "Automatically setup the venv when entering a project"
    (when (file-exists-p (concat "~/.virtualenvs/" (projectile-project-name)))
      (pyvenv-workon (projectile-project-name))))
  (add-hook 'projectile-after-switch-project-hook 'chasinglogic-auto-venv))



;; Finally enable LSP mode in Python buffers and make Emacs treat
;; SCons build configuration files as python.


(add-hook 'python-mode-hook #'lsp)
;; Load SCons files as Python
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vars\\'" . python-mode))

;; TypeScript
;;
;; Nothing much to be done for TypeScript except install the major
;; mode as I don't work on it all that much.
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (add-hook typescript-mode-hook 'lsp))

;; Markdown Mode
;;
;; I really like Markdown. I obviously use Org mode whenever possible
;; but for those times when i need to write markdown this major mode
;; makes it the best editing experience I've had for markdown.
;;
;; Not much configuration is required here except to bind it to the
;; correct file extensions, make it fontify source blocks according to
;; the correct major mode, disable line numbers, and enable spell
;; checking.
(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.md\\'")
  :config
  ;; Use ndoc for exporting to HTML
  (setq-default markdown-command "pandoc")

  (defun chasinglogic-markdown-mode-hook ()
    "Disable line numbers and auto-wrap at 80 in Markdown"
    (markdown-toggle-fontify-code-block-natively)
    (display-line-numbers-mode -1)
    (flyspell-mode 1)
    (auto-fill-mode 1))

  (add-hook 'markdown-mode-hook 'chasinglogic-markdown-mode-hook))

;; Web Mode
;;
;; Web mode is great. It does everything other text editors can't and
;; treats tags as native source blocks of the appropriate type
;; (i.e. `script' tags get fontified and treated as if they're in a
;; Javacript mode "sub buffer", same for CSS). I don't do much web
;; programming or templating nowadays but this is configured so I can
;; effectively when required.
;;
;; The only settings here are configuring a local tab width of 2 (the
;; Javascript default) and setting up case statements to indent
;; according to eslint's desired configuration.
(use-package web-mode
  :commands (web-mode)
  :mode ("\\.html?\\'" "\\.tmpl\\'" "\\.css\\'"
         "\\.scss\\'" "\\.erb\\'" "\\.djhtml\\'"
         "\\.tsx\\'")
  :config
  (setq-default js-ident-level 2
                javascript-ident-level 2
                js2-basic-offset 2)
  (defun chasinglogic-web-mode-hook ()
    ;; indent case statements
    (c-set-offset 'case-label '+))
  (add-hook 'web-mode-hook 'chasinglogic-web-mode-hook)
  (add-hook 'web-mode-hook 'lsp)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

  (setq-default web-mode-markup-indent-offset 2
                web-mode-style-indent-offset 2
                web-mode-code-indent-offset 2))

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

;; Next create a C++ mode hook that makes Emacs format / indent things
;; correctly according to MongoDB's style guide. Additionally make it
;; so Flycheck will pass ~-std=c++17~ when doing syntax checking and
;; to allow `src' directory relative =#includes=. Finally make it such
;; that header files are treated as C++ and not C.
(defun chasinglogic-cpp-mode-hook ()
  "Set up various C++ tools and options."
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
  (defun chasinglogic-rust-mode-hook ()
    (setq-local compile-command "cargo clippy && cargo test"))
  (add-hook 'rust-mode-hook 'chasinglogic-rust-mode-hook)
  (add-hook 'rust-mode-hook #'lsp))

;; Miscellaneous Major Modes
;;
;; This is a list of major modes that I occasionally need and so are
;; useful to have installed but I do not configure them as I do not
;; write in these languages often or extensively.
;;
;; The following snippet just installs and attaches these modes to
;; file extensions.
(use-package vala-mode :mode ("\\.vala\\'"))
(use-package meson-mode :mode ("meson\\.build"))
(use-package powershell :mode ("\\.ps1\\'"))
(use-package groovy-mode :mode ("\\.groovy$" "\\.gradle$"))
(use-package yaml-mode :mode ("\\.yaml\\'" "\\.yml\\'" "\\.idl\\'"))
(use-package toml-mode :mode ("\\gitconfig\\'" "\\.toml\\'"))
(use-package cmake-mode :mode ("\\CMake.*txt\\'"))


(provide 'chasinglogic-major-modes)

;;; chasinglogic-major-modes.el ends here
