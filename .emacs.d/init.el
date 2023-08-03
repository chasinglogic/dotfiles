;;; init.el --- My initialization file

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

;; Auto saves and backups
;;     Emacs has amazing auto save and backup functionality that has
;;     saved me many times after such events as an X11 crash or power
;;     loss. However, it stores all of these files in very inconvenient
;;     locations when working with version control. These configuration
;;     options inform Emacs to store these files somewhere "out of the
;;     way" (=~/.emacs.d/autosaves= and =~/.emacs.d/backups=
;;     respectively).
(when (not (file-directory-p "~/.emacs.d/backups"))
  (make-directory "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/autosaves"))

;; Next we set `backup-directory-alist'. According to the
;; documentation for this variable: "Alist of filename patterns and
;; backup directory names.  Each element looks like (REGEXP
;; . DIRECTORY).  Backups of files with names matching REGEXP will be
;; made in DIRECTORY.". So we set it such that the `REGEXP' is '.*'
;; (matches anything) and `DIRECTORY' is our new backup directory.
(setq-default backup-directory-alist `((".*" . "~/.emacs.d/backups")))

;; Now we do effectively the same for auto saves. Weirdly the
;; configuration for auto saves is slightly different from
;; backups. The documentation for this variable I found a bit opaque
;; but, it takes a list of three element lists and it essentially does
;; an Emacs `replace-regexp' on the filename with the first two
;; elements. So that the list is `REGEXP' followed by
;; `REPLACEMENT'. See [[Emacs Regular Expressions]] in my Notes
;; section for an explanation of the syntax for this. The third
;; element specifies that the transformed name should be made unique
;; in relation to the other auto saves in this directory.
(setq-default auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/\\2" t)))

;; On MacOS make the command key meta. Self-explanatory, all of my
;; muscle memory puts meta at the same location as the MacOS command
;; key so we make Emacs treat it as such instead of as super.
(when (and (display-graphic-p) (eq system-type 'darwin))
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta)

  ;; Use right alt as proper alt so special chars work on uk keyboards
  (setq ns-right-alternate-modifier 'none))

;; Add the lisp directory where all other files are required from to
;; the load path.
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp/languages"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp/emacs"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp/packages"))

;; Package initialization
;;    Before we can set up our configuration for third party packages we
;;    have to initialize the built-in Emacs package for fetching and
;;    updating them.
;;    This snippet loads =package.el= and adds the following repositories
;;    to Emacs:
;;    - `elpa': The GNU default package repository. I actually install very
;;      little from here since it tends towards being out of date.
;;    - `melpa': This is where I get almost everything else. It's a
;;      rolling up to date Emacs package repository. Maybe someday if I
;;      experience breakage I'll switch to `melpa-stable' but for years
;;      now I've never had to roll back a package (except when I was on
;;      Spacemacs because an update broke Spacemacs code).
(require 'package)
(setq-default package-archives
              (list
               '("elpa" . "https://elpa.gnu.org/packages/")
               '("melpa" . "https://melpa.org/packages/")))

;; To compile elc files as needed, in a deferred/async manner.
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; Next we setup the amazing `use-package' package. Every package,
;; other than `use-package' itself, is installed with
;; `use-package'. It's a macro that makes configuration clear,
;; concise, and most importantly fast. It makes every single package
;; lazy load as you need it (when configured properly), greatly
;; improving Emacs startup time.

;; First we set a few global configuration options for `use-package':
;; - `use-package-always-ensure': Almost all of the packages that I
;;   configure with `use-package' are third party
;;   packages. `use-package' has a feature called =:ensure= that tells
;;   `use-package' to install the package on startup if it's not
;;   installed. Since `use-package' declarations where I don't want
;;   this behavior are the exception this setting tells `use-package'
;;   to set =:ensure t= by default.
(setq-default use-package-always-ensure t)

;; Next we actually install `use-package'. We wrap this in a
;; `eval-when-compile' call since I byte compile my =init.el= it means
;; I don't pay for this installation at startup time.
(eval-when-compile
  (package-initialize)
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)

;; Diminish
;;
;; Diminish is a neat package that lets me easily hide minor modes
;; from the mode line. It also has a `use-package' keyword that lets
;; me do this for third party packages easily. Here we ensure that
;; it's available and diminish some common minor modes:
(use-package diminish
  :init
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode))

;; Keybindings

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-escape
  :config
  (evil-escape-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :config
  (evil-collection-init))

;; Default / built-in Emacs feature settings
(require 'chasinglogic-editor)

;;;; Color Theme

(use-package dracula-theme)
(use-package modus-themes)
(use-package solarized-theme)

(defvar chasinglogic-dark-theme 'dracula)
(defvar chasinglogic-light-theme 'modus-operandi)

(if (display-graphic-p)
    (load-theme chasinglogic-light-theme t)
  (load-theme chasinglogic-dark-theme t))

(defun chasinglogic-toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (if (custom-theme-enabled-p chasinglogic-dark-theme)
      (progn
        (disable-theme chasinglogic-dark-theme)
        (load-theme chasinglogic-light-theme t))
    (progn
      (disable-theme chasinglogic-dark-theme)
      (load-theme chasinglogic-dark-theme t))))

;; Work specific commands or settings
(require 'chasinglogic-work)

(require 'chasinglogic-keys)

;; Tools

;; Company Mode
;;
;; Company stands for COMPlete ANYthing and it does. I enable it
;; globally and diminish it since it is always on. I only set
;; `company-dabbrev-downcase' to nil. This ignores casing when
;; providing suggestions taken from inside the current buffer.
(use-package company
  :diminish ""
  :config
  (setq-default company-idle-delay 0.25
                company-minimum-prefix-length 2
                company-tooltip-limit 14
                company-tooltip-align-annotations t
                company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode)
                ;; company-frontends '(company-pseudo-tooltip-frontend
                ;;                     company-tng-frontend
                ;;                     company-echo-metadata-frontend)

                ;; Buffer-local backends will be computed when loading a major mode, so
                ;; only specify a global default here.
                company-backends  '(company-capf company-dabbrev)

                ;; Company overrides `company-active-map' based on
                ;; `company-auto-complete-chars'; no magic please!
                ;; company-auto-complete-chars nil

                ;; Only search the current buffer for `company-dabbrev' (a backend that
                ;; suggests text your open buffers). This prevents Company from causing
                ;; lag once you have a lot of buffers open.
                company-dabbrev-other-buffers nil

                ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
                ;; domain-specific words with particular casing.
                company-dabbrev-ignore-case nil
                company-dabbrev-downcase nil)
  (global-company-mode))

(use-package yasnippet
  :diminish 'yas-minor-mode
  :config (yas-global-mode 1))

;; Format all the things. It's better than tool-specific format
;; packages. https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :commands (format-all-buffer format-all-mode)
  :init
  (add-hook 'prog-mode-hook (lambda () (format-all-mode 1))))

(require 'chasinglogic-ivy)
(require 'chasinglogic-lint)
(require 'chasinglogic-lsp)
(require 'chasinglogic-minor-modes)
(require 'chasinglogic-projects)
(require 'chasinglogic-vc)
(require 'chasinglogic-tree-sitter)

;; Applications
(require 'chasinglogic-org)

;; Programming languages
(require 'chasinglogic-elixir)
(require 'chasinglogic-go)
(require 'chasinglogic-javascript)
(require 'chasinglogic-markdown)
(require 'chasinglogic-minor-modes)
(require 'chasinglogic-prose)
(require 'chasinglogic-python)
(require 'chasinglogic-ruby)
(require 'chasinglogic-rust)
(require 'chasinglogic-terraform)
(require 'chasinglogic-typescript)
(require 'chasinglogic-web)

;; Miscellaneous Major Modes
;;
;; This is a list of major modes that I occasionally need and so are
;; useful to have installed but I do not configure them as I do not
;; write in these languages often or extensively.
;;
;; The following snippet just installs and attaches these modes to
;; file extensions.
(use-package dockerfile-mode :mode ("\\Dockerfile\\'"))
(use-package protobuf-mode :mode ("\\.proto\\'"))
(use-package vala-mode :mode ("\\.vala\\'"))
(use-package meson-mode :mode ("meson\\.build"))
(use-package powershell :mode ("\\.ps1\\'"))
(use-package yaml-mode :mode ("\\.yaml\\'" "\\.yml\\'" "\\.idl\\'"))
(use-package toml-mode :mode ("\\gitconfig\\'" "\\.toml\\'"))
(use-package cmake-mode :mode ("\\CMake.*txt\\'"))
(use-package nginx-mode :mode ("\\.conf'"))
(use-package kubel :commands (kubel kubernetes))
(use-package js2-mode
  :config
  (setq js-indent-level 2
        )
  (add-hook 'js-mode #'(lambda ()
                         (js2-minor-mode)
                         (electric-indent-local-mode -1)
                         (electric-layout-local-mode -1))))

;; Misc
(require 'chasinglogic-misc)
(setenv "GOPATH" (concat (getenv "HOME") "/Code/go"))
;; Emacs environment variables (exec-path-from-shell)
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Post initialization
;;
;; These are the few final steps we should take when bringing up
;; Emacs.
;;
;; First Maximize this frame, the initial frame won't see our hooks in
;; `make-frame-init-functions'.
(toggle-frame-maximized)
(put 'narrow-to-region 'disabled nil)
