;;; chasinglogic-minor-modes.el --- Configuration relating to various minor modes

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

;; Abbrev mode is a simple but magical minor mode. I make some
;; spelling mistakes all the time. At this point some of them have
;; become muscle memory and so while I know the spelling is wrong I
;; don't know if I'll ever be able to change them. This is where
;; Abbrev mode comes in. I register abbreviations on a major mode or
;; global basis and `abbrev-mode' will automatically expand them to
;; the correction whenever I type them.
(add-hook 'text-mode-hook 'abbrev-mode)

;; Spell Checking (Flyspell)

;; While Abbrev mode will solve my habitual spelling errors for me
;; it's still nice to have spell check on so I can catch new spelling
;; errors. This is baked into Emacs and requires the `aspell' (or
;; `ispell') program to be installed. I enable `flyspell-mode' for all
;; text buffers and use a subsequent hook for programming modes to
;; disable it and instead enable the programming variant that spell
;; checks comments instead of code.
(defun chasinglogic-enable-flyspell ()
  "Enable spell checking."
  (flyspell-mode 1))

(defun chasinglogic-enable-flyspell-prog ()
  "Enable spell checking."
  (flyspell-mode -1)
  (flyspell-prog-mode))

(add-hook 'text-mode-hook 'chasinglogic-enable-flyspell)
(add-hook 'prog-mode-hook 'chasinglogic-enable-flyspell-prog)

;; Automatically Do important programming stuff Emacs has a series of
;; modes that I call the "electric modes", as they all start with
;; `electric-'. All of these modes perform important editing functions
;; automatically.
;;
;; Electric indent mode on-the-fly reindents your code as you type. It
;; checks for newlines and other common chars that are configured via
;; the variable `electric-indent-chars'. This mode is invaluable and
;; saves me a lot of formatting time.
(electric-indent-mode 1)


;; Electric layout mode automatically inserts newlines around some
;; characters. The variable `electric-layout-rules' defines when and
;; how to insert newlines. The short of it is for many modes this auto
;; formats code.
(electric-layout-mode 1)

;; Electric pair mode automatically pairs common programming
;; operators: =(=, ={=, ="=, ='=, etc. I find this behavior annoying
;; in prose modes so I use a custom hook to only enable it for
;; programming modes.


(defun enable-electric-pair-local-mode ()
  "Enable eletric pair mode locally."
  (electric-pair-local-mode 1))
(add-hook 'prog-mode-hook 'enable-electric-pair-local-mode)

;; Show Paren Mode I'm just going to steal the description of this
;; straight from the documentation: Toggle visualization of matching
;; parens (Show Paren mode).
(show-paren-mode 1)


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


;; Which Key
;;
;; Which key is possibly the best package ever invented, except for
;; maybe helm. When pressing a key chord it will show you all possible
;; bindings and prefixes so you can interactively explore key bindings
;; as you type them. It's nothing short of amazing and a great
;; discovery tool. No real configuration is needed except that I do
;; diminish it since I always have it on globally.
(use-package which-key
  :diminish ""
  :init
  (which-key-mode))

;; Paredit
;;
;; When talking about editing languages at a syntactic level one can't
;; help but think of paredit. It's simply the best way to write lisp
;; code. It adds two important to remember key bindings:
;;
;; | Key Bindings | Descripition |
;; |-------------------+---------------------------| C-M-<Right Arrow>
;; || Barf current expression | C-M-<Left Arrow> | Slurp current
;; |expression |
;;
;; Additionally I overwrite one mapping so that Paredit uses my
;; preferred `comment-actually-dwim' function instead of it's own that
;; mostly mimics the Emacs default.
(use-package paredit
  :bind (:map paredit-mode-map
              ;; I do not like any version of the original
              ;; `comment-dwim' and paredit has it's own special
              ;; version that I find more confusing. So overwrite it's
              ;; mapping with my `comment-actually-dwim' function.
              ("M-;" . comment-actually-dwim))
  :hook '(emacs-lisp-mode . paredit-mode))

;; Company Mode
;;
;; Company stands for COMPlete ANYthing and it does. I enable it
;; globally and diminish it since it is always on. I only set
;; `company-dabbrev-downcase' to nil. This ignores casing when
;; providing suggestions taken from inside the current buffer.
(use-package company
  :diminish ""
  :config
  (setq-default company-dabbrev-downcase nil)
  (global-company-mode))

;; LSP Mode
;;
;; LSP mode attempts to make Emacs as featureful as VSCode when it
;; comes to "IDE-esque" features. I would say it gets almost all the
;; way there. However I disable a lot of these features for
;; performance or visual disruption reasons. Even with most of these
;; UI elements disabled it provies the best completion and linting of
;; any package in the Emacs ecosystem. The best part is that it's a
;; single package so I don't have to maintain a milling =company-*=
;; and =flycheck-*= packages. It consists of two packages `lsp-mode'
;; itself that provides the Language Server interaction and `lsp-ui'
;; that provides the bulk of interactive features for the Language
;; Server. I only install `lsp-ui' for the Flycheck integration.
(use-package lsp-mode
  :init (setq-default lsp-auto-guess-root t
                      lsp-prefer-flymake nil)
  :commands 'lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq-default
   lsp-ui-doc-enable nil
   lsp-ui-peek-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-imenu-enable nil
   lsp-ui-flycheck-enable t))

;; LSP powered auto completion
;;
;; We need one more package to integrate LSP mode with my completion
;; framework Company, the cleverly named, `company-lsp'. All that we
;; need to do is add it to company backends.
(use-package company-lsp
  :config (push 'company-lsp company-backends)
  :after (lsp-mode company))

;; CCLS support
;;
;; I've found CCLS to be the best and most available language server
;; for C / C++. For LSP to use it however it requires the additional
;; third party package `ccls'. So here we install it and hook in
;; enabling `lsp' for all C / C++ modes.
(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

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
  :bind (("C-c e l" . flycheck-list-errors)
         ("C-c e v" . flycheck-verify-setup)
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error))
  :hook 'text-mode-hook
  :config
  ;; this trys to run the dash shell which I don't use but instead
  ;; opens the Dash.app program which I do use.
  (setq flycheck-sh-posix-dash-executable ""))

;; Flycheck Vale
;;
;; The only additional Flycheck linter package I install is Flycheck
;; Vale. This integrates the awesome
;; [[https://github.com/errata-ai/vale][Vale prose linter]] with
;; Flycheck. I use this for all my prose.
(use-package flycheck-vale
  :after 'flycheck
  :config
  (flycheck-vale-setup))

;; Highlight TODO mode
;;
;; By default Emacs doesn't highlight TODO comments. This makes them
;; stand out by fontifying them the same as Org mode TODO header
;; keywords.
(use-package hl-todo
  :demand
  :config
  (global-hl-todo-mode))

;; Writeroom Mode
;;
;; Writeroom Mode is a simple but great package that provides a
;; focused editing experience. It removes all chrome and centers the
;; buffer on the window so you can focus only on the prose.
(use-package writeroom-mode :commands (writeroom-mode))


;; anzu-mode enhances isearch & query-replace by showing total matches
;; and current match position
(use-package anzu
  :diminish ""
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace))
  :config (global-anzu-mode))

(provide 'chasinglogic-minor-modes)

;;; chasinglogic-minor-modes.el ends here
