;;; chasinglogic-editor.el --- Editor and display settings

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

;; Font
;;     First set the font. I've tried many fonts in my time and I find
;;     Source Code Pro to be a Pretty Good Font™. Other fonts I like are
;;     Inconsolata and DejaVu Sans Mono, and one day I may switch back to
;;     them but getting them on all platforms can be a hassle.
;;     The only thing fancy about the way this font is getting set is that
;;     I use two font sizes: one for my Mac because of the retina display
;;     and one for everything else where I use regular monitors.
(setq-default
 ;; Lockfiles trip up auto reloading compilers like react-scripts
 create-lockfiles nil
;; By default when Emacs tries to open a symlink that points to a git
;; repository it prompts you like "do you really wanna open this
;; file". I use symlinks like this a lot so I disable this prompt.
 vc-follow-symlinks t
 ;; Use spaces instead of tabs
 indent-tabs-mode nil
 ;; default tab size to 4 spaces
 tab-width 4
 chasinglogic-font-size (if (eq system-type 'darwin) "15" "11")
 chasinglogic-font (format "Fira Code-%s" chasinglogic-font-size)
 ;; Just save buffers before compiling
 compilation-ask-about-save nil
 ;; Always kill old compilation without prompting
 compilation-always-kill t
 ;; I use =M-x compile= for running all kinds of commands. This
 ;; setting makes it so that the buffer auto scrolls to keep up with
 ;; the output. More like a regular terminal would.
 compilation-scroll-output t
 ;; hippie expand is dabbrev expand on steroids
 hippie-expand-try-functions-list '(try-expand-dabbrev
                                    try-expand-dabbrev-all-buffers
                                    try-expand-dabbrev-from-kill
                                    try-complete-file-name-partially
                                    try-complete-file-name
                                    try-expand-all-abbrevs
                                    try-expand-list
                                    try-expand-line
                                    try-complete-lisp-symbol-partially
                                    try-complete-lisp-symbol)
 ;; Don't pollute init.el with custom settings
 custom-file "~/.local/share/emacs/custom.el"
 ;; Ediff is a handy tool I don't use often enough. However I really
 ;; hate the default layout. This makes Ediff less eggregious about
 ;; upsetting my window manager when I load it.
 ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; Window Chrome
;;     Emacs by default has lots of window chrome to make it more mouse
;;     accessible. While I actually use my mouse quite a bit and love
;;     Emacs mouse integration I really hate big UI elements and I never
;;     use the mouse for the operations available in this chrome. These
;;     mode disable lines remove all of this chrome so it's just Me, My
;;     Buffer, and I.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (use-package doom-modeline :config (doom-modeline-mode))

(add-to-list 'default-frame-alist (cons 'font chasinglogic-font))
;; On MacOS there's a new feature to have title bars match the window
;; they belong to. This makes Emacs do that so the title bar looks
;; like it's part of the buffer.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


;;;; Advices

;; When the shell exits close the buffer and window
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (if (> (length (window-list)) 1)
      (kill-buffer-and-window)
    (kill-buffer)))

(defadvice tab-bar-close-tab
    (after remove-tab-bar-on-last-closed activate)
  (when (= (length (tab-bar-tabs)) 1)
    (tab-bar-mode -1)))

;;;; Hooks

;; Line numbers in programming modes.
;;     I enable line numbers using the new Emacs 26
;;     `display-line-numbers-mode' for all text major modes.
(defun enable-display-line-numbers-mode ()
  "Enable display-line-numbers-mode"
  (display-line-numbers-mode 1))

(add-hook 'text-mode-hook 'enable-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'enable-display-line-numbers-mode)
(add-hook 'org-mode-hook '(lambda () (display-line-numbers-mode -1)))

;; Automatically maximize Emacs frames when they are created
;;     This is a custom function I wrote that maximizes the frame it's
;;     passed. I then hook it into the `after-make-frame-functions' hook
;;     so any time a frame is created it is maximized.
(defun maximize-gui-frames (frame)
  "Maxmize a the GUI frame FRAME."
  (with-selected-frame frame
    (set-frame-parameter nil 'fullscreen 'maximized)))
(add-hook 'after-make-frame-functions 'maximize-gui-frames)

;; Attempt to display ANSI colors in compilation buffer, at the very
;; least sanitize them out.
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

;;;; Misc

;; One of the best features of Emacs is it's ability to integrate
;; with programming languages at a syntactic level. It enables you to
;; really edit these languages at that level in some
;; cases. One of the common tasks that it can automate is commenting
;; and uncommenting text in a source file. Unfortunately the default
;; function for this `comment-dwim' assumes that if you have no
;; region you want to insert a line comment. I rarely if ever use
;; line comments and would prefer it to instead comment out the
;; current line if no region is selected so I wrote
;; `comment-actually-dwim' that does this and overwrite the default
;; `comment-dwim' keybinding with my version.
(defun comment-actually-dwim (arg)
  "A simpler and more functional version of `comment-dwim'. It
simply calls `comment-or-uncomment-region' with the current line
or the active region.

The complexity in the original `comment-dwim' comes from trying
to manage comments at the end of lines. I rarely do on line
comments so this function better suits my needs."
  (interactive "*P")
  (comment-normalize-vars)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; As I discover commands that have the "new user warnings" when I use
;; them I disable them here.
(put 'downcase-region 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)


;; Ensure a few important paths are always present
(mapc 
 (lambda (path)
   (add-to-list 'exec-path path)
   (setenv "PATH" (concat (getenv "PATH") ":" path)))
 (list
  (concat (getenv "HOME") "/.local/bin")
  (concat (getenv "HOME") "/.cargo/bin")))

;;;; Color Theme

(use-package modus-vivendi-theme)
(use-package modus-operandi-theme)

(defvar chasinglogic-dark-theme 'modus-vivendi)
(defvar chasinglogic-light-theme 'modus-operandi)

(load-theme chasinglogic-light-theme t)

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

;; Note that ‘uniquify’ is builtin. This configures how buffers with
;; the same name are made unique.
(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style

(provide 'chasinglogic-editor)

;;; chasinglogic-editor.el ends here
