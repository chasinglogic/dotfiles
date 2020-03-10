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

(eval-and-compile
  ;; Font
  ;;     First set the font. I've tried many fonts in my time and I find
  ;;     Source Code Pro to be a Pretty Good Font™. Other fonts I like are
  ;;     Inconsolata and DejaVu Sans Mono, and one day I may switch back to
  ;;     them but getting them on all platforms can be a hassle.
  ;;     The only thing fancy about the way this font is getting set is that
  ;;     I use two font sizes: one for my Mac because of the retina display
  ;;     and one for everything else where I use regular monitors.
  (setq-default chasinglogic-font-size "10")
  (when (and (display-graphic-p) (eq system-type 'darwin))
    ;; Retina display requires bigger font IMO.
    (setq chasinglogic-font-size "15"))

  ;; Faster than set-frame-font
  (setq chasinglogic-font (format "Fira Code-%s" chasinglogic-font-size))
  (add-to-list 'default-frame-alist (cons 'font chasinglogic-font))

  ;; Window Chrome
  ;;     Emacs by default has lots of window chrome to make it more mouse
  ;;     accessible. While I actually use my mouse quite a bit and love
  ;;     Emacs mouse integration I really hate big UI elements and I never
  ;;     use the mouse for the operations available in this chrome. These
  ;;     mode disable lines remove all of this chrome so it's just Me, My
  ;;     Buffer, and I.
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when (display-graphic-p)
    (scroll-bar-mode -1))

  ;; On MacOS there's a new feature to have title bars match the window
  ;; they belong to. This makes Emacs do that so the title bar looks
  ;; like it's part of the buffer.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

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
      (when (display-graphic-p)
        (set-frame-parameter nil 'fullscreen 'maximized))
      (when (not display-graphic-p)
        (disable-theme 'doom-solarized-light))))
  (add-hook 'after-make-frame-functions 'maximize-gui-frames)

  ;; When the shell exits close the buffer and window
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (if (> (length (window-list)) 1)
        (kill-buffer-and-window)
      (kill-buffer)))
  ) ;; end eval-and-compile

;; Color Theme
;;
;;   I change this too often to really document why whatever
;;   theme I'm in the mood for is the one I'm in the mood for.
(use-package doom-themes
  :config
  (load-theme 'doom-palenight t)
  (doom-themes-org-config))

(use-package doom-modeline
  :config
  (doom-modeline-mode))

(provide 'chasinglogic-editor)

;;; chasinglogic-editor.el ends here
