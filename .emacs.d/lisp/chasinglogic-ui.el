;;; chasinglogic-ui.el --- UI / UX configuration for Emacs

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

;; Anything related to how pretty Emacs looks goes here. (font, color
;; scheme, etc.)

;;; Code:

;; Font
;;     First set the font. I've tried many fonts in my time and I find
;;     Source Code Pro to be a Pretty Good Font™. Other fonts I like are
;;     Inconsolata and DejaVu Sans Mono, and one day I may switch back to
;;     them but getting them on all platforms can be a hassle.
;;     The only thing fancy about the way this font is getting set is that
;;     I use two font sizes: one for my Mac because of the retina display
;;     and one for everything else where I use regular monitors.
(setq-default chasinglogic-font-size "13")
(when (and (display-graphic-p) (eq system-type 'darwin))
  ;; Retina display requires bigger font IMO.
  (setq chasinglogic-font-size "15"))
(set-frame-font (format "Hack %s" chasinglogic-font-size) nil t)

;; Window Chrome
;;     Emacs by default has lots of window chrome to make it more mouse
;;     accessible. While I actually use my mouse quite a bit and love
;;     Emacs mouse integration I really hate big UI elements and I never
;;     use the mouse for the operations available in this chrome. These
;;     mode disable lines remove all of this chrome so it's just Me, My
;;     Buffer, and I.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; On MacOS there's a new feature to have title bars match the window
;; they belong to. This makes Emacs do that so the title bar looks
;; like it's part of the buffer.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Color Theme
;;
;;   I change this too often to really document why whatever
;;   theme I'm in the mood for is the one I'm in the mood for.
;; (use-package zenburn-theme :config (load-theme 'zenburn t))
(use-package solarized-theme
  :config
  (setq-default solarized-distinct-fringe-background t)
  (load-theme 'solarized-light t))

;; Line numbers in programming modes.
;;     I enable line numbers using the new Emacs 26
;;     `display-line-numbers-mode' for all programming major modes.
(defun enable-display-line-numbers-mode ()
  "Enable display-line-numbers-mode"
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'enable-display-line-numbers-mode)


;; Automatically maximize Emacs frames when they are created
;;     This is a custom function I wrote that maximizes the frame it's
;;     passed. I then hook it into the `after-make-frame-functions' hook
;;     so any time a frame is created it is maximized.
(defun maximize-gui-frames (frame)
  "Maxmize a the GUI frame FRAME."
  (with-selected-frame frame
    (when (display-graphic-p)
      (set-frame-parameter nil 'fullscreen 'maximized))))
(add-hook 'after-make-frame-functions 'maximize-gui-frames)

(use-package all-the-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))


(provide 'chasinglogic-ui)
;;; chasinglogic-ui.el ends here
