;;; chasinglogic-misc.el --- Utility and other packages that just don't fit anywhere else

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

(use-package crux
  :general (leader!
             "fS" 'crux-sudo-edit
             "fD" 'crux-delete-buffer-and-file
             "fr" 'crux-rename-buffer-and-file
             "x" '(lambda () (interactive) (ansi-term (executable-find "bash")))
             "'" 'crux-visit-term-buffer)
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("C-c f D" . crux-delete-buffer-and-file)
         ("C-c f r" . crux-rename-buffer-and-file)
         ("C-c '" . crux-visit-term-buffer)
         ("C-x '" . crux-visit-term-buffer)))

;; Better terminal emulator for Emacs
(when module-file-suffix
  (use-package vterm
    :commands 'vterm
    :init
    (defun crux-vterm (buffer-name)
      (vterm (format "*%s*" buffer-name)))
    (setq crux-term-func #'crux-vterm)))

(use-package multi-line
  :config (setq-default multi-line-current-strategy
                        (multi-line-strategy
                         :respace (multi-line-default-respacers
                                   (make-instance multi-line-always-newline))))
  :bind ("C-;" . multi-line))


(use-package ace-window
  :bind ("M-o" . ace-window))

;; Highlight TODO mode
;;
;; By default Emacs doesn't highlight TODO comments. This makes them
;; stand out by fontifying them the same as Org mode TODO header
;; keywords.
(use-package hl-todo
  :demand
  :config
  (global-hl-todo-mode))

;; anzu-mode enhances isearch & query-replace by showing total matches
;; and current match position, it also does the smartest
;; case-insensitive replacing you've ever seen.
(use-package anzu
  :diminish ""
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace))
  :config
  (global-anzu-mode)
  (use-package evil-anzu
    :after 'evil))

(provide 'chasinglogic-misc)

;;; chasinglogic-misc.el ends here
