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
         ("C-x '" . crux-visit-term-buffer)))

;; Better terminal emulator for Emacs
;; (when module-file-suffix
;;   (use-package vterm
;;     :config
;;     (setq crux-term-buffer-name "v")
;;     (require 'crux)
;;     (defun crux-visit-term-buffer ()
;;       "Create or visit a terminal buffer. If the process in that buffer died, ask to restart."
;;       (interactive)
;;       (crux-start-or-switch-to (lambda ()
;;                                  (vterm (concat "*" crux-term-buffer-name "-term" "*")))
;;                                (format "*%s-term*" crux-term-buffer-name))
;;       (when (and (null (get-buffer-process (current-buffer)))
;;                  (y-or-n-p "The process has died.  Do you want to restart it? "))
;;         (kill-buffer-and-window)
;;         (crux-visit-term-buffer)))))

(provide 'chasinglogic-misc)

;;; chasinglogic-misc.el ends here
