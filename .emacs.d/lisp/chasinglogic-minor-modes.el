;;; chasinglogic-minor-modes.el --- Minor modes

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
;; and current match position
(use-package anzu
  :diminish ""
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace))
  :config (global-anzu-mode)

  (use-package evil-anzu
    :after 'evil
    ))


(provide 'chasinglogic-minor-modes)

;;; chasinglogic-minor-modes.el ends here
