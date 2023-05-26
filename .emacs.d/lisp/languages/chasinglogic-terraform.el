;;; chasinglogic-python.el --- Python language setup

;; Copyright (C) 2023 Mathew Robinson

;; Author: Mathew Robinson <mathew@chasinglogic.io>
;; Created: 22 May 2023

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

(use-package terraform-mode
  :mode ("\\.tf\\'")
  :init
  (defun chasinglogic-tf-hook ()
    (add-hook 'before-save-hook 'terraform-format-buffer))
  (add-hook 'terraform-mode-hook 'chasinglogic-tf-hook)
  (add-hook 'terraform-mode-hook 'chasinglogic-enable-lsp))

(provide 'chasinglogic-terraform)
