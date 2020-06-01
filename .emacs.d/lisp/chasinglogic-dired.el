;;; chasinglogic-dired.el --- Dired settings

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
(setq dired-dwim-target t)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)

;; Various dired settings
(setq dired-auto-revert-buffer t ; Just revert on changes, don't ask me
      dired-clean-confirm-killing-deleted-buffers nil) ; Just kill buffers when I delete the file in dired. Don't ask.

(provide 'chasinglogic-dired)

;;; chasinglogic-dired.el ends here
