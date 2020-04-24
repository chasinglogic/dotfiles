;;; chasinglogic-auto-complete.el --- Auto completion setup

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

(provide 'chasinglogic-auto-complete)

;;; chasinglogic-auto-complete.el ends here
