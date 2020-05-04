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
  (setq-default company-idle-delay 0.25
                company-minimum-prefix-length 2
                company-tooltip-limit 14
                company-tooltip-align-annotations t
                company-require-match 'never
                company-global-modes
                '(not erc-mode message-mode help-mode gud-mode eshell-mode)
                company-frontends '(company-pseudo-tooltip-frontend
                                    company-tng-frontend
                                    company-echo-metadata-frontend)

                ;; Buffer-local backends will be computed when loading a major mode, so
                ;; only specify a global default here.
                company-backends  '(company-capf company-dabbrev)

                ;; Company overrides `company-active-map' based on
                ;; `company-auto-complete-chars'; no magic please!
                company-auto-complete-chars nil

                ;; Only search the current buffer for `company-dabbrev' (a backend that
                ;; suggests text your open buffers). This prevents Company from causing
                ;; lag once you have a lot of buffers open.
                company-dabbrev-other-buffers nil
                ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
                ;; domain-specific words with particular casing.
                company-dabbrev-ignore-case nil
                company-dabbrev-downcase nil)
  (global-company-mode))

(provide 'chasinglogic-auto-complete)

;;; chasinglogic-auto-complete.el ends here
