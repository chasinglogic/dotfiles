;;; chasinglogic-prose.el --- Emacs is a text editor for everything

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

;; Writeroom Mode
;;
;; Writeroom Mode is a simple but great package that provides a
;; focused editing experience. It removes all chrome and centers the
;; buffer on the window so you can focus only on the prose.
(use-package writeroom-mode :commands (writeroom-mode))

;; Spell Checking (Flyspell)

;; While Abbrev mode will solve my habitual spelling errors for me
;; it's still nice to have spell check on so I can catch new spelling
;; errors. This is baked into Emacs and requires the `aspell' (or
;; `ispell') program to be installed. I enable `flyspell-mode' for all
;; text buffers and use a subsequent hook for programming modes to
;; disable it and instead enable the programming variant that spell
;; checks comments instead of code.
(defun chasinglogic-enable-flyspell ()
  "Enable spell checking."
  (flyspell-mode 1))

(defun chasinglogic-disable-flyspell ()
  "Enable spell checking."
  (flyspell-mode -1))

(add-hook 'text-mode-hook 'chasinglogic-enable-flyspell)
(add-hook 'prog-mode-hook 'chasinglogic-disable-flyspell)

(provide 'chasinglogic-prose)

;;; chasinglogic-prose.el ends here
