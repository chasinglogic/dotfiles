;;; chasinglogic-web.el --- Web languages support

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

;; Web Mode
;;
;; Web mode is great. It does everything other text editors can't and
;; treats tags as native source blocks of the appropriate type
;; (i.e. `script' tags get fontified and treated as if they're in a
;; Javacript mode "sub buffer", same for CSS). I don't do much web
;; programming or templating nowadays but this is configured so I can
;; effectively when required.
;;
;; The only settings here are configuring a local tab width of 2 (the
;; Javascript default) and setting up case statements to indent
;; according to eslint's desired configuration.
(use-package web-mode
  :commands (web-mode)
  :mode ("\\.html?\\'" "\\.tmpl\\'" "\\.css\\'"
         "\\.scss\\'" "\\.erb\\'" "\\.djhtml\\'"
         "\\.tsx\\'")
  :config
  (setq-default js-ident-level 2
                javascript-ident-level 2
                js2-basic-offset 2)
  (defun chasinglogic-web-mode-hook ()
    (lsp)
    ;; indent case statements
    (c-set-offset 'case-label '+))
  (add-hook 'web-mode-hook 'chasinglogic-web-mode-hook)
  (add-hook 'web-mode-hook 'lsp)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

  (setq-default web-mode-markup-indent-offset 2
                web-mode-style-indent-offset 2
                web-mode-code-indent-offset 2))

(provide 'chasinglogic-web)

;;; chasinglogic-web.el ends here
