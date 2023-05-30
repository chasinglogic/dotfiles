;;; chasinglogic-ivy.el --- Ivy configuration

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

;; Ivy
(use-package ivy
  :general (leader!
             "on" 'ivy-occur-next-error)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume))
  :init
  (setq ivy-always-two-windows nil
        ivy-default-display-buffer-functions '(display-buffer-in-side-window)
        ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy)

  (ivy-mode 1))

;; Better command sorting
(use-package amx
  :config (amx-mode 1))

(use-package counsel
  :general (leader!
             "<SPC>" 'counsel-M-x
             "ff" 'counsel-find-file
             "bb" 'counsel-switch-buffer
             "ji" 'counsel-imenu
             "jb" 'counsel-bookmark)
  :bind (("M-x"       . counsel-M-x)
         ("C-x b"     . counsel-switch-buffer)
         ("M-y"       . counsel-yank-pop)
         ("M-i"       . counsel-imenu)
         ("C-h f"     . counsel-describe-function)
         ("C-h v"     . counsel-describe-variable)
         ("C-x r b"   . counsel-bookmark)
         ("C-x C-f"   . counsel-find-file)
         ("C-c s o c" . counsel-org-capture)
         ("C-c s o h" . counsel-org-goto)
         ("C-c o o"   . counsel-org-goto)
         (:map minibuffer-local-map
               ("M-r" . 'counsel-minibuffer-history)))
  :init
  (setq counsel-rg-base-command
          (split-string
           (if (memq system-type '(ms-dos windows-nt))
               "rg -M 240 --with-filename --no-heading --line-number --hidden --color never %s --path-separator / ."
             "rg -M 240 --with-filename --no-heading --line-number --color never --hidden %s"))))

(provide 'chasinglogic-ivy)

;;; chasinglogic-ivy.el ends here
