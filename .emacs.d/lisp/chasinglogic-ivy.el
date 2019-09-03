;;; chasinglogic-ivy.el --- Ivy configuration

;; Copyright (C) 2019 Mathew Robinson

;; Author: Mathew Robinson <mathew@chasinglogic.io>
;; Created: 25 Aug 2019

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
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume))
  :config
  (setq ivy-always-two-windows nil)
  (setq ivy-default-display-buffer-functions '(display-buffer-in-side-window))
  (ivy-mode 1))

(use-package counsel
    :bind (("M-x"      . counsel-M-x)
           ("C-x b"    . counsel-switch-buffer)
           ("M-y"      . counsel-yank-pop)
           ("M-i"      . counsel-imenu)
           ("C-h f"    . counsel-describe-function)
           ("C-h v"    . counsel-describe-variable)
           ("C-x r b"  . counsel-bookmark)
           ("C-x C-f"  . counsel-find-file)
           (:map minibuffer-local-map
                 ("M-r" . 'counsel-minibuffer-history))))

;; Swiper
;;
;; This is an enhanced incremental search provided by Ivy.
(use-package swiper :bind ("C-M-s" . swiper))

;; Counsel Projectile
;;
;;      Projectile will use counsel for completion as I've set
;;      `projectile-completion-system' to ='ivy=. However this package
;;      provides some more feature rich actions in those Counsel buffers and
;;      so we rebind the `projectile-command-map' keys to these enhanced
;;      versions. Additionally I use `counsel-rg' with `counsel-projectile-rg'
;;      to search my projects. I use ripgrep both in and out of Emacs so
;;      I can keep the experience consistent and fast.
(use-package counsel-projectile
  :after (counsel projectile)
  :bind (:map projectile-command-map ("s" . counsel-projectile-rg)))

(provide 'chasinglogic-ivy)

;;; chasinglogic-ivy.el ends here
