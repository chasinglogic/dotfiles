;;; chasinglogic-helm.el --- Helm configuration

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

(provide 'chasinglogic-helm)

;;; chasinglogic-helm.el ends here
;; Helm
;;
;;     Helm is my current fuzzy searching framework. I've switched
;;     between Helm and Ivy often enough that it's almost worth having
;;     both. At this point and time however I think I'll stick with
;;     helm. It mostly behaves how I want, it's fast enough, and most
;;     importantly it has more third party packages that I use. I think
;;     when something like `helm-org-rifle' and `helm-mu' are available
;;     for Ivy I might switch back but for now I can't live without these
;;     and I'd rather have one way of doing fuzzy completion.
;;
;;     This `use-package' declaration for Helm does a few interesting
;;     things. First it binds common key chords to their improved helm
;;     equivalents. Next it defines two variables that when combined make
;;     helm display at the bottom of the frame full width. Kind of like
;;     the minibuffer but not. I prefer this positioning but I have no
;;     idea how these variables accomplish it I [[https://github.com/emacs-helm/helm/issues/2039#issuecomment-390103697][stole this configuration
;;     from this random comment on the Helm issue tracker]]. Finally
;;     enables the helm minor mode that just makes helm kind of work with
;;     everything even if it doesn't have an explicit helm integration.
(use-package helm
  :bind (("M-x"      . helm-M-x)
         ("C-x b"    . helm-mini)
         ("M-y"      . helm-show-kill-ring)
         ("M-i"      . helm-imenu)
         ("M-I"      . helm-imenu-in-all-buffers)
         ("C-x r b"  . helm-bookmarks)
         ("C-x C-f"  . helm-find-files))
  :config
  (setq helm-always-two-windows nil)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (helm-mode 1))

;; Helm Swoop
;;
;;      There were two main killer features, to me, of Ivy. One of them
;;      was Swiper, the enhanced incremental search. This package
;;      `helm-swoop' kind of fills that gap. It's less nice than swiper
;;      (it breaks my helm window configuration rules) but it gets the job
;;      done. I bind it to `C-M-s' globally.
(use-package helm-swoop
  :after helm
  :bind ("C-M-s" . helm-swoop))

;; Helm mu
;;
;;      This is my favorite thing about Helm. I use the excellent mu4e
;;      package and mu tool to manage my Email locally. This lets me
;;      incrementally search my email using the mu query language and
;;      interact with the results.
(use-package helm-mu
  :after (helm mu4e)
  :hook (mu4e-main-mode-hook .
                             (lambda ()
                               (bind-key "s" helm-mu mu4e-main-mode-map)
                               (bind-key "s" helm-mu mu4e-headers-mode-map)
                               (bind-key "s" helm-mu mu4e-view-mode-map)))
  :bind (("C-c s m" . helm-mu)
         ("C-c s c" . helm-mu-contacts)))

;; Helm Projectile
;;
;;      Projectile will use helm for completion as I've set
;;      `projectile-completion-system' to ='helm=. However this package
;;      provides some more feature rich actions in those Helm buffers and
;;      so we rebind the `projectile-command-map' keys to these enhanced
;;      versions. Additionally I use `helm-rg' with `helm-projectile-rg'
;;      to search my projects. I use ripgrep both in and out of Emacs so
;;      I can keep the experience consistent and fast.
(use-package helm-projectile
  :after (helm projectile)
  :bind (:map projectile-command-map
              ("h" . helm-projectile-find-other-file)
              ("f" . helm-projectile-find-file)
              ("p" . helm-projectile-switch-project)
              ("s" . helm-projectile-rg)))
(use-package helm-rg :after 'helm-projectile)

;; Helm Org
;;
;;      This package adds helm sources for org mode buffers. You can
;;      search through in buffer headers, capture templates, and more. I
;;      only bind the most commonly used commands however.
(use-package helm-org
  :after (helm org)
  :bind (("C-c s o c" . helm-org-capture-templates)
         ("C-c s o h" . helm-org-in-buffer-headings)
         ("C-c o o"   . helm-org-in-buffer-headings)))

;; Helm Org Rifle
;;
;;      While `helm-org' integrates Helm with org mode buffer
;;      editing. `helm-org-rifle' is something all together more
;;      powerful. It does entry based searching for terms across all open
;;      Org mode buffers. It can do the same for files, directories, and
;;      the agenda. It's exteremely powerful and I don't use it
;;      enough. But when I do need it, for instance trying to find a
;;      note, it's irreplaceable.
(use-package helm-org-rifle
  :after (helm org)
  :bind  ("C-c s o r" . helm-org-rifle))
