;;; chasinglogic-projectile.el --- The project interaction package for Emacs

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

;; Projectile
;;
;; Projectile is one of my most used packages. It provides searching
;; for and searching in projects (git repositories). It's a much more
;; powerful "Ctrl-P" equivalent. I do some customization to
;; projectile. First I rebind some of the default keys in the
;; `projectile-command-map' to mnemonics that I remember better. Then
;; I bind =C-c p= to the `projectile-command-map'.
;;
;; For actual behavioral changes I create a custom switch project
;; action that opens `magit-status' in a single window view. I disable
;; projectile when the `default-directory' is not a source code
;; repository. I disable caching since I don't run on Windows and
;; native methods are pretty fast. I also set the
;; `projectile-tags-command' to none. I don't like how it tried to run
;; without being asked causing Emacs to prompt me. Finally I integrate
;; it with my "frame naming system" so that when I switch to a project
;; the frame name will the project name.
;;
;; I also on startup load all projector projects into projectile so
;; switch project will at startup show me all of my projects even if I
;; haven't visited them in Emacs yet.
(use-package projectile
  :demand
  :general (leader! "p" '(:which-key "projects" :keymap projectile-command-map))
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (
         ("M-p" . projectile-find-file)
         (:map projectile-command-map
               ("s" . counsel-projectile-rg)
               ("p" . projectile-switch-project)
               ("f" . projectile-find-file)
               ("F" . projectile-find-file-in-known-projects)
               ("d" . projectile-find-dir)
               ("b" . projectile-switch-to-buffer)))
  :init
  ;; Counsel Projectile
  ;;
  ;;      Projectile will use counsel for completion as I've set
  ;;      `projectile-completion-system' to ='ivy=. However this package
  ;;      provides some more feature rich actions in those Counsel buffers and
  ;;      so we rebind the `projectile-command-map' keys to these enhanced
  ;;      versions. Additionally I use `counsel-rg' with `counsel-projectile-rg'
  ;;      to search my projects. I use ripgrep both in and out of Emacs so
  ;;      I can keep the experience consistent and fast.
  (use-package counsel-projectile :commands 'counsel-projectile-rg)

  ;; Projector => Projectile integration
  ;;
  ;;     I maintain (what I think) is a pretty cool tool called [[https://github.com/chasinglogic/projector][Projector]]
  ;;     and this "integrates" it with projectile. Simply put it seeds
  ;;     Projectile's known project list with the list of projects that
  ;;     Projector knows about. It's really nice when on a new machine that
  ;;     has all my repositories but since I haven't visited them I can't
  ;;     quickly switch to them.
  (defun chasinglogic-add-projector-projects-to-projectile ()
    "Add projector projects to projectile."
    (interactive)
    (setq
     projectile-known-projects
     (sort 
      (delete ""
              (split-string
               (shell-command-to-string "projector list") "\n"))
      #'(lambda (a b) (< (length a) (length b))))))

  :config
  (setq-default projectile-require-project-root t
                projectile-completion-system 'ivy
                projectile-enable-caching nil
                ;; I prefer a git status when switching to a project
                projectile-switch-project-action 'magit-status
                ;; I really don't need tags
                projectile-tags-command "")
  ;; When switching projects set frame name to project name
  (defun set-frame-name-to-project ()
    (set-frame-parameter (selected-frame) 'name (projectile-project-name)))
  (add-hook 'projectile-after-switch-project-hook 'set-frame-name-to-project)
  (chasinglogic-add-projector-projects-to-projectile))

(provide 'chasinglogic-projectile)

;;; chasinglogic-projectile.el ends here
