;;; chasinglogic-util-packages.el --- Packages which provide utility commands

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

;; Expand Region
;;
;;     Expand region takes the idea of, what I consider, one of the best
;;     key bindings in Emacs `M-h' (`mark-paragraph') and makes it work
;;     incrementally with semantic units. It's beautiful and useful. For
;;     consistency I bind it to `C-M-h'.
(use-package expand-region
  :bind ("C-M-h" . expand-region))

;; Avy
;;
;;     The legendary `abo-abo' wrote a great package called Avy. I can
;;     only compare it to EasyMotion for Vim but it's actually much
;;     better IMO. It has many "jumping" commands for going to words,
;;     subwords, characters, lines etc. Here I only bind a few of the
;;     most useful ones to me.
(use-package avy
  :bind
  (("M-j"     . 'avy-goto-word-1)
   ("C-c j c" . 'avy-goto-char)
   ("C-c j j" . 'avy-goto-word-1)
   ("C-c j l" . 'avy-goto-line)
   ("C-c j h" . 'avy-goto-heading))
  :config
  ;; When non-nil, a gray background will be added during the selection.
  (setq avy-background t)
  )

;; Ace Window
;;
;;     One of the hardest parts coming to Emacs from Vim was learning
;;     window management. The default keybinding =C-x o= felt cumbersome
;;     to press not to mention use. Luckily there is a package (again
;;     from `abo-abo') that solves this problem. Ace Window will
;;     highlight windows with a number and let you jump to them by
;;     pressing the corresponding number. It's also smart and when there
;;     are only two windows will simply cycle between them without
;;     prompting. I bind it to `M-o' as the original command bound to
;;     that key I never use and I prefer meta bindings for commonly
;;     pressed commands.
(use-package ace-window
  :commands 'ace-window
  :init (defun chasinglogic-ace-window (arg)
          "Create a window if only one window and/or call ace-window"
          (interactive "p")
          (when (eq (length (window-list)) 1)
            (split-window-horizontally))
          (ace-window arg))
  :config (setq aw-scope 'frame)
  :bind (("M-o" . chasinglogic-ace-window)
         (:map term-raw-map
               ("M-o" . ace-window))))

;; magit
;;
;; Magit is another of those top 5 packages. It's almost a reason to
;; use Emacs in and of itself. Here we only rebind some keys from
;; `vc-mode' based defaults to `magit' commands.
(use-package magit
  :bind (("C-x v d" . magit-diff)
         ("C-x v b" . magit-blame)
         ("C-x v l" . magit-log-current)
         ("C-x v a" . magit-stage-file)
         ("C-x v c" . magit-commit)
         ("C-x v s" . magit-status))
  :commands 'magit-status)

;; xgen-cru (MongoDB Code Reviews)
;;
;; `xgen-cru' is an internal tool for posting code reviews to
;; Rietveld. It's an Emacs wrapper around our Python script that most
;; people use. I keep it in a directory called `kernel-tools' and I
;; use `use-package' to load it from this local directory. I only set
;; a few options so it will pass my work email to the script.
(eval-and-compile
  (setq-default kernel-tools (concat (getenv "HOME") "/Work/kernel-tools/codereview")))
(use-package xgen-cru
  :load-path kernel-tools
  :commands (xgen-cru-update-review xgen-cru-post-review)
  :config
  (setq-default
   xgen-cru-upload-email "mathew.robinson@mongodb.com"
   xgen-cru-jira-username "mathew.robinson"
   xgen-cru-upload-py-path (concat kernel-tools "/upload.py")))


;; Git Link
;;
;; This packages opens and creates github links from within
;; Emacs. It's super handy for linking someone to a line in the code
;; base from Emacs. The few settings here make it link to the master
;; branch, otherwise it would try to use my local checked out branch,
;; and to open the link in my browser so I can verify the link before
;; sending to someone.
(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :config
  (setq-default
   git-link-default-branch "master"
   git-link-open-in-browser t))

;; Yasnippet
;;
;; Yasnippet is definitely in my top 5 packages. It's the most
;; powerful and simple snippet system I've ever used. You can program
;; snippets with elisp to generate code or you can write simple
;; TextMate style snippets that just define tab stops. No
;; configuration required on this one just type a snippet identifier
;; and press tab.
(use-package yasnippet
  :diminish 'yas-minor-mode
  :config (yas-global-mode 1))

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
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ((:map projectile-command-map
               ("p" . projectile-switch-project)
               ("f" . projectile-find-file)
               ("F" . projectile-find-file-in-known-projects)
               ("d" . projectile-find-dir)
               ("b" . projectile-switch-to-buffer)))
  :init
  
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
     (delete ""
             (split-string
              (shell-command-to-string "projector list") "\n"))))
  (chasinglogic-add-projector-projects-to-projectile)
  
  :config
  (defun chasinglogic-switch-project-action ()
    "Single view magit status page when switching projects."
    (interactive)
    (magit-status)
    (delete-other-windows))

  (setq-default projectile-require-project-root t
                projectile-completion-system 'ivy
                projectile-enable-caching nil
                ;; I prefer a git status when switching to a project
                projectile-switch-project-action 'chasinglogic-switch-project-action
                ;; I really don't need tags
                projectile-tags-command "")
  ;; When switching projects set frame name to project name
  (defun set-frame-name-to-project ()
    (set-frame-parameter (selected-frame) 'name (projectile-project-name)))
  (add-hook 'projectile-after-switch-project-hook 'set-frame-name-to-project))


(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("C-c f D" . crux-delete-buffer-and-file)
         ("C-c f r" . crux-rename-buffer-and-file)
         ("C-x '" . crux-visit-term-buffer)))

(provide 'chasinglogic-util-packages)

;;; chasinglogic-util-packages.el ends here
