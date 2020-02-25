;;; chasinglogic-vc.el --- Version Control and Emacs

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

;; magit
;;
;; Magit is another of those top 5 packages. It's almost a reason to
;; use Emacs in and of itself. Here we only rebind some keys from
;; `vc-mode' based defaults to `magit' commands.
(use-package magit
  :general (leader!
             "g" '(:which-key "git")
             "gs" 'magit-status
             "gb" 'magit-blame
             "ga" 'magit-stage-file
             "gc" 'magit-commit)
  :bind (("C-x v d" . magit-diff)
         ("C-x v b" . magit-blame)
         ("C-x v l" . magit-log-current)
         ("C-x v a" . magit-stage-file)
         ("C-x v c" . magit-commit)
         ("C-x v s" . magit-status))
  :config
  (when (bound-and-true-p evil-mode)
    (use-package evil-magit
      :config
      (require 'evil-magit))

    (general-nmap
      :keymaps '(magit-mode-map magit-status-mode-map)
      "TAB"   'magit-section-toggle
      "<tab>" 'magit-section-toggle))

  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-prefer-push-default t)

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
  :general (leader!
             "gll" 'git-link
             "glc" 'git-link-commit
             "glh" 'git-link-homepage)
  :commands (git-link git-link-commit git-link-homepage)
  :config
  (setq-default
   git-link-default-branch "master"
   git-link-open-in-browser t))

(provide 'chasinglogic-vc)

;;; chasinglogic-vc.el ends here
