;;; chasinglogic-org.el --- Org mode config

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

;; Org
;;
;; Ah Org mode. I use Org mode to save my brain from the stress of
;; being a brain. I store everything I know and need to do in Org mode
;; one way or another. I write all of my talks and blog posts in Org
;; mode. For that reason my configuration for Org mode is
;; huge. Because it's so big I've split it into sections.

;; First let's configure Org `use-package' and start the Org
;; configuration block. Additionally we'll setup autoloading commands
;; `org-capture' and `org-agenda'. I usually call these before I've
;; opened an org file.
(use-package org
  :ensure nil
  :commands (org-capture org-agenda)
  :mode ("\\.org\\'" . org-mode)
  ;; Key Bindings I have some pretty extensive org related key
  ;; bindings. Some of them have org mode defaults that I learned
  ;; after I developed muscle memory for these bindings. There isn't
  ;; anything special about these bindings they are self explanatory
  ;; based on command names. The last thing we do here is start the
  ;; =:config= section of the `use-package' definition.
  :general (leader!
             "o" '(:which-key "org")
             "oa" 'org-agenda
             "oc" 'org-capture
             "or" 'org-archive-subtree
             "om" '(:wk "misc")
             "ot" 'org-todo
             "os" 'org-schedule
             "og" 'org-set-tags-command
             "op" 'org-set-property-and-value
             "oi" '(:wk "insert")
             "oil" 'org-insert-link
             "oih" 'org-insert-heading
             "op" '(:wk "priority")
             "opp" 'org-priority)
  :bind (
         ("C-c o TAB" . org-global-cycle)
         ("C-c o a"   . org-agenda)
         ("C-c o c"   . org-capture)
         ("C-c o r"   . org-archive-subtree)
         ("C-c o t"   . org-todo)
         ("C-c o s"   . org-schedule)
         ("C-c o g"   . org-set-tags-command)
         ("C-c o P"   . org-set-property-and-value)
         ("C-c o i l" . org-insert-link)
         ("C-c o i h" . org-insert-heading)
         ("C-c o p p" . org-priority)
         ("C-c o p k" . org-priority-up)
         ("C-c o p j" . org-priority-down))
  :init
  ;; Org Mode Settings
  ;;
  ;; These settings are global variables that inform Org mode
  ;; functions or behavior.
  ;;
  ;; First we define global variables that describe where common Org
  ;; mode files can be found. I keep all of my Org files in
  ;; =~/Nextcloud/Org= so they are automatically synced to my
  ;; Nextcloud server by my clients.
  (setq-default org-directory (file-name-as-directory "~/Org")
                org-default-todo-file  (expand-file-name "inbox.org"  org-directory)
                org-default-notes-file (expand-file-name "notes.org.gpg" org-directory)
                org-default-ideas-file (expand-file-name "inbox.org" org-directory))

  :config

  ;; Org Refile
  ;;
  ;; I use org refile to organize tasks from my =inbox.org= file to my
  ;; agenda files or notes. I also use it to refile my notes between
  ;; headings in =notes.org.gpg=. These settings do the following
  ;; things:
  ;;
  ;;  - Add org agenda files to the refile targets.
  ;;  - Include the filename in the refile target path, this allows
  ;;    creating new top level headings in files via refile.
  ;;  - Enable creating new nodes via refile.
  ;;  - Disable complete-in-steps and let helm do the filtering.
  (setq-default org-refile-targets '((nil :maxlevel . 1)
                                     (org-agenda-files :maxlevel . 2))
                org-refile-use-outline-path 'file
                org-outline-path-complete-in-steps nil
                org-refile-allow-creating-parent-nodes 'confirm)

  ;; Org Mode Hooks
  ;;
  ;; This sets my org mode hook that disables
  ;; `display-line-numbers-mode' and `electric-pair-local-mode'.
  (defun chasinglogic-org-mode-hook ()
    "Enable some org mode specific settings"
    ;; Electric pair mode makes org links super annoying to write
    (when (string-match "/Org/" (buffer-file-name))
      (add-hook 'after-save-hook
                (lambda ()
                  (shell-command
                   "dfm sync --name Org --message 'Notes synced from Emacs'"
                   "*dfm output*"))
                nil
                'local-only))
    (display-line-numbers-mode -1)
    (electric-pair-local-mode -1))
  (add-hook 'org-mode-hook 'chasinglogic-org-mode-hook)

  ;; Capture Templates
  ;;
  ;; Capture templates are related to Agenda views in that they are
  ;; what feed my TODO list. I keep different templates for the
  ;; different kinds of things I add to the TODO list. It automates my
  ;; tagging system described in [[Org Mode Task Management
  ;; Workflow]].

  ;; First start the declaration of the variable:
  (setq-default org-capture-templates
                '(
                  ;; Capture: Notes

                  ;; A generic Note capture this should always be a
                  ;; top level heading. More often then not I manage
                  ;; the =notes.org= buffer directly instead of via
                  ;; capture but occasionally I'll want to just jot or
                  ;; start something and the buffer won't be
                  ;; immediately available. This entry will be
                  ;; prepended, added to the top of the notes buffer,
                  ;; since I list my notes in the buffer in reverse
                  ;; chronological order of their creation.
                  ("n" "A new note" entry (file org-default-notes-file) "* %?" :prepend t)

                  ;; Capture: Idea

                  ;; Ideas are captured with the date they were
                  ;; conceived. This template automatically adds this
                  ;; property to the entry. Additionally it tags the
                  ;; item as an idea for use in my Agenda view
                  ;; filtering.
                  ("i" "Idea" entry (file org-default-todo-file)
                   "* TODO %? :idea:
:PROPERTIES
:DATE: %t
:END:
         ")))

  ;; Org Export
  ;;
  ;; Org export (ox) is one of the best features of Org. It lets me
  ;; write in the format I've most used and then distribute in
  ;; whatever format is required. It supports a wide array of output
  ;; formats and requires very little configuration.
  ;;
  ;; One of the settings I like to set is `org-export-headline-levels'
  ;; The default value is 3 which I find a little too small. So I
  ;; double it to 6.
  (setq-default org-export-headline-levels 6)
  (require 'ox)
  ;; Next we enable some additional export formats. Markdown, which
  ;; ships with Emacs and Org mode by default is not enabled by
  ;; defualt and is probably my most common target so we always load
  ;; it here.
  (require 'ox-md)

  ;; Next we install and require the `ox-reveal' package. This lets me
  ;; export Org files as full blown slideshow presentations for use in
  ;; my browser.
  (use-package ox-reveal :config (require 'ox-reveal))

  ;; Org Babel
  ;;
  ;; I obviously use babel pretty extensively as I generate all of my
  ;; dotfiles and Emacs configuration using it. Luckily the defaults
  ;; are pretty great so it needs very little configuration. One issue
  ;; I've run into is that I prefer to use Python for generation in a
  ;; Babel file but it's now loaded by default. This setting makes it
  ;; so elisp and Python code blocks are executable in any Org buffer.
  (setq-default org-babel-load-languages '((emacs-lisp . t)
                                           (python . t))))

;; Org Bullets
;;
;; Org bullets replaces the basic =*= characters with prettier UTF-8
;; bullet points in the outline. It's just more aesthetically pleasing
;; I think and makes it clearer what level I'm working at when the
;; subtree isn't narrowed.
(use-package org-bullets
  :after org
  :hook '(org-mode . org-bullets-mode))

(use-package deft
  :commands 'deft
  :config
  (setq deft-extensions ("org")
        deft-use-filename-as-title t
        deft-recursive t
        deft-directory "~/Notes"))

(provide 'chasinglogic-org)

;;; chasinglogic-org.el ends here
