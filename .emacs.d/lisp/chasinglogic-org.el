;;; chasinglogic-org.el --- Org mode configuration

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
  :bind (("C-c o o"   . helm-org-in-buffer-headings)
         ("C-c o TAB" . org-global-cycle)
         ("C-c o a"   . org-agenda)
         ("C-c o c"   . org-capture)
         ("C-c o r"   . org-archive-subtree)
         ("C-c o m n" . chasinglogic-find-org-file-notes)
         ("C-c o m i" . chasinglogic-find-org-file-ideas)
         ("C-c o m t" . chasinglogic-find-org-file-todo)
         ("C-c o m r" . chasinglogic-add-to-reading-list)
         ("C-c o t"   . org-todo)
         ("C-c o s"   . org-schedule)
         ("C-c o g"   . org-set-tags-command)
         ("C-c o P"   . org-set-property-and-value)
         ("C-c o i l" . org-insert-link)
         ("C-c o i h" . org-insert-heading)
         ("C-c o p p" . org-priority)
         ("C-c o p k" . org-priority-up)
         ("C-c o p j" . org-priority-down))
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
    (display-line-numbers-mode -1)
    (electric-pair-local-mode -1))
  (add-hook 'org-mode-hook 'chasinglogic-org-mode-hook)

  (defun chasinglogic-org-agenda-hook ()
    "Set frame name to Agenda"
    (set-frame-name "Agenda"))
  (add-hook 'org-agenda-mode-hook 'chasinglogic-org-agenda-hook)

  ;; Org Mode Settings
  ;;
  ;; These settings are global variables that inform Org mode
  ;; functions or behavior.
  ;;
  ;; First we define global variables that describe where common Org
  ;; mode files can be found. I keep all of my Org files in
  ;; =~/Nextcloud/Org= so they are automatically synced to my
  ;; Nextcloud server by my clients.
  (setq-default org-directory (file-name-as-directory "~/Nextcloud/Org")
                org-default-todo-file  (expand-file-name "inbox.org"  org-directory)
                org-default-notes-file (expand-file-name "notes.org.gpg" org-directory)
                org-default-ideas-file (expand-file-name "inbox.org" org-directory))

  ;; Org Agenda
  ;;
  ;; I use the Org agenda to track what tasks I have to do at any
  ;; given time. I sync this up my Nextcloud instance where it works
  ;; on my phone and iPad via the Beorg app. It's actually a really
  ;; nice system but my primary consumption of this information is via
  ;; agenda views.
  ;;
  ;; First define what files can contain TODO's for the Agenda:
  (setq-default org-agenda-files (list org-default-todo-file
                                       (expand-file-name "todo.org" org-directory)))
  
  ;; Next define the priorities that are available to tasks. I use
  ;; priorities A - D with A being the highest priority.
  (setq-default org-highest-priority ?A
                org-lowest-priority ?D
                org-default-priority ?D)
  
  ;; This variable makes it so when completing a task Org logs the
  ;; time it was completed.
  (setq-default org-log-done 'time)
  ;; Make the agenda the only window when a view is selected. I rarely
  ;; want to look at my agenda and something else. I want to focus
  ;; entirely on planning.
  (setq-default org-agenda-window-setup 'only-window)
  ;; Now we define the valid TODO states a heading can be in. I use
  ;; three states: TODO, NEXT, and DONE. NEXT means either the next
  ;; task to do for a project or the task I'm currently working on for
  ;; that project.
  (setq-default org-todo-keywords '((sequence "TODO" "NEXT(n!)" "STARTED(s!)" "|" "DONE(d!)" "CANCELLED(c!)"))
                org-todo-keyword-faces '(("TODO" . (:foreground "#cc9393" :weight bold))
                                         ("NEXT" . (:foreground "#b58900" :weight bold))
                                         ("STARTED". (:foreground "#6c71c4" :weight bold))
                                         ("DONE" . (:foreground "green" :weight bold))
                                         ("CANCELLED" . (:foreground "#dc322f"))))

  ;; Daily Agenda
  ;;
  ;; This is my most referenced Agenda view. It shows me all scheduled
  ;; items for the day, my "next actions", as well as all of stuck
  ;; projects. My =todo.org= file has top level headings that
  ;; represent projects. Projects range in scope but it's usually
  ;; something that will require more than one step to complete. Stuck
  ;; projects are any level 1 headings in =todo.org= that have no NEXT
  ;; or STARTED subheading.
  ;;
  ;; We define how to find them via the variable `org-stuck-projects':
  (setq-default org-stuck-projects '("+LEVEL=1/-DONE" ("STARTED" "NEXT") nil ""))

  ;; Definition of Agenda Custom Commands
  ;;
  ;; The code that implements the views described above.
  (setq-default org-agenda-custom-commands
                '(

                  ("r" "Reading List"
                   ((tags "+reading_list" ((org-agenda-overriding-header "Reading List")))))

                  ("d" "Daily Agenda"
                   (
                    (agenda
                     ;; Query ("" matches everything)
                     ""
                     ;; Settings
                     ((org-agenda-overriding-header "Today:")
                      ;; Span 1 day (daily agenda)
                      (org-agenda-span 1)
                      ;; Sort by priority highest to lowest then tag
                      (org-agenda-sorting-strategy '(todo-state-down priority-down tag-up))
                      ;; 7 day advanced warning for deadlines
                      (org-deadline-warning-days 7)))
                    (todo "" ((org-agenda-overriding-header "Next Actions:")
                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'nottodo '("NEXT" "STARTED")))
                              (org-agenda-sorting-strategy '(todo-state-down))))
                    (stuck "" ((org-agenda-files (list (expand-file-name "todo.org" org-directory)))
                               (org-agenda-overriding-header "Stuck Projects:")))
                    (todo "" ((org-agenda-overriding-header "Inbox:")
                              (org-agenda-files (list (expand-file-name "inbox.org"org-directory)))))))))

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
                  ;; Capture: TODO
                  ;;
                  ;; This is the simplest, and probably most used,
                  ;; capture template I have. It just records a TODO
                  ;; item with the default priority of `M'.
                  ("t" "Task todo" entry (file org-default-todo-file) "* TODO %?")

                  ;; Capture: Reading List
                  ;;
                  ;; This captures a TODO item that should be on my
                  ;; reading list. I most frequently interact with
                  ;; this template via my utility function
                  ;; `chasinglogic-add-to-reading-list' which is
                  ;; defined later on in this document. It will
                  ;; capture whatever text is in the active region.
                  ("r" "Reading list" entry (file org-default-todo-file)
                   "* TODO %i %? :reading_list:
:PROPERTIES:
:CREATED: %t
:END:")

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

                  ;; Capture: Interview

                  ;; When conducting an interview I file this into my
                  ;; Notes under the Interviews heading. I also tag
                  ;; these entries with the dates they were conducted
                  ;; on.
                  ("I" "Interview"
                   entry (file+headline org-default-notes-file "Interviews")
                   "** Interviewee: %? :interview:
:PROPERTIES:
:DATE: %t
:END:

")

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

  ;; Add to reading list
  ;;
  ;; I get a lot of weekly newsletter emails. Since I read my email in
  ;; Emacs I can quickly add the link under the point to my reading
  ;; list with this command. It will grab the link, make an HTTP
  ;; request to try and find the title of the webpage then insert a
  ;; reading list entry whose heading is a link with the display text
  ;; of the title of the page. I find the extra step to determine the
  ;; page title can be slow but is worth it for two reasons:
  ;;
  ;;  - Reading links to determine their target is hard on Mobile
  ;;    where I consume the reading list the most.
  ;;  - A lot of newsletters use affialiate or click tracking links so
  ;;    the links often have no indication of where they actually go.
  (defun chasinglogic-add-to-reading-list ()
    (interactive)
    (let ((url (thing-at-point 'url)))
      (org-capture-string
       (concat "[[" url "]["
               ;; Get the link title if possible
               (condition-case nil
                   ;; Get title of web page, with the help of
                   ;; functions in url.el
                   (with-current-buffer (url-retrieve-synchronously url)
                     ;; find title by grep the html code
                     (goto-char 0)
                     (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
                     (setq web_title_str (match-string 1))
                     ;; find charset by grep the html code
                     (goto-char 0)
                     (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
                     ;; downcase the charaset. e.g, UTF-8 is not
                     ;; acceptible for emacs, while utf-8 is ok.
                     (setq coding_charset (downcase (match-string 1)))
                     ;; Sometimes titles have newlines but that breaks
                     ;; our org link so strip them.
                     (replace-regexp-in-string
                      "\n" ""
                      ;; decode the string of title.
                      (decode-coding-string web_title_str (intern coding_charset))))
                 ;; Work even in the case of transient network
                 ;; failure. If so just use the url as the title.
                 (error url))
               "]]")
       "r")
      (org-capture-finalize)))

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

(provide 'chasinglogic-org)
;;; chasinglogic-org.el ends here
