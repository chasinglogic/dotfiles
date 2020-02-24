;;; init.el --- My initialization file

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

;; Auto saves and backups
;;     Emacs has amazing auto save and backup functionality that has
;;     saved me many times after such events as an X11 crash or power
;;     loss. However, it stores all of these files in very inconvenient
;;     locations when working with version control. These configuration
;;     options inform Emacs to store these files somewhere "out of the
;;     way" (=~/.emacs.d/autosaves= and =~/.emacs.d/backups=
;;     respectively).
(when (not (file-directory-p "~/.emacs.d/backups"))
  (make-directory "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/autosaves"))

;; Next we set `backup-directory-alist'. According to the
;; documentation for this variable: "Alist of filename patterns and
;; backup directory names.  Each element looks like (REGEXP
;; . DIRECTORY).  Backups of files with names matching REGEXP will be
;; made in DIRECTORY.". So we set it such that the `REGEXP' is '.*'
;; (matches anything) and `DIRECTORY' is our new backup directory.
(setq-default backup-directory-alist `((".*" . "~/.emacs.d/backups")))

;; Now we do effectively the same for auto saves. Weirdly the
;; configuration for auto saves is slightly different from
;; backups. The documentation for this variable I found a bit opaque
;; but, it takes a list of three element lists and it essentially does
;; an Emacs `replace-regexp' on the filename with the first two
;; elements. So that the list is `REGEXP' followed by
;; `REPLACEMENT'. See [[Emacs Regular Expressions]] in my Notes
;; section for an explanation of the syntax for this. The third
;; element specifies that the transformed name should be made unique
;; in relation to the other auto saves in this directory.
(setq-default auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/\\2" t)))

;; On MacOS make the command key meta. Self-explanatory, all of my
;; muscle memory puts meta at the same location as the MacOS command
;; key so we make Emacs treat it as such instead of as super.
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta))

;; Add the lisp directory where all other files are required from to
;; the load path.
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp"))

;; Package initialization
;;    Before we can set up our configuration for third party packages we
;;    have to initialize the built-in Emacs package for fetching and
;;    updating them.
;;    This snippet loads =package.el= and adds the following repositories
;;    to Emacs:
;;    - `elpa': The GNU default package repository. I actually install very
;;      little from here since it tends towards being out of date.
;;    - `melpa': This is where I get almost everything else. It's a
;;      rolling up to date Emacs package repository. Maybe someday if I
;;      experience breakage I'll switch to `melpa-stable' but for years
;;      now I've never had to roll back a package (except when I was on
;;      Spacemacs because an update broke Spacemacs code).
(require 'package)
(setq-default package-archives
              (list
               '("elpa" . "https://elpa.gnu.org/packages/")
               '("melpa" . "https://melpa.org/packages/")))

;; Next we setup the amazing `use-package' package. Every package,
;; other than `use-package' itself, is installed with
;; `use-package'. It's a macro that makes configuration clear,
;; concise, and most importantly fast. It makes every single package
;; lazy load as you need it (when configured properly), greatly
;; improving Emacs startup time.

;; First we set a few global configuration options for `use-package':
;; - `use-package-enable-imenu-support': Allow searching through the
;;   =init.el= for packages using `imenu'.
;; - `use-package-always-ensure': Almost all of the packages that I
;;   configure with `use-package' are third party
;;   packages. `use-package' has a feature called =:ensure= that tells
;;   `use-package' to install the package on startup if it's not
;;   installed. Since `use-package' declarations where I don't want
;;   this behavior are the exception this setting tells `use-package'
;;   to set =:ensure t= by default.
(setq-default use-package-enable-imenu-support t
              use-package-always-ensure t)

;; Next we actually install `use-package'. We wrap this in a
;; `eval-when-compile' call since I byte compile my =init.el= it means
;; I don't pay for this installation at startup time.
(eval-when-compile
  (package-initialize)
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)

(require 'chasinglogic-evil)
(require 'chasinglogic-keys)

;; Emacs environment variables (exec-path-from-shell)
;;
;; Only enabled for MacOS because my .profile works correctly on Linux
(when (eq 'system-type 'darwin)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;; Ensure a few important paths are always present
(mapc 
 (lambda (path)
   (add-to-list 'exec-path path)
   (setenv "PATH" (concat (getenv "PATH") ":" path)))
 (list
  (concat (getenv "HOME") "/.local/bin")
  (concat (getenv "HOME") "/.cargo/bin")))

;; Global customization variables

;; Set mu4e load path as appropriate for the system.
(eval-and-compile (require 'chasinglogic-globals))
(eval-and-compile (require 'chasinglogic-editor))

;; Expand Region
;;
;;     Expand region takes the idea of, what I consider, one of the best
;;     key bindings in Emacs `M-h' (`mark-paragraph') and makes it work
;;     incrementally with semantic units. It's beautiful and useful. For
;;     consistency I bind it to `C-M-h'.
(use-package expand-region
  :bind ("C-M-h" . expand-region))

;; Avy
;;     only compare it to EasyMotion for Vim but it's actually much
;;     better IMO. It has many "jumping" commands for going to words,
;;     subwords, characters, lines etc. Here I only bind a few of the
;;     most useful ones to me.
(use-package avy
  :general
  (leader!
    "jj" 'avy-goto-word-1
    "jw" 'avy-goto-word-1
    "jl" 'avy-goto-line
    "jh" 'avy-goto-heading)
  :bind
  (("M-j"     . 'avy-goto-word-1)
   ("C-c j c" . 'avy-goto-char)
   ("C-c j j" . 'avy-goto-word-1)
   ("C-c j l" . 'avy-goto-line)
   ("C-c j h" . 'avy-goto-heading))
  :config
  ;; When non-nil, a gray background will be added during the selection.
  (setq avy-background t))

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
(require 'term)
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
  (general-nmap
    :keymaps '(magit-mode-map magit-status-mode-map)
    "<tab>" 'magit-section-toggle)
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
  :demand
  :general (leader! "p" '(:which-key "projects" :keymap projectile-command-map))
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ((:map projectile-command-map
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
  (add-hook 'projectile-after-switch-project-hook 'set-frame-name-to-project)
  (chasinglogic-add-projector-projects-to-projectile))


(use-package crux
  :general (leader!
             "fS" 'crux-sudo-edit
             "fD" 'crux-delete-buffer-and-file
             "fr" 'crux-rename-buffer-and-file
             "x" '(lambda () (interactive) (ansi-term (executable-find "bash")))
             "'" 'crux-visit-term-buffer)
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("C-c f D" . crux-delete-buffer-and-file)
         ("C-c f r" . crux-rename-buffer-and-file)
         ("C-x '" . crux-visit-term-buffer)))

;; Better terminal emulator for Emacs
(when module-file-suffix
  (use-package vterm)
  (setq crux-term-buffer-name "v")
  (defun crux-visit-term-buffer ()
    "Create or visit a terminal buffer. If the process in that buffer died, ask to restart."
    (interactive)
    (crux-start-or-switch-to (lambda ()
                               (vterm (concat "*" crux-term-buffer-name "-term" "*")))
                             (format "*%s-term*" crux-term-buffer-name))
    (when (and (null (get-buffer-process (current-buffer)))
               (y-or-n-p "The process has died.  Do you want to restart it? "))
      (kill-buffer-and-window)
      (crux-visit-term-buffer))))

(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
  (if (> (length (window-list)) 1)
      (kill-buffer-and-window)
    (kill-buffer)))

;;;; Global Keybindings

;; Ivy
(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume))
  :config
  (setq ivy-always-two-windows nil)
  (setq ivy-default-display-buffer-functions '(display-buffer-in-side-window))
  (ivy-mode 1))

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
               ("M-r" . 'counsel-minibuffer-history))))

;; Swiper
;;
;; This is an enhanced incremental search provided by Ivy.
(use-package swiper :bind ("C-M-s" . swiper))

(require 'chasinglogic-minor-modes)

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
  (setq-default org-directory (file-name-as-directory "~/Nextcloud/Org")
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
    (display-line-numbers-mode -1)
    (electric-pair-local-mode -1))
  (add-hook 'org-mode-hook 'chasinglogic-org-mode-hook)

  (defun chasinglogic-org-agenda-hook ()
    "Set frame name to Agenda"
    (set-frame-name "Agenda"))
  (add-hook 'org-agenda-mode-hook 'chasinglogic-org-agenda-hook)

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
                   ((tags "+reading_list" ((org-agenda-overriding-header "Reading List")
                                           (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "Reading List"))))))

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

;;;; Major Modes

;; Ruby
;;
;; I don't do much writing of Ruby so I find the built in `ruby-mode'
;; pretty much adequate with one exception: automatically adding `end'
;; where needed.
;;
;; This package extends `electric-pair-mode' to handle languages like
;; Ruby where the closing pair can sometimes be a word or other
;; stranger set of symbols. In short it automatically adds `end' for
;; `if''s, `functions''s, and loops in Ruby.
(use-package ruby-electric
  :diminish ""
  :hook 'ruby-mode)

;; Python
;;
;; I write a lot of Python code. Luckily I only write Python 3 code
;; nowadays so the first thing to do is set the
;; `python-shell-interpreter' variable to Python 3. Additionally tell
;; Flycheck to always use this variable for the various Python linters
;; it runs.

;; Use correct Python3
(setq-default python-shell-interpreter (if (eq system-type 'darwin)
                                           "/usr/local/bin/python3"
                                         "python3"))
(setq-default flycheck-python-flake8-executable python-shell-interpreter
              flycheck-python-pylint-executable python-shell-interpreter
              flycheck-python-pycompile-executable python-shell-interpreter)

;; Next I use the Black Python formatter for my code. This package
;; integrates it into Emacs and lets me run it as an after save
;; hook. My hook has to be a little smarter however because my work
;; projects do not use this formatter so define a "black list" for
;; Black and only add the hook if we aren't in one of those projects.
(use-package blacken
  :commands 'blacken-buffer
  :init
  (setq-default chasinglogic-blacken-black-list
                '("scons"
                  "mongo"
                  "enterprise"
                  "mongo-enterprise-modules"
                  "toolchain-builder"
                  "kernel-tools"))

  (defun chasinglogic-python-format-hook ()
    "Set up blacken-buffer on save if appropriate."
    (unless (member (projectile-project-name) chasinglogic-blacken-black-list)
      (message "Not in a blacklisted project, enabling format on save.")
      (add-hook 'before-save-hook 'blacken-buffer nil t)))
  (add-hook 'python-mode-hook 'chasinglogic-python-format-hook))

;; Making Emacs and virtualenvs work together has been one of the most
;; frustrating things about my time with Emacs. After literal years of
;; tweaking and testing I finally have a solution that I like. I use
;; `virtualenvwrapper' to create my virtualenvs with names that match
;; the names returned by =(projectile-project-name)=, essentially this
;; is just the basename of the project directory. Then whenever I run
;; `projectile-switch-project' check for a matching virtualenv if so
;; activate it with the `pyvenv' package.
(use-package pyvenv
  :config
  (defun chasinglogic-auto-venv ()
    "Automatically setup the venv when entering a project"
    (when (file-exists-p (concat "~/.virtualenvs/" (projectile-project-name)))
      (pyvenv-workon (projectile-project-name))))
  (add-hook 'projectile-after-switch-project-hook 'chasinglogic-auto-venv))

;; Load SCons files as Python
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vars\\'" . python-mode))

;; TypeScript
;;
;; Nothing much to be done for TypeScript except install the major
;; mode as I don't work on it all that much.
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (add-hook typescript-mode-hook 'lsp))

;; Markdown Mode
;;
;; I really like Markdown. I obviously use Org mode whenever possible
;; but for those times when i need to write markdown this major mode
;; makes it the best editing experience I've had for markdown.
;;
;; Not much configuration is required here except to bind it to the
;; correct file extensions, make it fontify source blocks according to
;; the correct major mode, disable line numbers, and enable spell
;; checking.
(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.md\\'")
  :config
  ;; Use ndoc for exporting to HTML
  (setq-default markdown-command "pandoc")

  (defun chasinglogic-markdown-mode-hook ()
    "Disable line numbers and auto-wrap at 80 in Markdown"
    (markdown-toggle-fontify-code-block-natively)
    (display-line-numbers-mode -1)
    (flyspell-mode 1)
    (auto-fill-mode 1))

  (add-hook 'markdown-mode-hook 'chasinglogic-markdown-mode-hook))

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
    ;; indent case statements
    (c-set-offset 'case-label '+))
  (add-hook 'web-mode-hook 'chasinglogic-web-mode-hook)
  (add-hook 'web-mode-hook 'lsp)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

  (setq-default web-mode-markup-indent-offset 2
                web-mode-style-indent-offset 2
                web-mode-code-indent-offset 2))

;; C / C++
;;
;; I have to read more C++ than I have to write but for those times
;; when I do this configuration ensures that my code is formatted,
;; correct, and ready to commit.
;;
;; First we install the `clang-format' package and point it at the
;; MongoDB toolchain binary since it's always the right version.
(use-package clang-format
  :commands (clang-format-buffer)
  :config
  (setq clang-format-binary "/opt/mongodbtoolchain/v3/bin/clang-format"))

;; Use clang-tidy for flycheck on top of the compilation checking
;; provided by LSP. I use clangd as my language server which does
;; support clang-tidy but unfortunately it ignores the configuration
;; files
(use-package flycheck-clang-tidy)

(use-package ccls)

;; Next create a C++ mode hook that makes Emacs format / indent things
;; correctly according to MongoDB's style guide. Additionally make it
;; so Flycheck will pass ~-std=c++17~ when doing syntax checking and
;; to allow `src' directory relative =#includes=. Finally make it such
;; that header files are treated as C++ and not C.
(defun chasinglogic-cpp-mode-hook ()
  "Set up various C++ tools and options."
  (require 'ccls)
  ;; Don't indent namespaces
  (c-set-offset 'innamespace [0])
  (setq-local c-basic-offset 4)
  ;; Tell Flycheck I write modern C++ and use src-relative includes
  (setq flycheck-clang-language-standard "c++17"
        flycheck-clang-include-path (list (concat (projectile-project-root) "src")))

  ;; Auto format C/C++ buffers
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(add-hook 'c++-mode-hook 'chasinglogic-cpp-mode-hook)
(add-hook 'c-mode-hook 'chasinglogic-cpp-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Rust is my go to programming language outside of work. It has
;; excellent Emacs support but most of the features I need are
;; actually provided by LSP mode. This simply installs and attaches
;; the Rust major mode to =.rs= files, enables format on save, and
;; sets a better default compile command. Finally it loads `lsp' on
;; `rust-mode' startup.
(use-package rust-mode
  :mode ("\\.rs\\'")
  :config
  (setq rust-format-on-save t)
  (defun chasinglogic-rust-mode-hook ()
    (setq-local compile-command "cargo clippy && cargo test"))
  (add-hook 'rust-mode-hook 'chasinglogic-rust-mode-hook)
  (add-hook 'rust-mode-hook #'lsp))

;; Miscellaneous Major Modes
;;
;; This is a list of major modes that I occasionally need and so are
;; useful to have installed but I do not configure them as I do not
;; write in these languages often or extensively.
;;
;; The following snippet just installs and attaches these modes to
;; file extensions.
(use-package vala-mode :mode ("\\.vala\\'"))
(use-package meson-mode :mode ("meson\\.build"))
(use-package powershell :mode ("\\.ps1\\'"))
(use-package groovy-mode :mode ("\\.groovy$" "\\.gradle$"))
(use-package yaml-mode :mode ("\\.yaml\\'" "\\.yml\\'" "\\.idl\\'"))
(use-package toml-mode :mode ("\\gitconfig\\'" "\\.toml\\'"))
(use-package cmake-mode :mode ("\\CMake.*txt\\'"))
(use-package nginx-mode :mode ("\\.conf'"))

;; Hydra
;;
;;     Hydra is a weird package. You define a hydra and when it's
;;     activated you can press any keys in the hydra to use the command
;;     bound to that "hydra head". When you press any key that doesn't
;;     correspond to a head the hydra ends and you go back to a regular
;;     "Emacs state". I use this for when I need to string commands
;;     together frequently such as when searching / moving through a code
;;     base or doing complex window management. It saves having hold
;;     control or meta forever.
(use-package hydra
  :config
  ;; Movement Hydra
  ;;  I use this hydra for when I'm reading a code base. It lets me
  ;;  string together Emacs movement and search commands.
  (defhydra chasinglogic-movement-hydra (global-map "C-x m")
    ("q" nil "quit")
    ("n" next-line "next line")
    ("p" previous-line "previous line")
    ("b" backward-char "backward char")
    ("f" forward-char "forward char")
    ("i" isearch-forward "isearch forward")
    ("s" helm-swoop "swoop search")
    ("r" helm-rg "ripgrep search")
    ("R" helm-projectile-rg "project level ripgrep search")
    ("w" forward-word "forward word")
    ("W" backward-word "backward word")
    ("v" scroll-up-command "scroll down")
    ("V" scroll-down-command "scroll up")
    ("l" recenter-top-bottom "recenter")
    ("h" org-next-visible-heading "next heading")
    ("H" org-previous-visible-heading "previous heading")
    ("[" backward-paragraph "backward paragraph")
    ("]" forward-paragraph "forward paragraph"))

  ;; Window Management Hydra
  ;;
  ;;      I use this when I'm setting up a specific window configuration or
  ;;      flipping between window configurations with registers.
  (defhydra chasinglogic-window-hydra (global-map "C-c j w")
    ("q" nil "quit")
    ("j" ace-window "switch windows")
    ("r" window-configuration-to-register "save window configuration to register")
    ("l" jump-to-register "load window configuration from register")
    ("=" balance-windows "balance windows")
    ("d" delete-window "delete this window")
    ("o" delete-other-windows "delete other windows")
    ("v" split-window-right "split window to right")
    ("s" split-window-below "split window below")))

(when (not (display-graphic-p))
  (use-package xclip :config (xclip-mode 1)))

(require 'chasinglogic-email)
(require 'auto-sync-mode)

;; Post initialization
;;
;; These are the few final steps we should take when bringing up
;; Emacs.
;;
;; First Maximize this frame, the initial frame won't see our hooks in
;; `make-frame-init-functions'.
(toggle-frame-maximized)
