;;; init.el --- My init file.
;;
;; Copyright (C) 2018 Mathew Robinson
;;
;; Author: Mathew Robinson <chasinglogic@gmail.com>
;; Created: 24 Aug 2018
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This is my Emacs config, there are many like it but this one is
;; mine.
;;
;;; Code:

;;;; Inititialization

;; (desktop-save-mode 1)

;;;; Package setup

(require 'package)

(setq-default package-archives
              (list
               '("elpa" . "http://elpa.gnu.org/packages/")
               '("org" . "http://orgmode.org/elpa/")
               '("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(setq-default use-package-enable-imenu-support t)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t
  :commands 'diminish
  :init
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode))

;;;; Add my Utilities to the load-path
;;;; Install and setup use-package

(eval-when-compile
  (when (file-exists-p "~/.secrets.el")
    (load-file "~/.secrets.el"))

  (add-to-list 'load-path (concat user-emacs-directory "lisp"))
  (require 'chasinglogic-utils)
  (require 'use-package))

(setq use-package-always-ensure t) ; Always install packages given to use-package

;;;; Install and setup quelpa

(use-package quelpa :commands 'quelpa)
(setq-default evergreen-generate-description t
              evergreen-finalize-when-patching t
              evergreen-browse-when-patching t
              evergreen-default-project "mongodb-mongo-master"
              evergreen-assume-yes t)
(unless (package-installed-p 'evergreen)
  (quelpa
   '(evergreen :repo "evergreen-ci/evergreen.el" :fetcher github)))

;;;; Initialize Environment

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
(setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "HOME") "/.cargo/bin")))

;;;; Global variables

;; Create auto saves dir if not exist
(when (not (file-directory-p "~/.emacs.d/backups"))
  (make-directory "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/autosaves"))

(setq-default
 ;; Tell Emacs a bit about me
 user-full-name "Mathew Robinson"
 user-mail-address "chasinglogic@gmail.com"
 message-signature "- Mathew Robinson @chasinglogic"
 ;; Don't prompt for "git symlinks" and always follow them
 vc-follow-symlinks t
 ;; Change where Emacs saves automatic custom settings
 custom-file "~/.custom.el"
 ;; Store automatic backup files in "~/.saves" instead of `pwd`
 backup-directory-alist `((".*" . "~/.emacs.d/backups"))
 ;; Store auto save files in "~/.emacs.d/autosaves"
 auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/\\2" t))
 ;; show scratch buffer by default
 inhibit-splash-screen t
 ;; spaces not tabs
 indent-tabs-mode nil
 ;; tab default to 4 spaces
 tab-width 4)

;; Change CMD to META on Mac OS
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta))

;;;; Keybindings

(use-package which-key
  :demand
  :diminish ""
  :config
  (which-key-mode))

(use-package general
  :demand
  :config
  (general-create-definer cc! :prefix "C-c")

  (general-define-key "C-x '" 'chasinglogic-shell)
  
  (cc!
    "j" '(:which-key jumps)
    "jb" 'chasinglogic-copy-breakpoint-for-here
    "j="   'chasinglogic-indent-buffer
    "ji"   'imenu)

  (unbind-key "C-x f")
  (general-define-key :prefix "C-x"
                      ;; File Management
                      "f"    '(:which-key "files")
                      "ff" 'find-file
                      "fr"   'chasinglogic-rename-file-and-buffer
                      "fR"   'chasinglogic-reload-config
                      "fs"   'save-buffer
                      "fD"   'chasinglogic-delete-current-buffer-file)

  ;; Reverse the M-<> keybinds with M-,. because I move to the
  ;; beginning and end of buffers far more often than I
  ;; xref-pop-marker-stack
  (general-define-key "M-<" 'xref-pop-marker-stack)
  (general-define-key "M->" 'xref-find-definitions)
  (general-define-key "M-," 'beginning-of-buffer)
  (general-define-key "M-." 'end-of-buffer)

  ;; Bind M-[] to paragraph movement. Normally this is M-{} which
  ;; still is bound. This is more convenient and the M-[] keys were
  ;; bound to nighting anyway
  (general-define-key "M-[" 'backward-paragraph)
  (general-define-key "M-]" 'forward-paragraph)
  
  (general-define-key "C-x C-b" 'ibuffer)
  (general-define-key "M-<up>" 'chasinglogic-move-line-down)
  (general-define-key "M-<down>" 'chasinglogic-move-line-up))

;;;; UI/UX

;; Expand region is like one of my favorite keydings M-h
;; `mark-paragraph'. Except that it keeps expanding a region based on
;; the smallest semtantic meaning near the cursor.
(use-package expand-region :general ("C-M-h" 'er/expand-region))

;; TODO: magnar's multiple cursors

;; Disable some unused chrome
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Font
(setq-default chasinglogic-font-size "11")
(when (and (display-graphic-p) (eq system-type 'darwin))
  ;; Retina display requires bigger font IMO.
  (setq chasinglogic-font-size "18"))
(set-frame-font (format "Source Code Pro %s" chasinglogic-font-size) nil t)

;; Make title bar match color theme on MacOS
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; Color theme

;; I actually like a default Emacs theme.
(load-theme 'wombat)
(add-to-list 'default-frame-alist '(cursor-color . "#fff"))
(set-cursor-color "#fff")
;; Make variables names same color as other text
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#fff")

;; pretty modeline
;; (use-package all-the-icons)
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;;;; Editting Improvements

;; Ediff

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package avy
  :general
  ("M-j" 'avy-goto-word-1)
  (cc!
    "jc" 'avy-goto-char
    "jj" 'avy-goto-word-1
    "jl" 'avy-goto-line
    "jh" 'avy-goto-heading))

(use-package ace-window
  :general ("M-o" 'ace-window))

(use-package hydra
  :demand
  :config
  (defhydra chasinglogic-movement-hydra (global-map "C-x m")
    ("q" nil "quit")
    ("n" next-line "next line")
    ("p" previous-line "previous line")
    ("b" backward-char "backward char")
    ("f" forward-char "forward char")
    ("s" swiper "swiper search")
    ("r" counsel-ripgrep "ripgrep search")
    ("w" forward-word "forward word")
    ("W" backward-word "backward word")
    ("v" scroll-up-command "scroll down")
    ("V" scroll-down-command "scroll up")
    ("l" recenter-top-bottom "recenter")
    ("[" backward-paragraph "backward paragraph")
    ("]" forward-paragraph "forward paragraph"))
  (defhydra chasinglogic-window-hydra (global-map "C-c j w")
    ("q" nil "quit")
    ("j" ace-window "switch windows")
    ("=" balance-windows "balance windows")
    ("d" delete-window "delete this window")
    ("o" delete-other-windows "delete other windows")
    ("v" split-window-right "split window to right")
    ("s" split-window-below "split window below")))

;; auto pair things in lisp
(use-package paredit)
;; Auto do stuff that I like.
(electric-layout-mode 1)
(electric-indent-mode 1)

(use-package ruby-electric
  :diminish ""
  :config
  (ruby-electric-mode 1))
;; highlight matching parens
(show-paren-mode 1)

(defun maximize-gui-frames (frame)
  "Maxmize a the GUI frame FRAME."
  (with-selected-frame frame
    (when (display-graphic-p)
      (set-frame-parameter nil 'fullscreen 'maximized))))
(add-hook 'after-make-frame-functions 'maximize-gui-frames)

;; Enable line numbers in programming modes
(defun chasinglogic-enable-line-numbers-hook ()
  "Enable line numbers."
  (electric-pair-local-mode 1)
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'chasinglogic-enable-line-numbers-hook)
(add-hook 'text-mode-hook 'chasinglogic-enable-line-numbers-hook)

;;;; Ivy

(use-package ivy
  :diminish ""
  :general
  ("M-y" 'counsel-yank-pop)
  ("C-M-s" 'swiper)
  :init
  (setq
   enable-recursive-minibuffers t
   ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package smex :after 'counsel)
(use-package counsel
  :commands (
             ;; Auto loaded by projectile on use.
             counsel-ag
             counsel-rg)
  :general
  ("C-x r b" 'counsel-bookmark)
  ("M-x" 'counsel-M-x)
  ("M-y" 'counsel-yank-pop))

;;;; Org / Notes

(defun chasinglogic-enable-flyspell ()
  "Enable spell checking."
  (flyspell-mode 1))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :general
  (cc!
    "o" '(:which-key "org")
    "oTAB" 'org-global-cycle
    "oa"   'org-agenda
    "oc"   'org-capture
    "or"   'org-archive-subtree
    "ons"  'org-toggle-narrow-to-subtree
    "oon"  (chasinglogic-find-org-file notes)
    "ooi"  (chasinglogic-find-org-file ideas)
    "oot"  (chasinglogic-find-org-file todo)
    "oor"  'chasinglogic-add-to-reading-list
    "ot"   'org-todo
    "os"   'org-schedule
    "og"   'org-set-tags-command
    "oP"   'org-set-property-and-value
    "oil"  'org-insert-link
    "oih"  'org-insert-heading
    "op"   '(:which-key "priority")
    "opp"  'org-priority
    "opk"  'org-priority-up
    "opj"  'org-priority-down)
  :commands (org-capture org-insert-link)
  :ensure org-plus-contrib
  :init
  (require 'subr-x)
  (add-hook 'org-mode-hook 'chasinglogic-enable-flyspell)
  (defun chasinglogic-org-mode-hook ()
    "Enable some org mode specific settings"
    ;; Electric pair mode makes org links super annoying to write
    (display-line-numbers-mode -1)
    (electric-pair-local-mode -1)
    (local-set-key (kbd "RET") 'newline-and-indent))
  (add-hook 'org-mode-hook 'chasinglogic-org-mode-hook)

  (defun chasinglogic-org-capture-mode-hook ()
    "Do some capture stuff"
    (delete-other-windows))
  (add-hook 'org-capture-mode-hook 'chasinglogic-org-capture-mode-hook)

  (setq-default
   org-agenda-custom-commands
   '(
     ("r" "Reading List"
      ((tags-todo "+reading_list")))

     ("t" "TODO List"
      ((tags-todo
        ;; Query (filter out the reading_list)
        "-reading_list"
        ;; Settings
        ((org-agenda-sorting-strategy '(tag-up priority-down))))))

     ("d" "Daily Agenda and all TODOs"
      (
       (agenda
        ;; Query ("" matches everything)
        ""
        ;; Settings 
        (
         ;; Span 1 day (daily agenda)
         (org-agenda-span 1)
         ;; Sort by tag then priority highest to lowest
         (org-agenda-sorting-strategy '(tag-up priority-down))
         ;; 7 day advanced warning for deadlines
         (org-deadline-warning-days 7)))

       (tags-todo
        ;; Query (filter out the reading_list)
        "-reading_list-idea"
        ;; Settings
        (
         ;; Same
         (org-agenda-sorting-strategy '(tag-up priority-down))
         ;; Skip if the todo is scheduled
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))))

     ("is" "Ideas for software that need fleshed out"
      ((tags-todo
        ;; Query ideas
        "+idea+software")))

     ("ib" "Ideas for the blog that need fleshed out"
      ((tags-todo
        ;; Query ideas
        "+idea+blog")))

     ("ii" "Ideas that need fleshed out"
      ((tags-todo
        ;; Query ideas
        "+idea")))))

  (setq-default
   ;; Set the default org-directory
   org-directory (file-name-as-directory "~/Nextcloud/Org")

   ;; Define my commonly used files
   org-default-todo-file  (expand-file-name "todo.org"  org-directory)
   org-default-notes-file (expand-file-name "notes.org" org-directory)
   org-default-ideas-file (expand-file-name "ideas.org" org-directory)
   org-agenda-files (list org-default-todo-file)

   ;; Define the custom capture templates
   org-capture-templates '(
                           ("t" "Task todo"
                            entry (file org-default-todo-file)
                            "* TODO [#M] %?")
                           ("r" "Reading list"
                            entry (file org-default-todo-file)
                            "* TODO [#M] %i %? :reading_list:")
                           ("n" "A new note"
                            entry (file org-default-notes-file)
                            "* %? :note:
:PROPERTIES:
:DATE: %t
:END:

"
                            :prepend t
                            :unnarrowed t)
                           
                           
                           ("I" "Interview"
                            entry (file+headline org-default-notes-file "Interviews")
                            "** Interviewee: %? :interview:
:PROPERTIES:
:DATE: %t
:END:

** Phone Interview

** Onsite Interview

")

                           ("i" "Idea" entry (file org-default-ideas-file)
                            "* %? :idea:
:PROPERTIES
:DATE: %t
:END:

"
                            :unnarrowed t)
                           )

   )

  ;; Refiling
  (setq-default
   org-refile-targets '((org-default-todo-file :maxlevel . 1)
                        (org-default-notes-file :maxlevel . 1)))

  (defun chasinglogic-add-to-reading-list ()
    (interactive)
    (let ((url (thing-at-point 'url)))
      (org-capture-string
       (concat "[[" url "]["
               ;; Get the link title if possible
               (condition-case nil
                   ;; Get title of web page, with the help of functions in url.el
                   (with-current-buffer (url-retrieve-synchronously url)
                     ;; find title by grep the html code
                     (goto-char 0)
                     (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
                     (setq web_title_str (match-string 1))
                     ;; find charset by grep the html code
                     (goto-char 0)
                     (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
                     ;; downcase the charaset. e.g, UTF-8 is not acceptible for emacs, while utf-8 is ok.
                     (setq coding_charset (downcase (match-string 1)))
                     ;; Sometimes titles have newlines but that breaks our org link so strip them.
                     (replace-regexp-in-string
                      "\n" ""
                      ;; decode the string of title.
                      (decode-coding-string web_title_str (intern coding_charset))))
                 ;; Work even in the case of transient network failure. If
                 ;; so just use the url as the title.
                 (error url))
               "]]")
       "r")
      (org-capture-finalize)))

  (setq-default org-babel-load-languages
                '((emacs-lisp . t)
                  (python . t)))

  ;; Exporting
  (setq-default
   org-export-headline-levels 6)

  ;; Setup org task management
  (setq-default
   org-highest-priority ?A
   org-lowest-priority ?Z
   org-default-priority ?M
   org-log-done 'time
   org-agenda-window-setup 'only-window
   org-agenda-sorting-strategy '((agenda todo-state-up priority-down timestamp-down)
                                 (todo todo-state-up priority-down timestamp-down)
                                 (tags priority-down timestamp-down)
                                 (search priority-down timestamp-down))))

(use-package org-bullets
  :after org
  :commands 'org-bullets-mode
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package ox-reveal
  :after org
  :config (require 'ox-reveal))

(use-package org2blog
  :after org
  :commands (
             org2blog/wp-login
             org2blog/wp-new-entry
             org2blog/wp-post-buffer
             org2blog/wp-post-subtree
             org2blog/wp-preview-buffer-post
             )
  :config
  (require 'auth-source)
  (let* ((credentials (auth-source-user-and-password "chasinglogic.wordpress.com"))
         (username (nth 0 credentials))
         (password (nth 1 credentials))
         (config `("wordpress"
                   :url "https://chasinglogic.wordpress.com/xmlrpc.php"
                   :username ,username
                   :password ,password)))
    (setq org2blog/wp-blog-alist (list config))))

;;;; Linting

(use-package flycheck
  :diminish ""
  :commands 'flycheck-mode
  :general (cc!
             "e"  '(:which-key "errors")
             "el" 'flycheck-list-errors
             "ev" 'flycheck-verify-setup
             "en" 'flycheck-next-error
             "ep" 'flycheck-previous-error)
  :init
  (defun chasinglogic-enable-flycheck ()
    "Enable flycheck mode"
    ;; enable syntax checking
    (flycheck-mode 1))
  (add-hook 'text-mode-hook 'chasinglogic-enable-flycheck)
  
  (add-hook 'markdown-mode-hook 'chasinglogic-enable-flyspell)
  :config
  ;; this trys to run the dash shell which I don't use but instead
  ;; opens the Dash.app program which I do use.
  (setq flycheck-sh-posix-dash-executable ""))

;; Use vale linter when appropriate
(use-package flycheck-vale
  :after flycheck
  :init
  (flycheck-vale-setup))

;;;; Auto completion

;; COMPlete ANYthing
(use-package company
  :diminish ""
  :config
  (setq-default
   ;; Shorten the default delay to show completions
   company-idle-delay 0.1
   ;; Keep capitalization when completing
   company-dabbrev-downcase nil)
  ;; Enable completion everywhere
  (global-company-mode))

;;;; Git tools

;; Enable magit the git client for Emacs
(use-package magit
  :general (cc! 
             "g" '(:which-key "git")
             "gb" 'magit-blame
             "gl" 'magit-log-current
             "ga" 'magit-stage-file
             "gc" 'magit-commit
             "gs" 'magit-status)
  :commands (magit-status)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-traditional))

(defvar kernel-tools (concat
                      (getenv "HOME")
                      "/Work/kernel-tools/codereview"))
(use-package xgen-cru
  :load-path kernel-tools
  :general (cc!
             "xp" 'xgen-cru-post-review
             "xu" 'xgen-cru-update-review)
  :commands (xgen-cru-update-review xgen-cru-post-review)
  :config
  (setq-default
   xgen-cru-upload-email "mathew.robinson@mongodb.com"
   xgen-cru-jira-username "mathew.robinson"
   xgen-cru-upload-py-path (concat kernel-tools "/upload.py")))

;; Email / Set up mu4e

(defvar mu4e-load-path
  (if (eq system-type 'darwin)
      "/usr/local/share/emacs/site-lisp/mu/mu4e"
    "/usr/share/emacs/site-lisp/mu4e"))
(when (file-exists-p mu4e-load-path)
  (use-package mu4e
    :load-path mu4e-load-path
    :general (cc!
               "m" 'mu4e)
    :config
    ;; Customization variables
    (setq-default mu4e-maildir "~/Mail"
                  mu4e-get-mail-command "true"
                  mu4e-confirm-quit nil
                  mu4e-headers-show-threads nil
                  mu4e-context-policy 'pick-first
                  mu4e-split-view 'horizontal
                  mu4e-view-show-images t
                  mu4e-view-show-addresses t
                  mu4e-use-fancy-chars t
                  mu4e-change-filenames-when-moving t
                  mu4e-headers-include-related nil
                  mu4e-sent-messages-behavior 'delete
                  mu4e-compose-format-flowed t
                  mu4e-update-interval 300
                  message-send-mail-function 'message-smtpmail-send-it
                  send-mail-function 'smtpmail-send-it

                  ;; HTML email settings
                  shr-use-colors nil
                  shr-color-visible-luminance-min 100
                  mu4e-html2text-command 'mu4e-shr2text

                  mail-user-agent 'mu4e-user-agent
                  message-kill-buffer-on-exit t)

    (add-hook 'mu4e-view-mode-hook
              (lambda()
                ;; try to emulate some of the eww key-bindings
                (local-set-key (kbd "<tab>") 'shr-next-link)
                (local-set-key (kbd "<backtab>") 'shr-previous-link)))

    (defvaralias 'mu4e-compose-signature 'message-signature)

    ;; Multi-account automation
    (setq-default chasinglogic-mail-inbox-q "(maildir:/personal/INBOX or maildir:/work/INBOX) AND "
                  mu4e-contexts
                  `(
                    ,(make-mu4e-context
                      :name "Work"
                      :match-func (lambda (msg)
                                    (when msg
                                      (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
                      :vars '(
                              (mu4e-drafts-folder . "/work/drafts")
                              (mu4e-sent-folder . "/work/sent")
                              (mu4e-trash-folder . "/work/trash")
                              (mu4e-refile-folder . "/work/archive")
                              (smtpmail-smtp-user . "chasinglogic@gmail.com")
                              (user-mail-address . "chasinglogic@gmail.com")
                              )
                      )
                    ,(make-mu4e-context
                      :name "Personal"
                      :match-func (lambda (msg)
                                    (when msg
                                      (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
                      :vars '(
                              (mu4e-drafts-folder . "/personal/drafts")
                              (mu4e-sent-folder . "/personal/sent")
                              (mu4e-trash-folder . "/personal/trash")
                              (mu4e-refile-folder . "/personal/archive")
                              (smtpmail-smtp-user . "mathew.robinson@10gen.com")
                              (user-mail-address . "mathew.robinson@mongodb.com")
                              )
                      )
                    )
                  mu4e-bookmarks
                  `(
                    ,(make-mu4e-bookmark
                      :name  "Inbox"
                      :query "(maildir:/personal/INBOX OR maildir:/work/INBOX) AND (flag:unread OR flag:flagged) AND NOT flag:trashed"
                      :key ?i)
                    ,(make-mu4e-bookmark
                      :name  "Unread messages"
                      :query (concat chasinglogic-mail-inbox-q "(flag:unread AND NOT flag:trashed)")
                      :key ?u)
                    ,(make-mu4e-bookmark
                      :name "Flagged (Starred)"
                      :query "flag:flagged"
                      :key ?f)
                    ,(make-mu4e-bookmark
                      :name "Today's messages"
                      :query "(date:today..now)"
                      :key ?t)
                    )
                  ;; I have my "default" parameters from Gmail
                  smtpmail-local-domain "gmail.com"
                  smtpmail-default-smtp-server "smtp.gmail.com"
                  smtpmail-smtp-server "smtp.gmail.com"
                  smtpmail-smtp-service 587)

    ;; Accounts used for composing messages
    (setq-default chasinglogic-mu4e-account-alist
                  '(
                    ("personal"
                     (mu4e-sent-folder "/personal/sent")
                     (user-mail-address "chasinglogic@gmail.com")
                     (smtpmail-smtp-user "chasinglogic"))
                    ("work"
                     (mu4e-sent-folder "/work/sent")
                     (user-mail-address "mathew.robinson@mongodb.com")
                     (smtpmail-smtp-user "mathew.robinson@10gen.com"))
                    ))

    (defun chasinglogic-determine-compose-account ()
      "Determines the account name if possible or prompts for selection"
      (if mu4e-compose-parent-message
          (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
            (string-match "/\\(.*?\\)/" maildir)
            (match-string 1 maildir))
        (completing-read
         "Compose with account: "
         (mapcar #'(lambda (var) (car var)) chasinglogic-mu4e-account-alist)
         nil t nil nil (caar chasinglogic-mu4e-account-alist))))

    (defun chasinglogic-mu4e-set-account ()
      "Set the account for composing a message.
   This function is taken from: 
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
      (let* ((account (chasinglogic-determine-compose-account))
             (account-vars (cdr (assoc account chasinglogic-mu4e-account-alist))))
        (if account-vars
            (mapc #'(lambda (var)
                      (set (car var) (cadr var)))
                  account-vars)
          (error "No email account found"))))
    (add-hook 'mu4e-compose-pre-hook 'chasinglogic-mu4e-set-account)

    (add-hook 'mu4e-compose-mode-hook 'auto-fill-mode)

    (defun chasinglogic-mu4e-hook ()
      "Set up some mu4e stuff on load"
      (set-frame-name "Email"))
    (add-hook 'mu4e-main-mode-hook 'chasinglogic-mu4e-hook)

    (defun chasinglogic-sign-emails ()
      "Sign emails with GPG on send"
      (mml-secure-message-sign))
    (add-hook 'mu4e-compose-mode-hook 'chasinglogic-sign-emails)
    )


  (use-package mu4e-alert
    :ensure t
    :after mu4e
    :init
    (setq mu4e-alert-interesting-mail-query
          "(flag:unread maildir:/personal/INBOX) OR (flag:unread maildir:/work/INBOX)")
    (when (eq system-type 'gnu/linux)
      (mu4e-alert-set-default-style 'libnotify))
    (mu4e-alert-enable-mode-line-display)
    (mu4e-alert-enable-notifications))
  )

;; Open links to Github from Emacs
;; Open Github links from Emacs
(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :config
  (setq-default
   git-link-default-branch "master"
   git-link-open-in-browser t))

;;;; Snippets

(use-package yasnippet
  :diminish 'yas-minor-mode
  :config
  (require 'yasnippet)
  (yas-global-mode 1))

;;;; Project Interaction / Projectile

(use-package projectile
  :general
  ("C-c p" '(:keymap projectile-command-map
                     :which-key "projects"
                     :package projectile))
  (projectile-command-map
   "p" 'projectile-switch-project
   "f" 'projectile-find-file
   "g" 'counsel-rg
   "s" 'counsel-rg
   "F" 'projectile-find-file-in-known-projects
   "d" 'projectile-find-dir
   "b" 'projectile-switch-to-buffer)
  :config
  (defun chasinglogic-switch-project-action ()
    "Single view magit status page when switching projects."
    (interactive)
    (magit-status)
    (delete-other-windows))
  
  (setq-default
   projectile-require-project-root t
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

;;;; Language Server Protocol

(setq-default
 lsp-ui-doc-enable nil
 lsp-ui-peek-enable nil
 lsp-ui-sideline-enable nil
 lsp-ui-imenu-enable nil
 lsp-ui-flycheck-enable t)

(use-package lsp-mode
  :commands 'lsp
  :config
  (setq-default
   lsp-ui-doc-enable nil
   lsp-ui-peek-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-imenu-enable nil
   lsp-ui-flycheck-enable t
   lsp-prefer-flymake nil
   lsp-auto-guess-root t))

(setq-default
 lsp-ui-doc-enable nil
 lsp-ui-peek-enable nil
 lsp-ui-sideline-enable nil
 lsp-ui-imenu-enable nil
 lsp-ui-flycheck-enable t
 lsp-prefer-flymake nil)

(use-package lsp-ui
  :after 'lsp-mode
  :commands 'lsp-ui-mode)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package company-lsp
  :commands 'company-lsp
  :after (lsp-mode company))

;;;; Frame Management

;; This allows selecting a frame by name
(require 'chasinglogic-frames)
(defun chasinglogic-ivy-get-a-frame ()
  "Search and select frames by name."
  (interactive)
  (select-frame-by-name
   (ivy-completing-read
    "Frame: "
    (mapcar 'get-frame-name (frame-list)))))

;;;; Writing

(use-package writeroom-mode
  :general (cc!
             "w" 'writeroom-mode)
  :commands (writeroom-mode))

(use-package hl-todo
  :demand
  :config
  (global-hl-todo-mode))

;;;; Python

;; Use correct Python3
(setq-default
 python-shell-interpreter
 (if (eq system-type 'darwin)
     "/usr/local/bin/python3"
   "python3"))
;; Make flycheck use the previously set python3
(setq-default
 flycheck-python-flake8-executable python-shell-interpreter
 flycheck-python-pylint-executable python-shell-interpreter
 flycheck-python-pycompile-executable python-shell-interpreter)

(use-package blacken
  :commands 'blacken-buffer
  :init
  (defvar chasinglogic-blacken-black-list
    '("scons"
      "mongo"
      "enterprise"
      "mongo_modules_enterprise"
      "toolchain-builder"
      "kernel-tools")
    "Projects who don't use black so don't auto format them.")

  (defun chasinglogic-python-format-hook ()
    "Set up blacken-buffer on save if appropriate."
    (unless (member (projectile-project-name) chasinglogic-blacken-black-list) 
      (message "Not in a blacklisted project, enabling format on save.")
      (add-hook 'before-save-hook 'blacken-buffer nil t)))
  (add-hook 'python-mode-hook 'chasinglogic-python-format-hook))

(use-package pyvenv
  :commands 'pyvenv-workon
  :init
  (defun chasinglogic-auto-venv ()
    "Automatically setup the venv when entering a project"
    (when (file-exists-p (concat "~/.virtualenvs/" (projectile-project-name)))
      (pyvenv-workon (projectile-project-name))))
  (add-hook 'projectile-after-switch-project-hook 'chasinglogic-auto-venv))

(add-hook 'python-mode-hook #'lsp)

;; Load SCons files as Python
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vars\\'" . python-mode))

;;;; TypeScript

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (add-hook typescript-mode-hook 'lsp))

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (typescript-mode . eldoc-mode)
;;          (before-save . tide-format-before-save)))

;;;; Powershell

(use-package powershell :mode ("\\.ps1\\'"))

;;;; Groovy

(use-package groovy-mode :mode ("\\.groovy$" "\\.gradle$"))

;;;; YAML

(use-package yaml-mode :mode ("\\.yaml\\'" "\\.yml\\'" "\\.idl\\'"))

;;;; TOML

(use-package toml-mode :mode ("\\gitconfig\\'" "\\.toml\\'"))

;;;; CMake files

(use-package cmake-mode :mode ("\\CMake.*txt\\'"))

;;;; Markdown

(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.md\\'")
  :config
  ;; Use pandoc for exporting to HTML
  (setq-default markdown-command "pandoc")

  (defun chasinglogic-markdown-mode-hook ()
    "Disable line numbers and auto-wrap at 80 in Markdown"
    (markdown-toggle-fontify-code-block-natively)
    (display-line-numbers-mode -1)
    (flyspell-mode 1)
    (auto-fill-mode 1))

  (add-hook 'markdown-mode-hook 'chasinglogic-markdown-mode-hook))

;;;; Web (HTML/JS/CSS)

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
  ;; (add-hook 'web-mode-hook
  ;;           (lambda ()
  ;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
  ;;               (tide-setup))))
  ;; configure jsx-tide checker to run after your default jsx checker
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

  (setq-default web-mode-markup-indent-offset 2
                web-mode-style-indent-offset 2
                web-mode-code-indent-offset 2))

;;;; C++ / CPP

(use-package clang-format
  :commands (clang-format-buffer)
  :config
  (setq clang-format-binary "/opt/mongodbtoolchain/v3/bin/clang-format"))

(defun chasinglogic-cpp-mode-hook ()
  "Set up various C++ tools and options."
  ;; Don't indent namespaces
  (c-set-offset 'innamespace [0])
  (setq c-basic-offset 4)

  ;; Tell Flycheck I write modern C++ and use src-relative includes
  (setq
   flycheck-clang-language-standard "c++17"
   flycheck-clang-include-path (list (concat (projectile-project-root) "src")))

  ;; Auto format C/C++ buffers
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(setq compilation-scroll-output t)
(add-hook 'c++-mode-hook 'chasinglogic-cpp-mode-hook)
(add-hook 'c-mode-hook 'chasinglogic-cpp-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;; Emacs Lisp

;;;; Rust

(use-package rust-mode
  :mode ("\\.rs\\'")
  :config
  (setq rust-format-on-save t)
  (defun chasinglogic-rust-mode-hook ()
    (setq-local compile-command "cargo clippy && cargo test"))
  (add-hook 'rust-mode-hook 'chasinglogic-rust-mode-hook)
  (add-hook 'rust-mode-hook #'lsp))

;;;; Post-init

;; Maximize this frame
(maximize-gui-frames (selected-frame))

;; Always load keybinds last
(chasinglogic-add-projector-projects-to-projectile)

(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
