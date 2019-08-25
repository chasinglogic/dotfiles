(when (not (file-directory-p "~/.emacs.d/backups"))
  (make-directory "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/autosaves"))

(setq-default backup-directory-alist `((".*" . "~/.emacs.d/backups")))

(setq-default auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/\\2" t)))

(setq-default user-full-name "Mathew Robinson"
              user-mail-address "mathew@chasinglogic.io"
              message-signature "- Mathew Robinson @chasinglogic")

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta))

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq-default vc-follow-symlinks t)

(defun comment-actually-dwim (arg)
  "A simpler and more functional version of `comment-dwim'. It
simply calls `comment-or-uncomment-region' with the current line
or the active region.

The complexity in the original `comment-dwim' comes from trying
to manage comments at the end of lines. I rarely do on line
comments so this function better suits my needs."
  (interactive "*P")
  (comment-normalize-vars)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(setq compilation-scroll-output t)

(put 'downcase-region 'disabled nil)

(require 'package)

(setq-default package-archives
              (list
               '("elpa" . "http://elpa.gnu.org/packages/")
               '("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(setq-default use-package-enable-imenu-support t
              use-package-always-ensure t)

(eval-when-compile
  (package-initialize)
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(use-package quelpa
  :init
  (when (not (package-installed-p 'quelpa-use-package))
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://framagit.org/steckerhalter/quelpa-use-package.git")))
  (require 'quelpa-use-package))

(setq-default chasinglogic-font-size "13")
(when (and (display-graphic-p) (eq system-type 'darwin))
  ;; Retina display requires bigger font IMO.
  (setq chasinglogic-font-size "15"))
(set-frame-font (format "Source Code Pro %s" chasinglogic-font-size) nil t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package zenburn-theme)
(use-package solarized-theme
  :config
  (setq-default solarized-high-contrast-mode-line t
                solarized-distinct-doc-face t
                solarized-distinct-fringe-background t)
  (load-theme 'solarized-light t))

(defun enable-display-line-numbers-mode ()
  "Enable display-line-numbers-mode"
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'enable-display-line-numbers-mode)

(defun maximize-gui-frames (frame)
  "Maxmize a the GUI frame FRAME."
  (with-selected-frame frame
    (when (display-graphic-p)
      (set-frame-parameter nil 'fullscreen 'maximized))))
(add-hook 'after-make-frame-functions 'maximize-gui-frames)

(require 'dired-x)

(setq-default dired-dwim-target t)

(abbrev-mode 1)

(defun chasinglogic-enable-flyspell ()
  "Enable spell checking."
  (flyspell-mode 1))

(defun chasinglogic-enable-flyspell-prog ()
  "Enable spell checking."
  (flyspell-mode -1)
  (flyspell-prog-mode))

(add-hook 'text-mode-hook 'chasinglogic-enable-flyspell)
(add-hook 'prog-mode-hook 'chasinglogic-enable-flyspell-prog)

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

(electric-indent-mode 1)

(electric-layout-mode 1)

(defun enable-electric-pair-local-mode ()
  "Enable eletric pair mode locally."
  (electric-pair-local-mode 1))
(add-hook 'prog-mode-hook 'enable-electric-pair-local-mode)

(show-paren-mode 1)

;; from spacemacs-core
;; from magnars
(defun chasinglogic-delete-current-buffer-file ()
  "Remove file connected to current buffer and kill the related buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (projectile-project-p)
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun chasinglogic-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region-line-by-line (point-min) (point-max)))

(defmacro chasinglogic-find-org-file (name)
  "Create a function to find the org file NAME."
  `(defun ,(intern (format "chasinglogic-find-org-file-%s" name)) ()
     (interactive)
     (let ((file-name (expand-file-name ,(format "%s.org" name) org-directory)))
       (find-file (if (file-exists-p (concat file-name ".gpg"))
                      (concat file-name ".gpg")
                    file-name)))))
(chasinglogic-find-org-file notes)
(chasinglogic-find-org-file ideas)
(chasinglogic-find-org-file todo)

(defun chasinglogic-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun chasinglogic-add-projector-projects-to-projectile ()
  "Add projector projects to projectile."
  (interactive)
  (setq
   projectile-known-projects
   (delete ""
           (split-string
            (shell-command-to-string "projector list") "\n"))))

(defun chasinglogic-shell ()
  "Open my shell in 'ansi-term'."
  (interactive)
  (let* ((project-name (if (projectile-project-name)
                           (projectile-project-name)
                         "main"))
         (shell-buf-name (concat project-name "-shell"))
         (shell-buf-asterisks (concat "*" shell-buf-name "*")))
    (if (get-buffer shell-buf-asterisks)
        (switch-to-buffer shell-buf-asterisks)
      (ansi-term (executable-find "bash") shell-buf-name))))

(defun chasinglogic-copy-breakpoint-for-here (&optional copy)
  "Return a filename:linenumber pair for point for use with LLDB/GDB.

If COPY is provided copy the value to kill ring instead of returning."
  (interactive (list t))
  (let* ((line-number (format "%d" (line-number-at-pos)))
         (file-name (if (projectile-project-root)
                        (file-relative-name (buffer-file-name) (projectile-project-root))
                      (file-name-nondirectory (buffer-file-name))))
         (breakpoint (concat file-name ":" line-number)))
    (if copy
        (progn
          (kill-new breakpoint)
          (message "%s" breakpoint))
      breakpoint)))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

(bind-key "C-x '"   'chasinglogic-shell)
(bind-key "C-c j b" 'chasinglogic-copy-breakpoint-for-here)
(bind-key "C-c j =" 'chasinglogic-indent-buffer)
(bind-key "C-c f r" 'chasinglogic-rename-file-and-buffer)
(bind-key "C-c f D" 'chasinglogic-delete-current-buffer-file)

;; Reverse the M-<> keybinds with M-,. because I move to the
;; beginning and end of buffers far more often than I
;; xref-pop-marker-stack
(bind-key "M-<" 'xref-pop-marker-stack)
(bind-key "M->" 'xref-find-definitions)
(bind-key "M-," 'beginning-of-buffer)
(bind-key "M-." 'end-of-buffer)

;; Bind M-[] to paragraph movement. Normally this is M-{} which
;; still is bound. This is more convenient and the M-[] keys were
;; bound to nighting anyway
(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)

(bind-key "C-x C-b" 'ibuffer)

(bind-key "M-;" 'comment-actually-dwim)

(use-package evergreen
  :quelpa (evergreen :repo "evergreen-ci/evergreen.el" :fetcher github)
  :commands (evergreen-patch evergreen-list-spawn-hosts)
  :config
  (setq-default evergreen-generate-description t
                evergreen-finalize-when-patching t
                evergreen-browse-when-patching t
                evergreen-default-project "mongodb-mongo-master"
                evergreen-assume-yes t))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package diminish
  :init
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode))

(use-package which-key
  :diminish ""
  :init
  (which-key-mode))

(use-package expand-region
  :bind ("C-M-h" . expand-region))

(use-package avy
  :bind
  (("M-j"     . 'avy-goto-word-1)
   ("C-c j c" . 'avy-goto-char)
   ("C-c j j" . 'avy-goto-word-1)
   ("C-c j l" . 'avy-goto-line)
   ("C-c j h" . 'avy-goto-heading)))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package hydra
  :config

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

(defhydra chasinglogic-window-hydra (global-map "C-c j w")
  ("q" nil "quit")
  ("j" ace-window "switch windows")
  ("r" window-configuration-to-register "save window configuration to register")
  ("l" jump-to-register "load window configuration from register")
  ("=" balance-windows "balance windows")
  ("d" delete-window "delete this window")
  ("o" delete-other-windows "delete other windows")
  ("v" split-window-right "split window to right")
  ("s" split-window-below "split window below"))

)

(use-package paredit
  :bind (:map paredit-mode-map
              ;; I do not like any version of the original
              ;; `comment-dwim' and paredit has it's own special
              ;; version that I find more confusing. So overwrite it's
              ;; mapping with my `comment-actually-dwim' function.
              ("M-;" . comment-actually-dwim))
  :hook '(emacs-lisp-mode . paredit-mode))

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

(use-package helm-swoop
  :after helm
  :bind ("C-M-s" . helm-swoop))

(use-package helm-mu
  :after (helm mu4e)
  :hook (mu4e-main-mode-hook .
                             (lambda ()
                               (bind-key "s" helm-mu mu4e-main-mode-map)
                               (bind-key "s" helm-mu mu4e-headers-mode-map)
                               (bind-key "s" helm-mu mu4e-view-mode-map)))
  :bind (("C-c s m" . helm-mu)
         ("C-c s c" . helm-mu-contacts)))

(use-package helm-projectile
  :after (helm projectile)
  :bind (:map projectile-command-map
              ("h" . helm-projectile-find-other-file)
              ("f" . helm-projectile-find-file)
              ("p" . helm-projectile-switch-project)
              ("s" . helm-projectile-rg)))
(use-package helm-rg :after 'helm-projectile)

(use-package helm-org
  :after (helm org)
  :bind (("C-c s o c" . helm-org-capture-templates)
         ("C-c s o h" . helm-org-in-buffer-headings)
         ("C-c o o"   . helm-org-in-buffer-headings)))

(use-package helm-org-rifle
  :after (helm org)
  :bind  ("C-c s o r" . helm-org-rifle))

(use-package company
  :diminish ""
  :config
  (setq-default company-dabbrev-downcase nil)
  (global-company-mode))

(use-package lsp-mode
  :init (setq-default lsp-auto-guess-root t
                      lsp-prefer-flymake nil)
  :commands 'lsp)

(use-package lsp-ui
  :hook 'lsp-mode
  :init
  (setq-default
   lsp-ui-doc-enable nil
   lsp-ui-peek-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-imenu-enable nil
   lsp-ui-flycheck-enable t))

(use-package company-lsp
  :config (push 'company-lsp company-backends)
  :after (lsp-mode company))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package org
  :ensure nil
  :commands (org-capture org-agenda)
  :mode ("\\.org\\'" . org-mode)

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

(setq-default org-refile-targets '((nil :maxlevel . 1)
                                   (org-agenda-files :maxlevel . 2))
              org-refile-use-outline-path 'file
              org-outline-path-complete-in-steps nil
              org-refile-allow-creating-parent-nodes 'confirm)

(defun chasinglogic-org-mode-hook ()
  "Enable some org mode specific settings"
  ;; Electric pair mode makes org links super annoying to write
  (display-line-numbers-mode -1)
  (electric-pair-local-mode -1))
(add-hook 'org-mode-hook 'chasinglogic-org-mode-hook)

(setq-default org-directory (file-name-as-directory "~/Nextcloud/Org")
              org-default-todo-file  (expand-file-name "inbox.org"  org-directory)
              org-default-notes-file (expand-file-name "notes.org.gpg" org-directory)
              org-default-ideas-file (expand-file-name "inbox.org" org-directory))

(setq-default org-agenda-files (list org-default-todo-file
                                     (expand-file-name "todo.org" org-directory)))

(setq-default org-highest-priority ?A
              org-lowest-priority ?D
              org-default-priority ?D)

(setq-default org-log-done 'time)

(setq-default org-agenda-window-setup 'only-window)

(setq-default org-todo-keywords '((sequence "TODO(t)" "NEXT(n!)" "STARTED(s!)" "|" "DONE(d!)" "CANCELLED(c!)"))
              org-todo-keyword-faces '(("TODO" . (:foreground "#cc9393" :weight bold))
                                       ("NEXT" . (:foreground "#b58900" :weight bold))
                                       ("STARTED". (:foreground "#6c71c4" :weight bold))
                                       ("DONE" . (:foreground "green" :weight bold))
                                       ("CANCELLED" . (:foreground "#dc322f"))))

(setq-default org-stuck-projects '("+LEVEL=1/-DONE" ("STARTED" "NEXT") nil ""))

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
                    (org-agenda-sorting-strategy '(priority-down tag-up))
                    ;; 7 day advanced warning for deadlines
                    (org-deadline-warning-days 7)))
                  (todo "" ((org-agenda-overriding-header "Next Actions:")
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT" "STARTED")))))
                  (stuck "" ((org-agenda-overriding-header "Stuck Projects:")))))))

(setq-default org-capture-templates '(

("t" "Task todo" entry (file org-default-todo-file) "* TODO %?")

("r" "Reading list" entry (file org-default-todo-file)
          "* TODO %i %? :reading_list:
:PROPERTIES:
:CREATED: %t
:END:")

("n" "A new note" entry (file org-default-notes-file) "* %?" :prepend t)

("I" "Interview"
 entry (file+headline org-default-notes-file "Interviews")
 "** Interviewee: %? :interview:
:PROPERTIES:
:DATE: %t
:END:

")

("i" "Idea" entry (file org-default-todo-file)
          "* TODO %? :idea:
:PROPERTIES
:DATE: %t
:END:
         ")

))

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

(setq-default org-export-headline-levels 6)

(require 'ox-md)

(use-package ox-reveal :config (require 'ox-reveal))

(setq-default org-babel-load-languages '((emacs-lisp . t)
                                         (python . t)))

)

(use-package org-bullets
  :after org
  :hook '(org-mode . org-bullets-mode))

(use-package flycheck
  :diminish ""
  :bind (("C-c e l" . flycheck-list-errors)
         ("C-c e v" . flycheck-verify-setup)
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error))
  :hook 'text-mode-hook
  :config
  ;; this trys to run the dash shell which I don't use but instead
  ;; opens the Dash.app program which I do use.
  (setq flycheck-sh-posix-dash-executable ""))

(use-package flycheck-vale
  :after 'flycheck
  :config
  (flycheck-vale-setup))

(use-package magit
  :bind (("C-x v d" . magit-diff)
         ("C-x v b" . magit-blame)
         ("C-x v l" . magit-log-current)
         ("C-x v a" . magit-stage-file)
         ("C-x v c" . magit-commit)
         ("C-x v s" . magit-status))
  :commands 'magit-status)

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

(eval-and-compile
  (setq-default mu4e-load-path (if (eq system-type 'darwin)
                                   "/usr/local/share/emacs/site-lisp/mu/mu4e"
                                 "/usr/share/emacs/site-lisp/mu4e")))
(when (file-exists-p mu4e-load-path)
  (use-package mu4e
    :load-path mu4e-load-path
    :bind ("C-c m" . mu4e)
    :config

(setq-default mu4e-maildir "~/Mail")

(setq-default mu4e-get-mail-command "true")

(setq-default mu4e-confirm-quit nil
              message-kill-buffer-on-exit t)

(setq-default mu4e-headers-show-threads nil
              mu4e-headers-include-related nil
              mu4e-context-policy 'pick-first)

(setq-default mu4e-split-view 'horizontal)

(setq-default mu4e-sent-messages-behavior 'delete)

(setq-default mu4e-view-show-images t
              mu4e-view-show-addresses t
              mu4e-use-fancy-chars t)

(setq-default mu4e-compose-format-flowed t)

(setq-default mu4e-update-interval 300)

(setq-default message-send-mail-function 'message-smtpmail-send-it
              send-mail-function 'smtpmail-send-it)

(setq-default shr-use-colors nil
              shr-color-visible-luminance-min 100
              mu4e-html2text-command 'mu4e-shr2text)

(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(setq-default mail-user-agent 'mu4e-user-agent)

(add-hook 'mu4e-main-mode-hook '(lambda () (set-frame-name "Email")))

(setq-default mu4e-contexts `(

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
          (smtpmail-stream-type . nil)
          (smtpmail-local-domain . "gmail.com")
          (smtpmail-default-smtp-server . "smtp.gmail.com")
          (smtpmail-smtp-server . "smtp.gmail.com")
          (smtpmail-smtp-service . 587)
          (smtpmail-smtp-user . "mathew.robinson@10gen.com")
          (user-mail-address . "mathew.robinson@mongodb.com")
          )
  )

,(make-mu4e-context
  :name "Personal"
  :match-func (lambda (msg)
                (when msg
                  (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
  :vars '(
          (mu4e-drafts-folder . "/personal/Drafts")
          (mu4e-sent-folder . "/personal/Sent")
          (mu4e-trash-folder . "/personal/Trash")
          (mu4e-refile-folder . "/personal/Archive")
          (smtpmail-stream-type . ssl)
          (smtpmail-smtp-server . "smtp.fastmail.com")
          (smtpmail-smtp-service . 465)
          (smtpmail-local-domain . "chasinglogic.io")
          (smtpmail-smtp-user . "mathew@chasinglogic.io")
          (user-mail-address . "mathew@chasinglogic.io")
          )
  )
)) ;; End the context setq

(setq-default mu4e-bookmarks
              `(
                ,(make-mu4e-bookmark
                  :name  "Inbox"
                  :query "(maildir:/personal/INBOX OR maildir:/work/INBOX) AND (flag:unread OR flag:flagged) AND NOT flag:trashed"
                  :key ?i)
                ,(make-mu4e-bookmark
                  :name  "Unread messages"
                  :query "flag:unread AND NOT flag:trashed"
                  :key ?u)
                ,(make-mu4e-bookmark
                  :name "Flagged (Starred)"
                  :query "flag:flagged"
                  :key ?f)
                ,(make-mu4e-bookmark
                  :name "Today's messages"
                  :query "(date:today..now)"
                  :key ?t)
                ))

(add-hook 'mu4e-compose-mode-hook 'auto-fill-mode)
(add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign)

)

(use-package mu4e-alert
  :after mu4e
  :hook (mu4e-main-mode-hook . (lambda ()
                                 (setq mu4e-alert-interesting-mail-query
                                       "(flag:unread maildir:/personal/Inbox) OR (flag:unread maildir:/work/INBOX)")
                                 (when (eq system-type 'gnu/linux)
                                   (mu4e-alert-set-default-style 'libnotify))
                                 (mu4e-alert-enable-mode-line-display)
                                 (mu4e-alert-enable-notifications))))

)

(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :config
  (setq-default
   git-link-default-branch "master"
   git-link-open-in-browser t))

(use-package yasnippet
  :diminish 'yas-minor-mode
  :config (yas-global-mode 1))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ((:map projectile-command-map
               ("p" . projectile-switch-project)
               ("f" . projectile-find-file)
               ("F" . projectile-find-file-in-known-projects)
               ("d" . projectile-find-dir)
               ("b" . projectile-switch-to-buffer)))
  :init
  (chasinglogic-add-projector-projects-to-projectile)
  :config
  (defun chasinglogic-switch-project-action ()
    "Single view magit status page when switching projects."
    (interactive)
    (magit-status)
    (delete-other-windows))

  (setq-default projectile-require-project-root t
                projectile-completion-system 'helm
                projectile-enable-caching nil
                ;; I prefer a git status when switching to a project
                projectile-switch-project-action 'chasinglogic-switch-project-action
                ;; I really don't need tags
                projectile-tags-command "")
  ;; When switching projects set frame name to project name
  (defun set-frame-name-to-project ()
    (set-frame-parameter (selected-frame) 'name (projectile-project-name)))
  (add-hook 'projectile-after-switch-project-hook 'set-frame-name-to-project))

(use-package writeroom-mode :commands (writeroom-mode))

(use-package hl-todo
  :demand
  :config
  (global-hl-todo-mode))

(use-package ruby-electric
  :diminish ""
  :hook 'ruby-mode)

;; Use correct Python3
(setq-default python-shell-interpreter (if (eq system-type 'darwin)
                                           "/usr/local/bin/python3"
                                         "python3"))
(setq-default flycheck-python-flake8-executable python-shell-interpreter
              flycheck-python-pylint-executable python-shell-interpreter
              flycheck-python-pycompile-executable python-shell-interpreter)

(use-package blacken
  :commands 'blacken-buffer
  :init
  (setq-default chasinglogic-blacken-black-list
                '("scons"
                  "mongo"
                  "enterprise"
                  "mongo_modules_enterprise"
                  "toolchain-builder"
                  "kernel-tools"))

  (defun chasinglogic-python-format-hook ()
    "Set up blacken-buffer on save if appropriate."
    (unless (member (projectile-project-name) chasinglogic-blacken-black-list) 
      (message "Not in a blacklisted project, enabling format on save.")
      (add-hook 'before-save-hook 'blacken-buffer nil t)))
  (add-hook 'python-mode-hook 'chasinglogic-python-format-hook))

(use-package pyvenv
  :commands 'pyvenv-workon
  :after 'projectile
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

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (add-hook typescript-mode-hook 'lsp))

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

(use-package clang-format
  :commands (clang-format-buffer)
  :config
  (setq clang-format-binary "/opt/mongodbtoolchain/v3/bin/clang-format"))

(defun chasinglogic-cpp-mode-hook ()
  "Set up various C++ tools and options."
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

(use-package rust-mode
  :mode ("\\.rs\\'")
  :config
  (setq rust-format-on-save t)
  (defun chasinglogic-rust-mode-hook ()
    (setq-local compile-command "cargo clippy && cargo test"))
  (add-hook 'rust-mode-hook 'chasinglogic-rust-mode-hook)
  (add-hook 'rust-mode-hook #'lsp))

(use-package vala-mode :mode ("\\.vala\\'"))
(use-package meson-mode :mode ("meson\\.build"))
(use-package powershell :mode ("\\.ps1\\'"))
(use-package groovy-mode :mode ("\\.groovy$" "\\.gradle$"))
(use-package yaml-mode :mode ("\\.yaml\\'" "\\.yml\\'" "\\.idl\\'"))
(use-package toml-mode :mode ("\\gitconfig\\'" "\\.toml\\'"))
(use-package cmake-mode :mode ("\\CMake.*txt\\'"))

(maximize-gui-frames (selected-frame))

(require 'server)
(unless (server-running-p)
  (server-start))
