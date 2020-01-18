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

;; Temporarily set gc-cons-threshold to 100MB to prevent garbage
;; collection during init. Speeds up startup time.
(setq gc-cons-threshold 100000000)

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

  ;;;; Evil

;; Which Key
;;
;; Which key is possibly the best package ever invented, except for
;; maybe helm. When pressing a key chord it will show you all possible
;; bindings and prefixes so you can interactively explore key bindings
;; as you type them. It's nothing short of amazing and a great
;; discovery tool. No real configuration is needed except that I do
;; diminish it since I always have it on globally.
(use-package which-key
  :diminish ""
  :init
  (which-key-mode))


;; General(.el)
;;
;; General is a convenient way to do evil / leader-esque
;; keybindings. It also provides some macros for easy Emacs style
;; keybindings. I simply use it because it's very convenient and
;; integrates with use-package well.
(use-package general
  :config
  
  (general-def "C-c j b" 'chasinglogic-copy-breakpoint-for-here)
  (general-def "C-c j =" 'chasinglogic-indent-buffer)
  (general-def "C-c f r" 'chasinglogic-rename-file-and-buffer)
  (general-def "C-c f D" 'chasinglogic-delete-current-buffer-file)
  (general-def "C-x 5 o" 'chasinglogic-select-frame-by-name)
  (general-def "C-x C-b" 'ibuffer)
  (general-def "M-;" 'comment-actually-dwim)
  (general-def "M-/" 'hippie-expand)
  (general-def "M-t" 'switch-to-buffer)

  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))

  (general-evil-setup t)
  (general-create-definer leader!
    :states '(normal visual)
    :keymaps 'override
    :prefix "<SPC>")

  (leader!
    "<SPC>" 'execute-extended-command
    "h" `(,(general-simulate-key "C-h") :wk "help")
    "q" '(:which-key "quit")
    "qf" 'delete-frame
    "qq" 'save-buffers-kill-emacs)

  (leader!
    "j" '(:which-key "jumps")
    "j=" 'chasinglogic-indent-buffer
    "jb" 'chasinglogic-copy-breakpoint-for-here)

  (leader!
    "b" '(:which-key "buffers")
    "bd" #'(lambda ()
             (interactive)
             (kill-buffer (current-buffer)))
    "bs" #'(lambda ()
             (interactive)
             (switch-to-buffer "*scratch*"))
    "br" 'revert-buffer
    "bD" 'kill-buffer
    "bm" 'ibuffer)

  (leader!
    "w"  '(:which-key "windows")
    "wf" 'make-frame
    "wo" 'chasinglogic-select-frame-by-name
    "wh" 'evil-window-left
    "wH" 'evil-window-move-far-left
    "wj" 'evil-window-down
    "wJ" 'evil-window-move-very-bottom
    "wk" 'evil-window-up
    "wK" 'evil-window-move-very-top
    "wl" 'evil-window-right
    "wL" 'evil-window-move-far-right
    "wd" 'evil-window-delete
    "wc" 'evil-window-delete
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split
    "wm" 'delete-other-windows)

  (leader!
    "m" '(:which-key "misc")
    "mon" 'chasinglogic-find-org-file-notes
    "moi" 'chasinglogic-find-org-file-ideas
    "mot" 'chasinglogic-find-org-file-todo
    "mor" 'chasinglogic-add-to-reading-list)

  (when (boundp 'tab-bar-mode)
    (leader!
      "t" '(:which-key "tabs")
      "to" 'tab-bar-new-tab
      "ts" 'tab-bar-select-tab-by-name
      "tc" 'tab-bar-close-tab
      "tp" 'tab-bar-switch-to-prev-tab
      "tn" 'tab-bar-switch-to-next-tab))

  (leader!
    "f" '(:which-key "files")
    "ff" 'find-file
    "fs" 'save-buffer))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (general-nmap
    "-" #'(lambda () (interactive) (dired "."))
    "<tab>" 'indent-according-to-mode
    "gcc" 'comment-actually-dwim)
  (general-vmap "gc" 'comment-or-uncomment-region)
  (evil-mode 1)

  (use-package evil-escape
    :config
    (evil-escape-mode 1))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))
  
  (use-package evil-collection
    :config
    (evil-collection-init))

  (use-package evil-magit
    :config
    (require 'evil-magit)))

;; Quelpa (install packages from git)
;;     I maintain a few Emacs packages and it's very helpful to be able to
;;     automatically install and update them. This is what the [[https://framagit.org/steckerhalter/quelpa][Quelpa]]
;;     package does. It lets you treat git remotes as if they were regular
;;     package repositories. Additionally we install [[https://framagit.org/steckerhalter/quelpa-use-package][quelpa-use-package]]
;;     which adds a =:quelpa= keyword argument to `use-package'.
;;     We also only call quelpa on use-package quelpa if it's not
;;     installed because I don't nee to update it very often and it slows
;;     down startup time significantly.
(use-package quelpa
  :init
  (when (not (package-installed-p 'quelpa-use-package))
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://framagit.org/steckerhalter/quelpa-use-package.git")))
  (require 'quelpa-use-package))

;; Emacs environment variables (exec-path-from-shell)

;; I use the `exec-path-from-shell' package to keep my shell and Emacs
;; environment variables in sync. I pay a little in startup time for
;; this but not maintaining two copies of environment variables is way
;; worth it.
(use-package exec-path-from-shell
  :config
  (when (eq 'system-type 'darwin)
    (exec-path-from-shell-initialize)))

(mapc 
 (lambda (path)
   (add-to-list 'exec-path path)
   (setenv "PATH" (concat (getenv "PATH") ":" path)))
 (list
  (concat (getenv "HOME") "/.local/bin")
  (concat (getenv "HOME") "/.cargo/bin")))

;; Add the lisp directory where all other files are required from to
;; the load path.
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp"))

;; Load all of my other configuration files. Order matters.
(eval-and-compile
  (setq-default mu4e-load-path (if (eq system-type 'darwin)
                                   "/usr/local/share/emacs/site-lisp/mu/mu4e"
                                 "/usr/share/emacs/site-lisp/mu4e")))

;; Global customization variables


;;     First we set spaces instead of tabs and set the default
;;     `tab-width' to 4 spaces.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; By default when Emacs tries to open a symlink that points to a git
;; repository it prompts you like "do you really wanna open this
;; file". I use symlinks like this a lot so I disable this prompt.
(setq-default vc-follow-symlinks t)

;; One of the best features of Emacs is it's ability to integrate
;; with programming languages at a syntactic level. It enables you to
;; really edit these languages at that level in some
;; cases. One of the common tasks that it can automate is commenting
;; and uncommenting text in a source file. Unfortunately the default
;; function for this `comment-dwim' assumes that if you have no
;; region you want to insert a line comment. I rarely if ever use
;; line comments and would prefer it to instead comment out the
;; current line if no region is selected so I wrote
;; `comment-actually-dwim' that does this and overwrite the default
;; `comment-dwim' keybinding with my version.
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

;; I use =M-x compile= for running all kinds of commands. This
;; setting makes it so that the buffer auto scrolls to keep up with
;; the output. More like a regular terminal would.
(setq compilation-scroll-output t)

;; As I discover commands that have the "new user warnings" when I use
;; them I disable them here.
(put 'downcase-region 'disabled nil)

;; Don't pollute ~/.emacs.d/init.el with customize settings.
(setq custom-file "~/.emacs-custom.el")

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Just save buffers before compiling
(setq-default compilation-ask-about-save nil
              ;; Always kill old compilation without prompting
              compilation-always-kill t
              ;; Automatically scroll to first error
              compilation-scroll-output 'first-error)

(defalias 'yes-or-no-p 'y-or-n-p)


;; Font
;;     First set the font. I've tried many fonts in my time and I find
;;     Source Code Pro to be a Pretty Good Font™. Other fonts I like are
;;     Inconsolata and DejaVu Sans Mono, and one day I may switch back to
;;     them but getting them on all platforms can be a hassle.
;;     The only thing fancy about the way this font is getting set is that
;;     I use two font sizes: one for my Mac because of the retina display
;;     and one for everything else where I use regular monitors.
(setq-default chasinglogic-font-size "11")
(when (and (display-graphic-p) (eq system-type 'darwin))
  ;; Retina display requires bigger font IMO.
  (setq chasinglogic-font-size "15"))
(set-frame-font (format "Fira Code %s" chasinglogic-font-size) nil t)

;; Window Chrome
;;     Emacs by default has lots of window chrome to make it more mouse
;;     accessible. While I actually use my mouse quite a bit and love
;;     Emacs mouse integration I really hate big UI elements and I never
;;     use the mouse for the operations available in this chrome. These
;;     mode disable lines remove all of this chrome so it's just Me, My
;;     Buffer, and I.
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; On MacOS there's a new feature to have title bars match the window
;; they belong to. This makes Emacs do that so the title bar looks
;; like it's part of the buffer.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Color Theme
;;
;;   I change this too often to really document why whatever
;;   theme I'm in the mood for is the one I'm in the mood for.
;; (use-package zenburn-theme :config (load-theme 'zenburn t))
;; (use-package doom-themes
;;   :config
;;   ;; (load-theme 'doom-palenight t)
;;   (load-theme 'doom-solarized-light t)
;;   (doom-themes-org-config))
(use-package solarized-theme
  :config
  (setq solarized-distinct-fringe-background t
        solarized-high-contrast-mode-line t
        solarized-use-variable-pitch nil)
  (load-theme 'solarized-light-high-contrast t))

;; Line numbers in programming modes.
;;     I enable line numbers using the new Emacs 26
;;     `display-line-numbers-mode' for all programming major modes.
(defun enable-display-line-numbers-mode ()
  "Enable display-line-numbers-mode"
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'enable-display-line-numbers-mode)


;; Automatically maximize Emacs frames when they are created
;;     This is a custom function I wrote that maximizes the frame it's
;;     passed. I then hook it into the `after-make-frame-functions' hook
;;     so any time a frame is created it is maximized.
(defun maximize-gui-frames (frame)
  "Maxmize a the GUI frame FRAME."
  (with-selected-frame frame
    (when (display-graphic-p)
      (set-frame-parameter nil 'fullscreen 'maximized))
    (when (not display-graphic-p)
      (disable-theme 'doom-solarized-light))))
(add-hook 'after-make-frame-functions 'maximize-gui-frames)

;; all-the-icons and doom-modeline
;;
;; Alternative to powerline that still looks good, isn't
;; distracting, but also has all the useful information. I waffle on
;; whether I actually like it or not over the basic modeline.
;; (use-package all-the-icons)
;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode))

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
  (add-hook 'projectile-after-switch-project-hook 'set-frame-name-to-project))


(use-package crux
  :general (leader!
             "fD" 'crux-delete-buffer-and-file
             "fr" 'crux-rename-buffer-and-file
             "'" 'crux-visit-term-buffer)
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("C-c f D" . crux-delete-buffer-and-file)
         ("C-c f r" . crux-rename-buffer-and-file)
         ("C-x '" . crux-visit-term-buffer)))


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

;;;; Minor Modes

;; Abbrev mode is a simple but magical minor mode. I make some
;; spelling mistakes all the time. At this point some of them have
;; become muscle memory and so while I know the spelling is wrong I
;; don't know if I'll ever be able to change them. This is where
;; Abbrev mode comes in. I register abbreviations on a major mode or
;; global basis and `abbrev-mode' will automatically expand them to
;; the correction whenever I type them.
(add-hook 'text-mode-hook 'abbrev-mode)

;; Spell Checking (Flyspell)

;; While Abbrev mode will solve my habitual spelling errors for me
;; it's still nice to have spell check on so I can catch new spelling
;; errors. This is baked into Emacs and requires the `aspell' (or
;; `ispell') program to be installed. I enable `flyspell-mode' for all
;; text buffers and use a subsequent hook for programming modes to
;; disable it and instead enable the programming variant that spell
;; checks comments instead of code.
(defun chasinglogic-enable-flyspell ()
  "Enable spell checking."
  (flyspell-mode 1))

(defun chasinglogic-disable-flyspell ()
  "Enable spell checking."
  (flyspell-mode -1))

(add-hook 'text-mode-hook 'chasinglogic-enable-flyspell)
(add-hook 'prog-mode-hook 'chasinglogic-disable-flyspell)

;; Automatically Do important programming stuff Emacs has a series of
;; modes that I call the "electric modes", as they all start with
;; `electric-'. All of these modes perform important editing functions
;; automatically.
;;
;; Electric indent mode on-the-fly reindents your code as you type. It
;; checks for newlines and other common chars that are configured via
;; the variable `electric-indent-chars'. This mode is invaluable and
;; saves me a lot of formatting time.
(electric-indent-mode 1)


;; Electric layout mode automatically inserts newlines around some
;; characters. The variable `electric-layout-rules' defines when and
;; how to insert newlines. The short of it is for many modes this auto
;; formats code.
(electric-layout-mode 1)

;; Electric pair mode automatically pairs common programming
;; operators: =(=, ={=, ="=, ='=, etc. I find this behavior annoying
;; in prose modes so I use a custom hook to only enable it for
;; programming modes.


(defun enable-electric-pair-local-mode ()
  "Enable eletric pair mode locally."
  (electric-pair-local-mode 1))
(add-hook 'prog-mode-hook 'enable-electric-pair-local-mode)

;; Show Paren Mode I'm just going to steal the description of this
;; straight from the documentation: Toggle visualization of matching
;; parens (Show Paren mode).
(show-paren-mode 1)


;; Diminish
;;
;; Diminish is a neat package that lets me easily hide minor modes
;; from the mode line. It also has a `use-package' keyword that lets
;; me do this for third party packages easily. Here we ensure that
;; it's available and diminish some common minor modes:
(use-package diminish
  :init
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode))

;; Paredit
;;
;; When talking about editing languages at a syntactic level one can't
;; help but think of paredit. It's simply the best way to write lisp
;; code. It adds two important to remember key bindings:
;;
;; | Key Bindings | Descripition |
;; |-------------------+---------------------------| C-M-<Right Arrow>
;; || Barf current expression | C-M-<Left Arrow> | Slurp current
;; |expression |
;;
;; Additionally I overwrite one mapping so that Paredit uses my
;; preferred `comment-actually-dwim' function instead of it's own that
;; mostly mimics the Emacs default.
(use-package paredit
  :bind (:map paredit-mode-map
              ;; I do not like any version of the original
              ;; `comment-dwim' and paredit has it's own special
              ;; version that I find more confusing. So overwrite it's
              ;; mapping with my `comment-actually-dwim' function.
              ("M-;" . comment-actually-dwim)))

;; Company Mode
;;
;; Company stands for COMPlete ANYthing and it does. I enable it
;; globally and diminish it since it is always on. I only set
;; `company-dabbrev-downcase' to nil. This ignores casing when
;; providing suggestions taken from inside the current buffer.
(use-package company
  :diminish ""
  :config
  (setq-default company-dabbrev-downcase nil)
  (global-company-mode))

;; LSP Mode
;;
;; LSP mode attempts to make Emacs as featureful as VSCode when it
;; comes to "IDE-esque" features. I would say it gets almost all the
;; way there. However I disable a lot of these features for
;; performance or visual disruption reasons. Even with most of these
;; UI elements disabled it provies the best completion and linting of
;; any package in the Emacs ecosystem. The best part is that it's a
;; single package so I don't have to maintain a milling =company-*=
;; and =flycheck-*= packages. It consists of two packages `lsp-mode'
;; itself that provides the Language Server interaction and `lsp-ui'
;; that provides the bulk of interactive features for the Language
;; Server. I only install `lsp-ui' for the Flycheck integration.
(use-package lsp-mode
  :init (setq-default lsp-auto-guess-root t
                      lsp-prefer-flymake nil)
  :commands 'lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq-default
   lsp-ui-doc-enable nil
   lsp-ui-peek-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-imenu-enable nil
   lsp-ui-flycheck-enable t))

;; LSP powered auto completion
;;
;; We need one more package to integrate LSP mode with my completion
;; framework Company, the cleverly named, `company-lsp'. All that we
;; need to do is add it to company backends.
(use-package company-lsp
  :config (push 'company-lsp company-backends)
  :after (lsp-mode company))

;; Flycheck
;;
;; Every good editor has syntax checking and Emacs is no different. I
;; use Flycheck for this since it's the most consistent, best
;; defaults, and functional package I've found for it. Flymake
;; recently got a rewrite in Emacs core but I still prefer Flycheck. I
;; do not install many =flycheck-*= packages as I use `lsp-mode' which
;; integrates with Flycheck and for everything else the Flycheck
;; defaults work great.
;;
;; I diminish Flycheck because it's almost always enabled so no reason
;; to pollute the mode-line. Additionally I set the variable
;; `flycheck-sh-posix-dash-executable' to an empty string. Most
;; people, I certainly didn't, don't know that there is a minimalistic
;; bash alternative called `dash' that is on a lot of Debian
;; systems. It's an awful shell IMO but Flycheck supports linting for
;; it. I use Dash.app on my Macbook and so Flycheck constanstly opens
;; Dash.app and freaks out whenever I'm in a `sh-mode' buffer. Setting
;; this to an empty string prevents Flycheck from trying to test Dash
;; shell syntax.
;;
;; I enable Flycheck for all text modes.
(use-package flycheck
  :diminish ""
  :general (leader!
             "el" 'flycheck-list-errors
             "ev" 'flycheck-verify-setup
             "ep" 'flycheck-previous-error
             "en" 'flycheck-next-error)
  :bind (("C-c e l" . flycheck-list-errors)
         ("C-c e v" . flycheck-verify-setup)
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error))
  :hook 'text-mode-hook
  :config
  ;; this trys to run the dash shell which I don't use but instead
  ;; opens the Dash.app program which I do use.
  (setq flycheck-sh-posix-dash-executable ""))

;; Flycheck Vale
;;
;; The only additional Flycheck linter package I install is Flycheck
;; Vale. This integrates the awesome
;; [[https://github.com/errata-ai/vale][Vale prose linter]] with
;; Flycheck. I use this for all my prose.
(use-package flycheck-vale
  :after 'flycheck
  :config
  (flycheck-vale-setup))

;; Highlight TODO mode
;;
;; By default Emacs doesn't highlight TODO comments. This makes them
;; stand out by fontifying them the same as Org mode TODO header
;; keywords.
(use-package hl-todo
  :demand
  :config
  (global-hl-todo-mode))

;; Writeroom Mode
;;
;; Writeroom Mode is a simple but great package that provides a
;; focused editing experience. It removes all chrome and centers the
;; buffer on the window so you can focus only on the prose.
(use-package writeroom-mode :commands (writeroom-mode))


;; anzu-mode enhances isearch & query-replace by showing total matches
;; and current match position
(use-package anzu
  :diminish ""
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace))
  :config (global-anzu-mode))

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


;; Dired
;;     I use dired as my primary file manager for anything that isn't
;;     multimedia content (videos, photos, music). I really love it and
;;     some kinds of file operations are simply not possible without it.
;;     First we require `dired-x'. Dired-X provides many extra features
;;     to Dired that take it from nice to unparalleled. See [[info:dired-x#Features][Dired-X
;;     Features]] for a full list with more info.
(require 'dired-x)
;; Now we set the variable `dired-dwim-target' to `t'. This makes it
;; such that when operating on files in Dired the target of the
;; operation will automatically suggest other Dired buffers as the
;; target preferring buffers that are visible. It's super handy.
(setq-default dired-dwim-target t)
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; Ediff
;;     Ediff is a handy tool I don't use often enough. However I really
;;     hate the default layout. This makes Ediff less eggregious about
;;     upsetting my window manager when I load it.
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; Evergreen CI integration
;;
;;     This is one of my personal packages. At MongoDB we run our in house
;;     CI system and this package integrates it into Emacs. We don't
;;     maintain an in-house ELPA repository so I recommend, and myself do,
;;     installing it with Quelpa.
;; (use-package evergreen
;;   :quelpa (evergreen :repo "evergreen-ci/evergreen.el" :fetcher github)
;;   :commands (evergreen-patch evergreen-list-spawn-hosts)
;;   :config
;;   (setq-default evergreen-generate-description t
;;                 evergreen-finalize-when-patching t
;;                 evergreen-browse-when-patching t
;;                 evergreen-default-project "mongodb-mongo-master"
;;                 evergreen-assume-yes t))

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

;; Finally enable LSP mode in Python buffers and make Emacs treat
;; SCons build configuration files as python.
(defun chasinglogic-enable-lsp ()
  "Enable LSP for all non SCons* files"
  (when (not (string-match "SCons.*" (buffer-file-name)))
    (lsp)))

(add-hook 'python-mode-hook 'chasinglogic-enable-lsp)
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
  (add-hook 'before-save-hook 'clang-format-buffer nil t)
  (lsp))

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

(require 'chasinglogic-frames)
(require 'chasinglogic-utils)
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

;; Set gc-cons-threshold back to it's default value.
(setq gc-cons-threshold 800000)
