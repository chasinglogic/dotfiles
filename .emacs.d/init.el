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
(let ((gc-cons-threshold 100000000))
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
                 '("elpa" . "http://elpa.gnu.org/packages/")
                 '("melpa" . "http://melpa.org/packages/")))
  (package-initialize)

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
      (package-install 'use-package))
    (require 'use-package))

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
    (exec-path-from-shell-initialize))

  ;; Add the lisp directory where all other files are required from to
  ;; the load path.
  (add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp"))

  ;; Load all of my other configuration files. Order matters.
  (eval-and-compile
    (setq-default mu4e-load-path (if (eq system-type 'darwin)
                                     "/usr/local/share/emacs/site-lisp/mu/mu4e"
                                   "/usr/share/emacs/site-lisp/mu4e")))
  (eval-and-compile
    (require 'chasinglogic-globals)
    (require 'chasinglogic-frames)
    (require 'chasinglogic-ui)
    (require 'chasinglogic-utils)
    (require 'chasinglogic-util-packages)
    (require 'chasinglogic-keybindings)
    (require 'chasinglogic-ivy)
    (require 'auto-sync-mode)
    (require 'chasinglogic-minor-modes)
    (require 'chasinglogic-org)
    (require 'chasinglogic-major-modes)
    (require 'chasinglogic-hydras)
    (when (file-exists-p mu4e-load-path)
      (require 'chasinglogic-email)))

  ;; Post initialization
  ;;
  ;; These are the few final steps we should take when bringing up
  ;; Emacs.
  ;;
  ;; First Maximize this frame, the initial frame won't see our hooks in
  ;; `make-frame-init-functions'.
  (maximize-gui-frames (selected-frame))

  ;; Finally start the Emacs server, this allows connecting terminal and
  ;; other Emacs clients to this GUI editing session for easy sharing of
  ;; information.
  (require 'server)
  (unless (server-running-p)
    (server-start)))
