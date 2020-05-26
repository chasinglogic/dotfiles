;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mathew Robinson"
      user-mail-address "mathew@chasinglogic.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light
      chasinglogic-light-theme 'doom-solarized-light
      chasinglogic-dark-theme 'doom-solarized-dark)

(defun chasinglogic-toggle-theme ()
  (interactive)
  (if (eq doom-theme chasinglogic-light-theme)
      (setq doom-theme chasinglogic-dark-theme)
    (setq doom-theme chasinglogic-light-theme))
  (doom/reload-theme))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(after! projectile
  ;; I prefer a git status when switching to a project
  (setq projectile-switch-project-action 'magit-status)
  (setq counsel-projectile-switch-project-action 'magit-status)
  (map! :leader
        (:prefix-map ("p" . "project")
         :desc "Search the Project" "s" #'counsel-projectile-rg)))

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
(chasinglogic-add-projector-projects-to-projectile)

(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

(after! evil
  ;; I use this binding alot.
  (map! :n "-" #'(lambda () (interactive) (dired ".")))
  (map!
   :localleader
   :map python-mode-map
   :prefix "t"
   "x" 'chasinglogic-run-mpbx-test
   "r" 'chasinglogic-run-mpb-test)
  (map! :leader :r "<SPC>" 'counsel-M-x))


  ;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun chasinglogic-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(after! org
  (load! "org.el"))


(load! "work.el")
