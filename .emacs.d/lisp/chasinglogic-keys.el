;;; chasinglogic-keys.el --- My Keybindings and Utility functions

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
  ;; Indent the buffer
  ;;
  ;;     This function uses Emacs built in indent facilities to indent the
  ;;     entire buffer. It doesn't work so great on languages where
  ;;     whitespace has semantic meaning, like Python, but it is a godsend
  ;;     for structured languages that are commonly poorly formatted, like
  ;;     HTML.
  (defun chasinglogic-indent-buffer ()
    "Indent the entire buffer."
    (interactive)
    (indent-region-line-by-line (point-min) (point-max)))

  ;; GDB/LLDB Debugging
  ;;     I maintain a developer toolchain that means I have to frequently
  ;;     interact with GDB *and* LLDB. Since LLDB does not have Emacs
  ;;     integration this function allow me to easily get breakpoints for
  ;;     wherever I am.
  ;;
  ;;     It checks for the `projectile-project-root' and if found will make
  ;;     the filename relative to this directory. Otherwise the full path
  ;;     of the `buffer-file-name' will be used. It grabs the line number
  ;;     the point is currently at then simply concatenates the generated
  ;;     filename, a colon, and the line number. When called interactively
  ;;     it will add it to the kill ring effectively "copying" the
  ;;     breakpoint for easy pasting.
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

  ;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
  (defun chasinglogic-unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  ;; Finding org files
  ;;
  ;;     I keep all of my org files in `org-directory' and some of them are
  ;;     encrypted. This macro lets me easily define functions for quickly
  ;;     finding them. It's a macro because [[https://www.jamesporter.me/2013/06/14/emacs-lisp-closures-exposed.html][Emacs has crazy scoping rules]]
  ;;     that make returning lambdas from functions difficult.
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
  
  (general-def "C-c j b" 'chasinglogic-copy-breakpoint-for-here)
  (general-def "C-c j =" 'chasinglogic-indent-buffer)
  (general-def "C-c f r" 'chasinglogic-rename-file-and-buffer)
  (general-def "C-c f D" 'chasinglogic-delete-current-buffer-file)
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
  (general-nmap
    "-" #'(lambda () (interactive) (dired "."))
    "gcc" 'comment-actually-dwim
    "<tab>" 'indent-according-to-mode)

  ;; Some of the bindings are not setup correctly in Emacs terminal
  ;; mode. It uses a different kbd identifier for some reason.
  (unless (display-graphic-p)
    (general-nmap
      "ESC" '(lambda () (interactive) (evil-escape-func))
      "TAB" 'indent-according-to-mode))

  (general-vmap "gc" 'comment-or-uncomment-region)

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
    "wr" 'window-configuration-to-register
    "wf" 'make-frame
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

(provide 'chasinglogic-keys)

;;; chasinglogic-keys.el ends here
