;;; chasinglogic-globals.el --- Global variables used throughout Emacs

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

;; Anything that is a built in Emacs global variable that doesn't
;; relate to a major mode goes here.  These settings effect the
;; editing experience of Emacs. Things like tabs/spaces etc. are
;; controlled here.

;;; Code:

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

(provide 'chasinglogic-globals)

;;; chasinglogic-globals.el ends here
