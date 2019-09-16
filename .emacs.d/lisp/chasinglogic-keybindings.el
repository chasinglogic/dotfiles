;;; chasinglogic-keybindings.el --- Global Emacs keybindings

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

;; Global Keybindings These are my global keybindings that bind my
;; custom commands or rebind Emacs defaults. I also document what the
;; default binding was if I overwrite one so I can remember if I ever
;; want or need to go back during a live session.

;;; Code:

(bind-key "C-c j b" 'chasinglogic-copy-breakpoint-for-here)
(bind-key "C-c j =" 'chasinglogic-indent-buffer)
(bind-key "C-c f r" 'chasinglogic-rename-file-and-buffer)
(bind-key "C-c f D" 'chasinglogic-delete-current-buffer-file)
(bind-key "C-x 5 o" 'chasinglogic-select-frame-by-name)

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

(bind-key "M-/" 'hippie-expand)

(bind-key "M-t" 'switch-to-buffer)
(bind-key "C-t" 'projectile-find-file)

(provide 'chasinglogic-keybindings)

;;; chasinglogic-keybindings.el ends here
