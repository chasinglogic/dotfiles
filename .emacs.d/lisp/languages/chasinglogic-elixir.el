;;; chasinglogic-elixir.el --- My Emacs config for the Elixir language

;; Copyright (C) 2023 Mathew Robinson

;; Author: Mathew Robinson <mathewrobinson@TL-TM6CFN745K>
;; Created: 23 May 2023

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

(use-package elixir-mode
  :mode ("\\.exs\\'" "\\.ex\\'")
  :config
  (add-hook 'elixir-mode-hook 'chasinglogic-use-tree-sitter-hl))

(provide 'chasinglogic-elixir)

;;; chasinglogic-elixir.el ends here
