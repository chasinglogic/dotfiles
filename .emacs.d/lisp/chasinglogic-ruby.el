;;; chasinglogic-ruby.el --- Ruby language setup

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

(provide 'chasinglogic-ruby)

;;; chasinglogic-ruby.el ends here
