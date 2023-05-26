;;; chasinglogic-projects.el --- The project interaction package for Emacs

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

(require 'project)

;; Projector => Projects integration
;;
;;     I maintain (what I think) is a pretty cool tool called [[https://github.com/chasinglogic/projector][Projector]]
;;     and this "integrates" it with projects. Simply put it seeds
;;     Projects's known project list with the list of projects that
;;     Projector knows about. It's really nice when on a new machine that
;;     has all my repositories but since I haven't visited them I can't
;;     quickly switch to them.
(defun chasinglogic-add-projector-projects-to-projectel ()
  "Add projector projects to projects."
  (interactive)
  (setq
    project--list
    (mapcar 
     #'list
     (sort
    (delete ""
            (split-string
              (shell-command-to-string "projector list") "\n"))
    #'(lambda (a b) (< (length a) (length b))))))
  (project--write-project-list))

(chasinglogic-add-projector-projects-to-projectel)

(provide 'chasinglogic-projects)

;;; chasinglogic-projects.el ends here
