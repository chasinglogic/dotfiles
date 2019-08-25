;;; chasinglogic-hydras.el --- Hydras for use with hydra.el

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

(provide 'chasinglogic-hydras)

;;; chasinglogic-hydras.el ends here
