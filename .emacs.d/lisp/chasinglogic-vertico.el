;;; chasinglogic-vertico.el --- Configuration for Vertico mode

;; Copyright (C) 2023 Mathew Robinson

;; Author: Mathew Robinson <mathewrobinson@TL-TM6CFN745K>
;; Created: 05 Aug 2023

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


(defvar +vertico-company-completion-styles '(basic partial-completion orderless)
  "Completion styles for company to use.

The completion/vertico module uses the orderless completion style by default,
but this returns too broad a candidate set for company completion. This variable
overrides `completion-styles' during company completion sessions.")

(defvar +vertico-consult-fd-args nil
  "Shell command and arguments the vertico module uses for fd.")

;;
;;; Packages

(use-package vertico
  :init
  (vertico-mode)
  :bind 
  (:map vertico-map
        ( "M-RET" . vertico-exit-input )
        ( "C-j"   . vertico-next )
        ( "C-M-j" . vertico-next-group )
        ( "C-k"   . vertico-previous )
        ( "C-M-k" . vertico-previous-group ))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (general-def :map vertico-map "DEL" #'vertico-directory-delete-char))


(use-package orderless
  :config
;;   (defadvice! +vertico--company-capf--candidates-a (fn &rest args)
;;     "Highlight company matches correctly, and try default completion styles before
;; orderless."
;;     :around #'company-capf--candidates
;;     (let ((orderless-match-faces [completions-common-part])
;;           (completion-styles +vertico-company-completion-styles))
;;       (apply fn args)))

  (defun chasinglogic-vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-style-dispatchers '(chasinglogic-vertico-orderless-dispatch)
        orderless-component-separator "[ &]")

  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))


(use-package consult
  :init
  (general-def
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop)
  :config
  (recentf-mode 1)

  (setq consult-project-root-function #'doom-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1))


(use-package consult-flycheck
  :after (consult flycheck))

(use-package embark
  :bind
  (:map minibuffer-local-map
        ( "C-;"     . embark-act )
        ( "C-c C-;" . embark-export )
        ( "C-c C-l" . embark-collect)
        ( "C-c C-e" . chasinglogic-embark-export-write))
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :config
  (defun chasinglogic-embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.

  Supports exporting consult-grep to wgrep"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (let* ((edit-command
            (pcase-let ((`(,type . ,candidates)
                        (run-hook-with-args-until-success 'embark-candidate-collectors)))
              (pcase type
                ('consult-grep #'wgrep-change-to-wgrep-mode)
                ('consult-ripgrep #'wgrep-change-to-wgrep-mode)
                (x (user-error "embark category %S doesn't support writable export" x)))))
          (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
      (embark-export)))

  (require 'consult))

(use-package embark-consult
  :after (embark consult))


(use-package marginalia
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode)
  :bind (:map minibuffer-local-map
  ("M-A" . #'marginalia-cycle)))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(provide 'chasinglogic-vertico)

;;; chasinglogic-vertico.el ends here
