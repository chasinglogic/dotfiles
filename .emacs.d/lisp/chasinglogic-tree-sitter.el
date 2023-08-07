;;; chasinglogic-tree-sitter.el --- Configuration for using Emacs as my mail client.

;; Copyright (C) 2023 Mathew Robinson

;; Author: Mathew Robinson <mathew@chasinglogic.io>
;; Created: 24 May 2023

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


(if (version< emacs-version "29")
    (progn
      (use-package tree-sitter)
      (use-package tree-sitter-langs
        :init
        (defun chasinglogic-use-tree-sitter-hl ()
          (tree-sitter-hl-mode 1))

        (global-tree-sitter-mode)))

  (progn

    (setq treesit-language-source-alist
          '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
            (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
            (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
            (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
            (cmake      "https://github.com/uyha/tree-sitter-cmake")
            (css        "https://github.com/tree-sitter/tree-sitter-css")
            (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")
            (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
            (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
            (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
            (go         "https://github.com/tree-sitter/tree-sitter-go")
            (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
            (html       "https://github.com/tree-sitter/tree-sitter-html")
            (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
            (json       "https://github.com/tree-sitter/tree-sitter-json")
            (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
            (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
            (make       "https://github.com/alemuller/tree-sitter-make")
            (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
            (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
            (python     "https://github.com/tree-sitter/tree-sitter-python")
            (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
            (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
            (toml       "https://github.com/tree-sitter/tree-sitter-toml")
            (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
            (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

    (defun chasinglogic-tree-sitter-mode-mapping (lang)
      "Create a cons of major-mode to major-ts-mode"
      (cons (make-symbol (format "%s-mode" lang))
            (make-symbol (format "%s-ts-mode" lang))))

    (setq major-mode-remap-alist
          (mapcar 'chasinglogic-tree-sitter-mode-mapping
                  (mapcar 'car treesit-language-source-alist)))

    (defun chasinglogic-install-all-grammars ()
      (interactive)
      (mapc #'treesit-install-language-grammar
            (mapcar #'car treesit-language-source-alist)))))

(provide 'chasinglogic-tree-sitter)

;;; chasinglogic-tree-sitter.el ends here
