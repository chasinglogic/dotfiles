;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's
;;      modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;window-select     ; visually switch windows
       zen               ; distraction-free coding or writing

       :editor
       (evil

        +everywhere)  ; come to the dark side, we have cookies
       (format +onsave)    ; automated prettiness
       snippets            ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       spell               ; tasing you for misspelling mispelling
       grammar             ; tasing grammar mistake every you make

       :tools
       editorconfig        ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       lsp
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs

       :lang
       cc             ; C/C++/Obj-C madness
       ;;clojure      ; java with a lisp
       data           ; config/data formats
       emacs-lisp     ; drown in parentheses
       json           ; At least it ain't XML
       javascript     ; all(hope(abandon(ye(who(enter(here))))))
       markdown       ; writing docs for people to ignore
       (org +hugo
            +journal
            +present
            +roam)            ; organize your plain life in plain text
       (python +lsp)  ; beautiful is better than ugly
       rest           ; Emacs as a REST client
       rst            ; ReST in peace
       (rust +lsp)    ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh             ; she sells {ba,z,fi}sh shells on the C xor
       web            ; the tubes
       yaml           ; JSON, but readable

       :email
       ;;(mu4e +gmail)

       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       (default +bindings +smartparens))
