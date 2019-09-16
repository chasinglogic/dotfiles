;;; chasinglogic-email.el --- Configuration for using Emacs as my mail client.

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


;; Tell Emacs my full name, my primary Email address, and my Email signature.
(setq-default user-full-name "Mathew Robinson"
              user-mail-address "mathew@chasinglogic.io"
              message-signature "- Mathew Robinson @chasinglogic")

;; mu4e (Email)
;;
;; I read my Email in Emacs using the excellent mu tool and it's
;; companion Emacs client mu4e (mu 4 Emacs). It is not packaged on any
;; ELPA repository and has to be installed with the companion tool via
;; the system package manager. I have ansible playbooks that do this
;; for me but it's not always there the first time I run Emacs. We
;; dynamically find the load path so it uses the correct location on
;; MacOS and Linux.
(use-package mu4e
  :load-path mu4e-load-path
  :bind ("C-c m" . mu4e)
  :config

  ;; mu4e: General settings
  ;;
  ;; These settings change mu4e global behavior or how emails are
  ;; displayed.
  ;;
  ;; I use mbsync to sync my Emails into the Maildir =~/Mail=. We
  ;; have to configure mu4e to look at this directory for Emails.
  (setq-default mu4e-maildir "~/Mail")

  ;; I use a SystemD timer to run mbsync every five
  ;; minutes. Fetching mail from mu4e causes Emacs to lock up since
  ;; the fetch command can take a long time. So set the fetch mail
  ;; command to `true' so it always exits 0 quickly.
  (setq-default mu4e-get-mail-command "true")

  ;; Don't ask me if I really want to quit when quitting mu4e, it's
  ;; not a big deal to re-launch it if I do quit on
  ;; accident. Additionally make it so message cleans up it's
  ;; compose buffers when I leave them.
  (setq-default mu4e-confirm-quit nil
                message-kill-buffer-on-exit t)

  ;; I do not like any Emails to show in the headers view unless
  ;; it's actually an email that matched by query. These setting
  ;; prevent mu4e from pulling all threaded / related emails into
  ;; the view.
  (setq-default mu4e-headers-show-threads nil
                mu4e-headers-include-related nil
                mu4e-context-policy 'pick-first)

  ;; Show headers on the top and messages on the bottom. I prefer
  ;; this view because otherwise the headers wrap or important
  ;; information is obscured.
  (setq-default mu4e-split-view 'horizontal)

  ;; Delete sent messages after they're sent, both my Email
  ;; providers keep these for me and I have the threads anyway so I
  ;; don't care about saving these locally.
  (setq-default mu4e-sent-messages-behavior 'delete)

  ;; Set display options for mu4e. First we show Email addresses in
  ;; view mode. We additionally render images by default. Finally we
  ;; use UTF-8 characters for indicating the flags set on a message
  ;; in the headers view.
  (setq-default mu4e-view-show-images t
                mu4e-view-show-addresses t
                mu4e-use-fancy-chars t)

  ;; Send emails as format=flowed to play nicer with everyone elses
  ;; text based email clients.
  (setq-default mu4e-compose-format-flowed t)

  ;; Update the index every 300 seconds (every 5 minutes). Index
  ;; updates are usually very fast, this timer is meant to try and
  ;; keep up with the rate at which SystemD runs mbsync.
  (setq-default mu4e-update-interval 300)

  ;; This is actually a basic Emacs setting as mu4e does not send
  ;; email. However I never use Emacs for email outside of a mu4e
  ;; context. These variables make Emacs always use smtp to send
  ;; emails instead of prompting me for how I want to send it.
  (setq-default message-send-mail-function 'message-smtpmail-send-it
                send-mail-function 'smtpmail-send-it)

  ;; Emacs is technically a text based email client and mu4e will
  ;; prefer that format if available. However the web has moved on
  ;; in a bad way to crazy HTML emails. Luckily Emacs has a built in
  ;; web browser EWW. This tells mu4e to render HTML emails using
  ;; the EWW engine. Additionally I add a hook so I can use <tab>
  ;; and <backtab> to traverse forward and backward to hyper links.
  (setq-default shr-use-colors nil
                shr-color-visible-luminance-min 100
                mu4e-html2text-command 'mu4e-shr2text)

  ;; Emulate some of eww key-bindings for moving between links
  (defun chasinglogic-next-link (&optional ARG)
    "Move forward to the next URL in buffer. If ARG is non-nil
move backwards."
    (interactive)
    (let ((regex "http\(s\)?://"))
      (if ARG
          (re-search-backward regex nil t 1)
        (re-search-forward regex nil t 1))))
  (add-hook 'mu4e-view-mode-hook
            (lambda()
              (local-set-key (kbd "<tab>") 'chasinglogic-next-link)
              (local-set-key (kbd "<backtab>") '(lambda () (chasinglogic-next-link t)))))

  ;; Set the email user agent to mu4e.
  (setq-default mail-user-agent 'mu4e-user-agent)

  ;; Integrate mu4e with my frame naming system.
  (add-hook 'mu4e-main-mode-hook '(lambda () (set-frame-name "Email")))

  ;; mu4e: Email Contexts
  ;;
  ;; The killer feature of mu4e is that it supports multiple
  ;; accounts seamlessly. It can automatically set the context on a
  ;; per messsage basis by checking metadata about that message.
  ;;
  ;; `mu4e-contexts' is a list of context objects as created by
  ;; `make-mu4e-context'. `make-mu4e-context' takes many keyword
  ;; arguments but I only use a few:
  ;;
  ;; - =:name=: The name of this context, will be prompted using
  ;; this mu4e isn't sure which to use.  - =:match-func=: A function
  ;; which takes a message object as input and returns `nil' or `t'
  ;; indicating this is the context to use.  - =:vars=: A list of
  ;; cons cells that set variables on "context local" level.
  ;;
  ;; I use the following folders for the different kinds of local
  ;; mail operations: =/context_name/drafts=, =/context_name/sent=,
  ;; =/context_name/trash=, =/context_name/archive=. The only other
  ;; context specific vars relate to sending email.
  (setq-default mu4e-contexts
                `(
                  ;; I define my Work Email context
                  ;; here. This is my most used email
                  ;; interface.
                  ,(make-mu4e-context
                    :name "Work"
                    :match-func (lambda (msg)
                                  (when msg
                                    (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
                    :vars '(
                            (mu4e-drafts-folder . "/work/drafts")
                            (mu4e-sent-folder . "/work/sent")
                            (mu4e-trash-folder . "/work/trash")
                            (mu4e-refile-folder . "/work/archive")
                            (smtpmail-stream-type . nil)
                            (smtpmail-local-domain . "gmail.com")
                            (smtpmail-default-smtp-server . "smtp.gmail.com")
                            (smtpmail-smtp-server . "smtp.gmail.com")
                            (smtpmail-smtp-service . 587)
                            (smtpmail-smtp-user . "mathew.robinson@10gen.com")
                            (user-mail-address . "mathew.robinson@mongodb.com")
                            )
                    )

                  ;; Personal Email context. This is determines if I'm
                  ;; viewing an email from my personal account.
                  ,(make-mu4e-context
                    :name "Personal"
                    :match-func (lambda (msg)
                                  (when msg
                                    (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
                    :vars '(
                            (mu4e-drafts-folder . "/personal/Drafts")
                            (mu4e-sent-folder . "/personal/Sent")
                            (mu4e-trash-folder . "/personal/Trash")
                            (mu4e-refile-folder . "/personal/Archive")
                            (smtpmail-stream-type . ssl)
                            (smtpmail-smtp-server . "smtp.fastmail.com")
                            (smtpmail-smtp-service . 465)
                            (smtpmail-local-domain . "chasinglogic.io")
                            (smtpmail-smtp-user . "mathew@chasinglogic.io")
                            (user-mail-address . "mathew@chasinglogic.io")
                            )
                    )))

  ;; mu4e: Bookmarks
  ;;
  ;; mu4e doesn't have folders. It's all about searching and
  ;; filtering email. So you shift the paradigm from how can I
  ;; organize my email to how can I view exactly what I need. The
  ;; easy way to quickly repeat or access filters is with
  ;; bookmarks. You get prompted for this whenever you start
  ;; mu4e. They are created via `make-mu4e-bookmark' which takes
  ;; three keyword arguments: =:name=, =:query=, and =:key=. These
  ;; are self explanatory. See the [[man:mu-find][mu-find man page]]
  ;; for an explanation of the query language but it's quite
  ;; flexible.
  ;;
  ;; I define four bookmarks:

  ;;  - Inbox: This is my primary email interface it shows me any
  ;;    unread or flagged email in my Inbox.
  ;;
  ;;  - Unread Messages: Occasionally I want to see what I never read
  ;;    but archived. This filter shows me all unread messages.
  ;;
  ;;  - Flagged: mu4e flagging is similar to starring in other email
  ;;    clients. I flag things if I need to take some follow up action
  ;;    and didn't add it to my todo list.
  ;;
  ;;  - Today's Messages: When I come back from vacation my Inbox will
  ;;    be overflowing. Since messages that came in today are more
  ;;    relevant I like to work through all of them first before
  ;;    getting to the rest of my email.
  (setq-default mu4e-bookmarks
                `(
                  ,(make-mu4e-bookmark
                    :name  "Inbox"
                    :query "(maildir:/personal/INBOX OR maildir:/work/INBOX)"
                    :key ?i)
                  ,(make-mu4e-bookmark
                    :name  "Unread messages"
                    :query "flag:unread AND NOT flag:trashed"
                    :key ?u)
                  ,(make-mu4e-bookmark
                    :name "Flagged (Starred)"
                    :query "flag:flagged"
                    :key ?f)
                  ,(make-mu4e-bookmark
                    :name "Today's messages"
                    :query "(date:today..now)"
                    :key ?t)
                  ))

  ;; mu4e: Composing Email
  ;;
  ;; Composing and sending email in Emacs is a mix of mu4e and
  ;; default Emacs packages. We set a few hooks for composing
  ;; emails. We make it so emails are automatically wrapped at 80
  ;; characters and set it so emails are signed when being sent.
  (add-hook 'mu4e-compose-mode-hook 'auto-fill-mode)
  (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign))

;; mu4e alert
;;
;; mu4e alert is a must have package if using mu4e. It periodically
;; updates the mu index and if new messages are found by the
;; interesting mail query then it sends a dbus
;; notification. Additionally it adds an unread email count to the
;; mode line.
(use-package mu4e-alert
  :after mu4e
  :config
  (setq mu4e-alert-interesting-mail-query "(flag:unread maildir:/personal/Inbox) OR (flag:unread maildir:/work/INBOX)")
  (when (eq system-type 'gnu/linux)
    (mu4e-alert-set-default-style 'libnotify))
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(provide 'chasinglogic-email)

;;; chasinglogic-email.el ends here
