  ;; Org Refile
  ;;
  ;; I use org refile to organize tasks from my =inbox.org= file to my
  ;; agenda files or notes. I also use it to refile my notes between
  ;; headings in =notes.org.gpg=. These settings do the following
  ;; things:
  ;;
  ;;  - Add org agenda files to the refile targets.
  ;;  - Include the filename in the refile target path, this allows
  ;;    creating new top level headings in files via refile.
  ;;  - Enable creating new nodes via refile.
  ;;  - Disable complete-in-steps and let helm do the filtering.
  (setq-default org-refile-targets '((nil :maxlevel . 1)
                                     (org-agenda-files :maxlevel . 2))
                org-refile-use-outline-path 'file
                org-outline-path-complete-in-steps nil
                org-refile-allow-creating-parent-nodes 'confirm)
