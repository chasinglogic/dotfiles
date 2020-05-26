

  ;;; Copy the test path for Django manage.py test
(defun chasinglogic-copy-mpbx-test-path-for-here (&optional copy)
  (interactive (list t))
  (let* ((file-name (if (projectile-project-root)
                        (file-relative-name (buffer-file-name) (projectile-project-root))
                      (file-name-nondirectory (buffer-file-name))))
         (current-point (point))
         (test-name (progn
                      (re-search-backward "def ")
                      (forward-word)
                      (forward-char)
                      (thing-at-point 'sexp)))
         (test-class-name (progn
                            (re-search-backward "class ")
                            (forward-word)
                            (forward-char)
                            (thing-at-point 'sexp)))
         (test-path (concat
                     (string-remove-prefix "mpb/" file-name)
                     ":" test-class-name
                     "." test-name)))
    (message "Copied: %s" test-path)
    (kill-new test-path)
    (goto-char current-point)))

(defun chasinglogic-run-mpbx-test ()
  "Run the test under point"
  (interactive)
  (chasinglogic-copy-mpbx-test-path-for-here t)
  (let ((default-directory (projectile-project-root))
        (compilation-command (concat "cd mpb && python ./manage.py test " (current-kill 0))))
    (puthash default-directory compilation-command projectile-compilation-cmd-map)
    (compile compilation-command)))

  ;;; Copy the test path for Django manage.py test
(defun chasinglogic-copy-mpb-test-path-for-here (&optional full-file)
  "Copy the test path of the test under cursor for pytest.

FULL-FILE indicates the full file-name should be copied instead of
just the singular test."
  (interactive)
  (let* ((file-name (if (projectile-project-root)
                        (file-relative-name (buffer-file-name) (projectile-project-root))
                      (file-name-nondirectory (buffer-file-name))))
         (current-point (point))
         (test-name (progn
                      (re-search-backward "def ")
                      (forward-word)
                      (forward-char)
                      (thing-at-point 'sexp)))
         (test-class-name (progn
                            (re-search-backward "class ")
                            (forward-word)
                            (forward-char)
                            (thing-at-point 'sexp)))
         (test-path (concat
                     file-name
                     (unless full-file
                       (concat
                        "::" test-class-name
                        "::" test-name)))))
    (message "Copied: %s" test-path)
    (kill-new test-path)
    (goto-char current-point)))

(defun chasinglogic-run-mpb-test ()
  "Run test under point."
  (interactive)
  (chasinglogic-copy-test-path-for-here)
  (let ((default-directory (projectile-project-root))
        (compilation-command (concat "./common/scripts/tests " (current-kill 0))))
    (puthash default-directory compilation-command projectile-compilation-cmd-map)
    (compile compilation-command)))
