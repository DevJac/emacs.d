;; Profile startup with use-package; run use-package-report to see results
;; (setq use-package-compute-statistics t)

;; I want to break my config into many files. If there is an error in
;; one file I want to continue trying to load other files.
(defun dj/load-init-file (file)
  "Load FILE from `user-emacs-directory'.
If an error occurs, log it, and then continue."
  (let ((full-file (expand-file-name file user-emacs-directory)))
    (condition-case err
        (load full-file nil 'nomessage)
      (error
       (message "[init] Error loading %s: %s"
                file
                (error-message-string err))
       nil))))

(dolist (file '("lisp/config-melpa.el"
                "lisp/config-emacs.el"
                "lisp/config-theme.el"
                "lisp/config-my.el"
                "lisp/config-org.el"
                "lisp/config-consult.el"
                "lisp/config-evil.el"
                "lisp/config-expreg.el"
                "lisp/config-evil-keys.el"
                "lisp/config-other-packages.el"
                "lisp/config-gptel.el"
                "lisp/config-c-mode.el"
                "lisp/config-initial-buffer.el"))
  (dj/load-init-file file))
