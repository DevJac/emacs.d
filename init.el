;; -*- lexical-binding: t; -*-

(defun my/load-init-file (file)
  "Load FILE from `user-emacs-directory'.
If an error occurs, report it and continue."
  (let ((full-file (expand-file-name file user-emacs-directory)))
    (condition-case err
        (load full-file nil 'nomessage)
      (error
       (display-warning
        'init
        (format "Error loading %s: %s"
                file (error-message-string err))
        :error)
       nil))))

;; Store customizations in another file to keep init.el clean
;; The custom-file variable is not a variable I created
;; See `C-h v custom-file`
(setq custom-file (locate-user-emacs-file "custom-sets.el"))
(load custom-file 'noerror 'nomessage)

(let ((errors 0))
  (dolist (file '("config/melpa.el"
                  "config/emacs.el"
                  "config/theme.el"
                  "config/evil.el"
                  "config/ai.el"
                  "config/annotate.el"
                  "config/consult.el"
                  "config/expreg.el"
                  "config/org.el"
                  "config/other-packages.el"
                  "config/total-review.el"
                  "config/whitespace.el"
                  "config/evil-keys.el"))
    (unless (my/load-init-file file)
      (cl-incf errors)))
  (if (zerop errors)
      (message "[init] All configuration files loaded successfully.")
    (message "[init] %d configuration file(s) failed to load." errors)))
