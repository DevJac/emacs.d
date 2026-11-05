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

(let ((errors 0))
  (dolist (file '("config/melpa.el"
		  "config/evil.el"))
    (unless (my/load-init-file file)
      (cl-incf errors)))
  (if (zerop errors)
      (message "[init] All configuration files loaded successfully.")
    (message "[init] %d configuration file(s) failed to load." errors)))
