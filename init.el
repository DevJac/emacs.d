;; MELPA has more up-to-date packages
;; See: https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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

(dolist (file '("lisp/config-emacs.el"
                "lisp/config-consult.el"
                "lisp/config-evil.el"))
  (dj/load-init-file file))
