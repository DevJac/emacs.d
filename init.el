;; MELPA has more up-to-date packages
;; See: https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
       (unless noninteractive
	 (sit-for 2)) ;; give myself extra time to see the error
       nil))))

(dolist (file '("lisp/config-evil.el"))
  (dj/load-init-file file))
