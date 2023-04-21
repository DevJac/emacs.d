;;; straight.el bootstrap
;; See: https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; straight integration with use-package
;; See: https://github.com/radian-software/straight.el#integration-with-use-package
;; We want all normal (use-package ...) expressions to use straight by default
(setq straight-use-package-by-default t)
;; Before we can use normal (use-package ...) expressions, we need to get use-package
(straight-use-package 'use-package)

;;; use-package's
;; :init runs before the package is loaded
;; :config runs after the package is loaded
(use-package evil
  :config
  (evil-mode 1))
