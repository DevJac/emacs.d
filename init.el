;;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Theme config
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(set-face-attribute 'default nil :height 160)
(setq scroll-margin 2)
(setq scroll-conservatively 100)
(setq next-screen-context-lines 4)

;;; Packages
(straight-use-package 'use-package)
(use-package restart-emacs ; keep first, useful in case remaining config is bad
  :straight t)
(use-package gruvbox-theme ; keep early, we want to set the theme ASAP
  :straight t
  :config
  (load-theme 'gruvbox-dark-hard t))
(use-package evil
  :straight t
  :config
  (evil-mode 1))
