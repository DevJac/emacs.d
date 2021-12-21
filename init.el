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

;;; File config
(global-auto-revert-mode 1)
(setq make-backup-files nil)

;;; Packages
(straight-use-package 'use-package)
(use-package restart-emacs ; keep first, useful in case remaining config is bad
  :straight t)
(use-package gruvbox-theme ; keep early, we want to set the theme ASAP
  :straight t
  :config
  (load-theme 'gruvbox-dark-hard t))
(use-package counsel
  :straight t
  :config
  (counsel-mode 1))
(use-package evil
  :straight t
  :load-path "elisp"
  :config
  (load "evil-keys")
  (evil-mode 1))
(use-package ivy
  :straight t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist
	'((counsel-M-x . "")
          (counsel-describe-function . "")
          (counsel-describe-variable . "")))
  (global-set-key (kbd "C-s") #'swiper)
  (ivy-mode 1))
(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))
