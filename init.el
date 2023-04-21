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

;;; Vanilla Emacs config
;; 16 point font
(set-face-attribute 'default nil :height 160)
;; tool-bar-mode show icons, menu-bar-mode show text menu dropdowns
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
;; Remember the files we've opened recently
(recentf-mode 1)
;; Save minibuffer history, making frequently used commands easier to access
(savehist-mode 1)

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

;;; Customs
;; Darkened highlight and region backgrounds by #101010
;; Removed region distant-foreground; it was an ugly bright color
;; Lightened comment face by #101010
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#8c7f74"))))
 '(highlight ((t (:background "#403935"))))
 '(region ((t (:extend t :background "#403935" :distant-foreground nil)))))
