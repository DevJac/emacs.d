;;; Elpaca bootstrap
;; See: https://github.com/progfolio/elpaca
(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Vanilla Emacs config
;; 16 point font
(set-face-attribute 'default nil :height 160)
;; tool-bar-mode show icons, menu-bar-mode show text menu dropdowns
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
;; Remember the files we've opened recently
(setq recentf-max-saved-items 200)
(recentf-mode 1)
;; Save minibuffer history, making frequently used commands easier to access
(savehist-mode 1)
;; Scrolling begins this many lines from edge
(setq scroll-margin 2)
;; Never center the point because of scrolling
(setq scroll-conservatively 101)
;; Number of lines of continuity when scrolling by screenfuls
(setq next-screen-context-lines 4)
;; Display column number on modeline
(column-number-mode 1)
;; Auto save to visited file instead of a backup
(auto-save-visited-mode 1)
;; Auto revert file when it changes from some outside source
(global-auto-revert-mode 1)
;; No backup files
(setq make-backup-files nil)
;; Visual line mode, which turns on word-wrap by default
(global-visual-line-mode 1)

;;; Elpaca integration with use-package
;; See: https://github.com/progfolio/elpaca
(elpaca elpaca-use-package
  (elpaca-use-package-mode 1)
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

(add-to-list
 'load-path
 (expand-file-name "elisp" user-emacs-directory))
(load "defuns")

;;; use-package's
;; :init runs before package is loaded
;; :config runs after package is loaded
;; Keep :init light.
;; Put as much in :config as possible; this will help with deferred loading.
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))
;; Emacs 29 will have a restart function; maybe remove later?
(use-package restart-emacs)
(use-package magit)
(use-package expand-region)
(use-package which-key
  :config
  (which-key-mode 1))
(use-package evil
  :init
  ;; When searching, keep search highlights visible
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  :config
  (load "evil-keys")
  (evil-mode 1))
;; evil-anzu is used to show search match count in modeline
(use-package evil-anzu
  :config
  (global-anzu-mode 1))
(use-package undo-tree
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-histories")))
  :config
  (global-undo-tree-mode 1))
(use-package doom-modeline
  :init
  ;; Select info shows word count
  (setq doom-modeline-enable-word-count t)
  :config
  (doom-modeline-mode 1))
;; consult provides specific commands, like consult-buffer
(use-package consult
  :config
  (consult-customize
   consult-buffer
   :preview-key
   '(:debounce 0.5 any)))
;; vertico provides the selection UI
(use-package vertico
  :config
  (vertico-mode 1))
;; orderless provides the fuzzy string matching
(use-package orderless
  :init
  ;; See C-h v completion-styles
  ;; "basic" is needed for Tramp host name completion
  (setq completion-styles '(orderless basic)))
;; marginalia provides added info in right margins
(use-package marginalia
  :config
  (marginalia-mode 1))
;; corfu is for completion-at-point
(use-package corfu
  :config
  (global-corfu-mode))
(use-package org
  :init
  (setq org-clock-idle-time 15)
  (setq org-clock-mode-line-total 'current)
  (setq org-duration-format 'h:mm)
  (setq org-ellipsis " ▼")
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-log-done 'time)
  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (add-hook 'whitespace-mode-hook #'fix-org-ellipsis-after-whitespace-mode)
  (add-hook 'org-mode-hook #'scale-latex-fragments)
  (add-hook 'text-scale-mode-hook
            (lambda ()
              (when (eq major-mode 'org-mode)
                (scale-latex-fragments)))))
(use-package org-drill
  :init
  (setq org-drill-hide-item-headings-p t)
  (setq org-drill-add-random-noise-to-intervals-p t)
  :config
  (load "overrides"))
(use-package org-roam
  :init
  (setq org-roam-directory "~/OrgRoam")
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}-%<%Y%m%d>.org" "#+title: ${title}")
           :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode 1))
(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))
(use-package poke-line
  :init
  (make-variable-buffer-local 'poke-line-pokemon)
  (add-hook 'poke-line-mode-hook #'poke-line-set-random-pokemon)
  :config
  (poke-line-global-mode 1))

;;; Mac / Homebrew config
; (add-to-list 'exec-path "/opt/homebrew/bin")

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
 '(org-block-begin-line ((t (:foreground "#8c7f74"))))
 '(org-block-end-line ((t (:foreground "#8c7f74"))))
 '(org-drawer ((t (:inherit org-special-keyword))))
 '(region ((t (:background "#403935"))))
 '(shadow ((t (:foreground "#8c7f74"))))
 '(show-paren-match ((t (:foreground nil :background "#403935" :weight bold)))))
