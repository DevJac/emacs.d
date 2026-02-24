;;; evil packages

(use-package goto-chg
  :ensure t)
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))
  :config
  (global-undo-tree-mode 1))
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  ;; evil-collection assumes evil-overriding-maps is nil.
  ;; evil-overriding-maps is nil by default, but we'll make sure.
  (setq evil-overriding-maps nil)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init '(dired)))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode 1))
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1))
(use-package evil-anzu
  :ensure t)
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-enable-word-count t)
  (doom-modeline-mode 1))

;;; evil commands

(defun kill-buffer-and-window-quiet ()
  "Kill current buffer, and kill window if there is more than one."
  (interactive)
  (kill-buffer (current-buffer))
  (unless (one-window-p)
    (evil-window-delete)))

(defun save-then-kill-buffer-and-window-quiet ()
  "Save and kill current buffer, and kill window if there is more than one."
  (interactive)
  (if (buffer-file-name)
      (save-buffer))
  (kill-buffer-and-window-quiet))

;; Don't allow evil commands to quit Emacs.
(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window-quiet)
(evil-ex-define-cmd "wq" #'save-then-kill-buffer-and-window-quiet)
(evil-define-key '(motion normal) 'global
  (kbd "ZQ") #'kill-buffer-and-window-quiet
  (kbd "ZZ") #'save-then-kill-buffer-and-window-quiet)

;; Create a text object between |'s. This is especially useful for org tables.
(evil-define-text-object evil-org-table-cell-inner (count &optional beg end type)
  "Select the contents of an org-mode table cell, excluding the | delimiters."
  (evil-select-paren "|" "|" beg end type count))

(define-key evil-inner-text-objects-map "|" #'evil-org-table-cell-inner)
