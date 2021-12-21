(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-selected-packages
      '(ag                  ; ag search support
        amx                 ; history provider for counsel
        company
        counsel
        doom-modeline
        evil
        expand-region
        flycheck
        gruvbox-theme
        ivy
        ivy-hydra           ; needed for C-o in Ivy buffers, not auto-installed
        julia-mode
        keyfreq
        lsp-haskell
        lsp-mode
        lsp-ui
        minions
        neotree
        org-drill
        projectile
        racket-mode
        rainbow-delimiters
        restart-emacs
        rg                  ; rg search support
        rust-mode
        undo-tree           ; needed for evil redo until emacs version 28
        virtualenvwrapper   ; must set virtualenv before lsp works
        which-key
        whitespace
        yasnippet))         ; suggested requirement of lsp


;;; doom config
(setq doom-modeline-minor-modes t)
(setq doom-modeline-enable-word-count t)
(minions-mode 1)
(doom-modeline-mode 1)

;;; buffer & file config
(global-auto-revert-mode 1)
(global-undo-tree-mode 1)
(setq make-backup-files nil)

;;; evil config
(setq evil-overriding-maps nil)
(setq evil-intercept-maps nil)
(setq evil-emacs-state-modes nil)
(setq evil-search-module 'evil-search)
(setq evil-undo-system 'undo-tree)
(evil-mode 1)
(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window)
(evil-define-key 'normal 'global
  (kbd "Z Z") #'save-then-kill-buffer-and-window)
(evil-define-key 'motion 'global
  ;; (C-x C-f), (C-x C-b), and (C-x k), are basic Emacs keys and should be used.
  ;; This will ensure I remain somewhat comfortable in vanilla Emacs.
  (kbd "SPC") nil
  (kbd "Z Z") #'save-then-kill-buffer-and-window
  (kbd "SPC TAB") #'ivy-switch-buffer
  (kbd "SPC v") #'er/expand-region
  ;; projectile
  (kbd "SPC f") #'projectile-find-file
  (kbd "SPC p k") #'projectile-kill-buffers
  ;; windows
  (kbd "SPC w h") #'evil-window-left
  (kbd "SPC w j") #'evil-window-down
  (kbd "SPC w k") #'evil-window-up
  (kbd "SPC w l") #'evil-window-right
  (kbd "SPC w w") #'evil-window-next
  (kbd "SPC w r") #'evil-window-rotate-downwards
  (kbd "SPC w o") #'delete-other-windows
  (kbd "SPC w d") #'evil-window-delete
  (kbd "SPC w s") #'evil-window-split
  (kbd "SPC w v") #'evil-window-vsplit
  ;; parens
  (kbd "SPC p p") #'rainbow-delimiters-mode
  (kbd "SPC p h") #'show-paren-mode
  ;; whitespace
  (kbd "SPC t l") #'visual-line-mode
  (kbd "SPC t t") #'trim-trailing-whitespace
  (kbd "SPC t w") #'whitespace-mode
  ;; spelling
  (kbd "SPC t s") #'flyspell-mode
  (kbd "SPC s s") #'flyspell-buffer
  ;; org
  (kbd "SPC o i") #'org-indent-mode
  (kbd "SPC o h") #'org-insert-heading-promote
  (kbd "SPC o j") #'org-insert-heading-down
  (kbd "SPC o k") #'org-insert-heading-up
  (kbd "SPC o l") #'org-insert-heading-demote
  (kbd "SPC o c c") #'org-clock-in
  (kbd "SPC o c o") #'org-clock-out
  (kbd "SPC o c g") #'org-clock-goto
  (kbd "SPC o c l") #'org-clock-last
  (kbd "SPC o c d") #'org-clock-display
  (kbd "SPC o c r") #'org-clock-report
  ;; neotree
  (kbd "SPC q q") #'neotree-toggle
  (kbd "SPC q d") #'neotree-dir)
(evil-define-key 'normal neotree-mode-map  ; We have to use 'normal map here?
  (kbd "a") #'neotree-hidden-file-toggle
  (kbd "d") #'neotree-change-root
  (kbd "TAB") #'neotree-quick-look
  (kbd "RET") #'neotree-enter)
(evil-define-key 'motion Info-mode-map
  (kbd "SPC") nil   ; Why is this needed here, but not in other modes?
  (kbd "SPC SPC") (lookup-key Info-mode-map (kbd "SPC")))
(evil-define-key 'motion markdown-mode-map
  (kbd "TAB") #'markdown-cycle)
(evil-define-key 'motion messages-buffer-mode-map
  (kbd "SPC SPC") (lookup-key messages-buffer-mode-map (kbd "SPC")))
;; The *Messages* buffer is created very early during Emacs startup,
;; so we have to help it apply these Evil keybindings, as follows:
;; See: https://github.com/noctuid/evil-guide/issues/11
(with-current-buffer "*Messages*" (evil-normalize-keymaps))

;;; functions
(defun save-then-kill-buffer-and-window ()
  (interactive)
  (when (buffer-modified-p) (save_buffer))
  (kill-buffer-and-window))
(defun trim-trailing-whitespace ()
  (interactive)
  (delete-trailing-whitespace)
  (message "Trimmed trailing whitespace"))
(defun org-insert-heading-promote ()
  (interactive)
  (org-insert-heading-after-current)
  (org-promote)
  (evil-insert 1))
(defun org-insert-heading-down ()
  (interactive)
  (if (org-at-item-p)
      (progn
	(evil-append-line 1)
	(org-insert-item (org-at-item-checkbox-p)))
    (progn
      (org-insert-heading-after-current)
      (evil-insert 1))))
(defun org-insert-heading-up ()
  (interactive)
  (if (org-at-item-p)
      (progn
	(evil-beginning-of-line)
	(org-insert-item (org-at-item-checkbox-p)))
    (progn
      (org-back-to-heading)
      (org-insert-heading)
      (end-of-line 1)
      (evil-insert 1))))
(defun org-insert-heading-demote ()
  (interactive)
  (org-insert-heading-after-current)
  (org-demote)
  (evil-insert 1))

;;; haskell config
(defun haskell-offset-4 ()
  "Use 4 space offsets for Haskell."
  (interactive)
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-starter-offset 4))

;;; ivy & counsel config
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") #'swiper)
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
(setq ivy-initial-inputs-alist
      '((counsel-M-x . "")
        (counsel-describe-function . "")
        (counsel-describe-variable . "")))

;;; keyfreq config
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file "~/.emacs.d/keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/keyfreq-lock")

;;; lsp config
(setq lsp-prefer-flymake nil)
(setq lsp-rust-server 'rust-analyzer)

;;; org config
(setq org-ellipsis "⤵")
(setq org-fontify-done-headline nil)
(setq org-fontify-todo-headline nil)
;; org-mode and whitespace-mode both modify Emacs' "display tables".
;; When leaving whitespace-mode, my custom org-ellipsis were being replaced with
;; the standard "...". To fix this, I reapply the display table modifications
;; made by org-mode when leaving whitespace-mode.
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Tables.html
(add-hook 'whitespace-mode-hook
          (lambda ()
            (when (and (eq major-mode 'org-mode) (eq whitespace-mode nil))
              ;; The remainder of this function was copied from the org-mode function.
              (unless org-display-table
                (setq org-display-table (make-display-table)))
              (set-display-table-slot
               org-display-table 4
               (vconcat (mapcar (lambda (c) (make-glyph-code c 'org-ellipsis))
                                org-ellipsis)))
              (setq buffer-display-table org-display-table))))
;; When we scale text, we want our rendered latex fragments to scale as well.
(defun scale-latex-fragments ()
  (interactive)
  (org-toggle-latex-fragment '(16))
  (let ((scale
         (if (boundp 'text-scale-mode-step)
             (* 2.0 (expt text-scale-mode-step text-scale-mode-amount))
           2.0)))
    (plist-put org-format-latex-options :scale scale))
  (org-toggle-latex-fragment '(16)))
(add-hook 'org-mode-hook 'scale-latex-fragments)
(add-hook 'text-scale-mode-hook
          (lambda () (when (eq major-mode 'org-mode) (scale-latex-fragments))))

;;; org-drill config
(setq org-drill-hide-item-headings-p t)
(setq org-drill-add-random-noise-to-intervals-p t)

;;; projectile config
(setq projectile-completion-system 'ivy)

;;; which-key config
(which-key-mode)

;;; Custom
(custom-set-faces
 '(org-block-begin-line ((t (:background nil :foreground "#7c6f64"))))
 '(org-block-end-line ((t (:background nil :foreground "#7c6f64"))))
 '(org-drawer ((t (:background nil :foreground "#7c6f64")))))
(custom-set-variables
 '(dante-load-flags
   (quote
    ("+c" "-ferror-spans" "-fdefer-typed-holes" "-Wwarn=missing-home-modules" "-fno-diagnostics-show-caret")))
 '(org-modules (quote (org-drill org-tempo)))
 '(indent-tabs-mode nil)
 '(sgml-basic-offset 4))
