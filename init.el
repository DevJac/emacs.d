(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-selected-packages
      '(ag                  ; ag search support
        amx                 ; history provider for counsel
        cl                  ; needed for org-drill
        company-lsp
        counsel
        dante               ; a backup for Haskell, in case lsp doesn't work
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
        org-drill
        projectile
        racket-mode
        rainbow-delimiters
        restart-emacs
        rg                  ; rg search support
        rich-minority       ; hides blacklisted minor modes
        rust-mode
        smart-mode-line
        virtualenvwrapper   ; must set virtualenv before lsp works
        which-key
        whitespace
        yasnippet))         ; suggested requirement of lsp

;;; theme config
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(set-face-attribute 'default nil :height 160)
(setq indent-tabs-mode nil)
(setq scroll-margin 2)
(setq scroll-conservatively 100)
(setq next-screen-context-lines 4)
(load-theme 'gruvbox-dark-hard t)
(sml/setup)
(setq rm-blacklist
      '(" counsel"
        " ivy"
        " Undo-Tree"
        " WK"))

;;; buffer & file config
(global-auto-revert-mode 1)
(setq make-backup-files nil)

;;; evil config
(setq evil-overriding-maps nil)
(setq evil-intercept-maps nil)
(setq evil-emacs-state-modes nil)
(setq evil-search-module 'evil-search)
(evil-mode 1)
(evil-ex-define-cmd "q[uit]" #'kill-buffer-and-window)
(evil-define-key 'motion 'global
  ;; (C-x C-f), (C-x C-b), and (C-x k), are basic Emacs and should used.
  ;; This will ensure I remain somewhat comfortable in vanilla Emacs.
  (kbd "SPC") nil
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
  (kbd "SPC t t") (lambda () (interactive) (message "Deleted trailing whitespace") (delete-trailing-whitespace))
  (kbd "SPC t w") #'whitespace-mode)
(evil-define-key 'motion Info-mode-map
  (kbd "SPC") nil   ; Why is this needed here, but not in other modes?
  (kbd "SPC SPC") (lookup-key Info-mode-map (kbd "SPC")))
(evil-define-key 'motion messages-buffer-mode-map
  (kbd "SPC SPC") (lookup-key messages-buffer-mode-map (kbd "SPC")))
;; The *Messages* buffer is created very early during Emacs startup,
;; so we have to help it apply these Evil keybindings, as follows:
;; See: https://github.com/noctuid/evil-guide/issues/11
(with-current-buffer "*Messages*" (evil-normalize-keymaps))
(evil-define-key 'motion markdown-mode-map
  (kbd "TAB") #'markdown-cycle)

;;; haskell config
(defun haskell-offset-4 ()
  "Use 4 space offsets for Haskell."
  (interactive)
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-starter-offset 4))
(setq dante-mode-hook (lambda () (company-mode 1) (flycheck-mode 1)))
(setq dante-flycheck-types '(("^warning" . warning) ("^splicing ") ("" . error)))

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
(require 'lsp-haskell)
(setq lsp-haskell-process-path-hie "ghcide")
(setq lsp-haskell-process-args-hie '())

;;; org config
(setq org-ellipsis "⤵")
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
;; org-drill needs (require 'cl) for some reason,
;; without it we get an error: Symbol's function definition is void: first
(add-hook 'org-mode-hook (lambda () (require 'cl)))
(setq org-drill-hide-item-headings-p t)
(setq org-drill-add-random-noise-to-intervals-p t)

;;; projectile config
(setq projectile-completion-system 'ivy)

;;; which-key config
(which-key-mode)

;;; Custom
(custom-set-faces
 '(org-block-begin-line ((t (:background nil :foreground "#7c6f64"))))
 '(org-block-end-line ((t (:background nil :foreground "#7c6f64")))))
(custom-set-variables
 '(dante-load-flags
   (quote
    ("+c" "-ferror-spans" "-fdefer-typed-holes" "-Wwarn=missing-home-modules" "-fno-diagnostics-show-caret")))
 '(org-modules (quote (org-drill org-tempo))))
