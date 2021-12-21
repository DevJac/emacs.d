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

;;; haskell config
(defun haskell-offset-4 ()
  "Use 4 space offsets for Haskell."
  (interactive)
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-starter-offset 4))

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
