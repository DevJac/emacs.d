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

;;; keyfreq config
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file "~/.emacs.d/keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/keyfreq-lock")

;;; lsp config
(setq lsp-prefer-flymake nil)
(setq lsp-rust-server 'rust-analyzer)

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
