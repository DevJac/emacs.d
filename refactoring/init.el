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



;;; lsp config
(setq lsp-prefer-flymake nil)
(setq lsp-rust-server 'rust-analyzer)
