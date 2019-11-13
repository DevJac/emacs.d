; The following line is a workaround for Emacs 26.2.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(setq package-selected-packages
      '(amx                 ; history provider for counsel
	company-lsp
	counsel
	evil
	evil-collection     ; evil bindings for more modes
	flycheck
	gruvbox-theme
	keyfreq
	lsp-mode
	lsp-ui
	projectile
	restart-emacs
	rust-mode
	whitespace
	yasnippet))         ; suggested requirement of lsp

; evil config
(setq evil-want-keybinding nil)
(evil-mode 1)
(evil-collection-init)

; ivy & counsel config
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers 1)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") #'swiper)
(setq spc-map (make-sparse-keymap))
(define-key evil-normal-state-map (kbd "SPC") spc-map)
(define-key evil-motion-state-map (kbd "SPC") spc-map)
(define-key spc-map (kbd "TAB") #'ivy-switch-buffer)

; keyfreq config
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file "~/.emacs.d/keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/keyfreq-lock")

(setq scroll-margin 2)
(setq scroll-conservatively 100)

(load-theme 'gruvbox-dark-hard 1)
(set-face-attribute 'default nil :height 140)

(custom-set-variables
 '(ivy-initial-inputs-alist
   (quote
    ((counsel-minor . "^+")
     (counsel-package . "^+")
     (counsel-org-capture . "^")
     (counsel-M-x . "")
     (counsel-describe-function . "")
     (counsel-describe-variable . "")
     (org-refile . "^")
     (org-agenda-refile . "^")
     (org-capture-refile . "^")
     (Man-completion-table . "^")
     (woman . "^")))))
(custom-set-faces
 )
