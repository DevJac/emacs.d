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
	rainbow-delimiters
	restart-emacs
	rust-mode
	whitespace
	yasnippet))         ; suggested requirement of lsp

; theme config
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'gruvbox-dark-hard 1)
(set-face-attribute 'default nil :height 160)

; evil config
(setq evil-search-module 'evil-search)
(setq evil-want-keybinding nil)
(evil-mode 1)
(evil-collection-init)
(define-key evil-motion-state-map (kbd "SPC") (make-sparse-keymap))
(define-key evil-motion-state-map (kbd "SPC TAB") #'ivy-switch-buffer)

; ivy & counsel config
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers 1)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") #'swiper)
(setq ivy-initial-inputs-alist
      '((counsel-minor . "^+")
	(counsel-package . "^+")
	(counsel-org-capture . "^")
	(counsel-M-x . "")
	(counsel-describe-function . "")
	(counsel-describe-variable . "")
	(org-refile . "^")
	(org-agenda-refile . "^")
	(org-capture-refile . "^")
	(Man-completion-table . "^")
	(woman . "^")))

; keyfreq config
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file "~/.emacs.d/keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/keyfreq-lock")

; scroll config
(setq scroll-margin 2)
(setq scroll-conservatively 100)
(setq next-screen-context-lines 4)
