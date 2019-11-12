; The following line is a workaround for Emacs 26.2.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(setq package-selected-packages
      '(company-lsp
	counsel
	evil
	flycheck
	gruvbox-theme
	keyfreq
	lsp-mode
	lsp-ui
	projectile
	restart-emacs
	rust-mode
	smex
	whitespace
	yasnippet))

(evil-mode 1)
(ivy-mode 1)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(load-theme 'gruvbox-dark-hard 1)
(set-face-attribute 'default nil :height 160)
