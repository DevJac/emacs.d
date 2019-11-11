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
	lsp-mode
	lsp-ui
	projectile
	restart-emacs
	rust-mode
	whitespace
	yasnippet))

(load-theme 'gruvbox-dark-hard 1)

(evil-mode 1)
(ivy-mode 1)
