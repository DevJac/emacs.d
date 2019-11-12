; The following line is a workaround for Emacs 26.2.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(setq package-selected-packages
      '(amx
	company-lsp
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
	whitespace
	yasnippet))

(evil-mode 1)
(ivy-mode 1)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file "~/.emacs.d/keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/keyfreq-lock")

(setq scroll-margin 2)
(setq scroll-conservatively 10)

(load-theme 'gruvbox-dark-hard 1)
(set-face-attribute 'default nil :height 160)
