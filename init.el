; The following line is a workaround for Emacs 26.2.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/undo-tree")
(require 'undo-tree)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(setq package-selected-packages
      '(company-lsp
	counsel
	flycheck
	gruvbox-theme
	lsp-mode
	lsp-ui
	projectile
	restart-emacs))

(load-theme 'gruvbox-dark-hard 1)

(ivy-mode 1)
