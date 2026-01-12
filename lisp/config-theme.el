(use-package doom-themes
  :ensure t)
(use-package gruvbox-theme
  :ensure t)

(load-theme 'gruvbox-dark-hard 'noconfirm)

(when (member 'gruvbox-dark-hard custom-enabled-themes)
  (set-face-attribute 'highlight nil
                      :background "#403933"))
