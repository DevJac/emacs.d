;; -*- lexical-binding: t; -*-

;;; Theme collections

(use-package doom-themes
  :ensure t)
(use-package ef-themes
  :ensure t)
(use-package gruvbox-theme
  :ensure t)
(use-package modus-themes
  :ensure t)

;;; Modeline

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-enable-word-count t)
  (doom-modeline-mode 1))

;;; Color packages

(use-package rainbow-mode
  :ensure t)
(use-package ct
  :ensure t)

;;; My theme config

(load-theme 'gruvbox-dark-hard 'noconfirm)

;;; gruvbox-dark-hard customizations

(when (member 'gruvbox-dark-hard custom-enabled-themes)
  (set-face-attribute 'highlight nil
                      :background
                      (color-darken-name
                       (face-attribute 'highlight :background)
                       40))
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground
                      (color-lighten-name
                       (face-attribute 'font-lock-comment-face :foreground)
                       25))
  ;; Circular glyphs from Nerd Fonts are being used by doom-modeline
  ;; to show the evil mode (normal, insert, etc). There's a bug
  ;; somewhere that causes the right edge to be visually cut off. We
  ;; can work around the bug by not using bold fonts when rendering
  ;; these glyphs.
  (dolist (face '(doom-modeline-evil-motion-state
                  doom-modeline-evil-normal-state
                  doom-modeline-evil-insert-state
                  doom-modeline-evil-visual-state
                  doom-modeline-evil-operator-state
                  doom-modeline-evil-replace-state
                  doom-modeline-evil-emacs-state
                  doom-modeline-urgent))
    (set-face-attribute face nil :weight 'normal)))
