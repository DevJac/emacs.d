;; -*- lexical-binding: t; -*-

(use-package whitespace
  :config
  (face-spec-reset-face 'whitespace-trailing)
  (set-face-attribute 'whitespace-trailing nil
                      :inherit 'trailing-whitespace))
