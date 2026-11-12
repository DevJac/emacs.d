;; -*- lexical-binding: t; -*-

(use-package whitespace
  :config
  ;; whitespace-style controls what is visualized.
  (setq whitespace-style
        '(face
          trailing
          tabs
          tab-mark
          spaces
          space-mark
          newline
          newline-mark
          missing-newline-at-eof))
  ;; Some whitespace faces have background colors,
  ;; this sometimes looks bad so we remove them.
  (dolist (face '(whitespace-tab
                  whitespace-space
                  whitespace-hspace
                  whitespace-newline))
    (set-face-attribute face nil
                        :background 'unspecified))

  ;; Make the whitespace-mode face for trailing whitespace
  ;; consistent with the trailing-whitespace face used elsewhere.
  (face-spec-reset-face 'whitespace-trailing)
  (set-face-attribute 'whitespace-trailing nil
                      :inherit 'trailing-whitespace))
