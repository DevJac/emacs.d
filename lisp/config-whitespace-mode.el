(use-package whitespace
  :config
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

  (dolist (face '(whitespace-tab
                  whitespace-space
                  whitespace-hspace
                  whitespace-newline))
    (set-face-attribute face nil
                        :background 'unspecified))

  (face-spec-reset-face 'whitespace-trailing)
  (set-face-attribute 'whitespace-trailing nil
                      :inherit 'trailing-whitespace))
