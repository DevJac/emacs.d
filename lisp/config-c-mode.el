;; cc-mode had its own TAB behavior, but we don't want it.
;; We want TAB to behave the same as everywhere else.
(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "TAB") nil 'remove)))
