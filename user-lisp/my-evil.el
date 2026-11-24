;; -*- lexical-binding: t; -*-

(declare-function evil-previous-visual-line "evil-commands")
(declare-function evil-next-visual-line "evil-commands")
(declare-function evil-window-delete "evil-commands")
(declare-function evil-ex-search-activate-highlight "evil-search")
(declare-function evil-push-search-history "evil-search")
(declare-function evil-ex-make-search-pattern "evil-search")
(declare-function evil-find-thing "evil-search")

;;;###autoload
(defun kill-buffer-and-window-quiet ()
  "Kill current buffer, and kill window if there is more than one."
  (interactive)
  (when (kill-buffer (current-buffer))
    (unless (one-window-p)
      (evil-window-delete))))

;;;###autoload
(defun save-then-kill-buffer-and-window-quiet ()
  "Save and kill current buffer, and kill window if there is more than one."
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (kill-buffer-and-window-quiet))

;; These help when navigating display overlays.
(defun my/evil-in-display-overlay-p ()
  "Return non-nil if point is inside a text-replacing overlay."
  (cl-some
   (lambda (ov)
     (stringp (overlay-get ov 'display)))
   (overlays-at (point))))

(defun my/evil-next-line-advice (original &optional count)
  "Use visual-line movement inside display overlays."
  (if (my/evil-in-display-overlay-p)
      (evil-next-visual-line count)
    (funcall original count)))

(defun my/evil-previous-line-advice (original &optional count)
  "Use visual-line movement inside display overlays."
  (if (my/evil-in-display-overlay-p)
      (evil-previous-visual-line count)
    (funcall original count)))

;;; # and * customizations, highlight words and symbols

(defun my/evil-highlight-word (&optional symbol)
  "Highlight the word at point without moving.
If SYMBOL is non-nil, highlight the entire symbol."
  (interactive)
  (let* ((text (or (evil-find-thing t (if symbol 'symbol 'word))
                   (user-error "No word at point")))
         (regex (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                        (regexp-quote text))))
    (setq evil-ex-search-direction 'forward
          evil-ex-search-offset nil
          evil-ex-last-was-search t
          evil-ex-search-pattern
          (let (evil-ex-search-vim-style-regexp)
            (evil-ex-make-search-pattern regex)))
    (unless (equal regex (car evil-ex-search-history))
      (push regex evil-ex-search-history))
    (evil-push-search-history regex t)
    (evil-ex-search-activate-highlight evil-ex-search-pattern)))

(defun my/evil-highlight-symbol ()
  "Highlight the symbol at point without moving."
  (interactive)
  (my/evil-highlight-word t))

(provide 'my-evil)
