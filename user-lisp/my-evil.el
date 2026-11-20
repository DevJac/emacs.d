;; -*- lexical-binding: t; -*-

(declare-function evil-previous-visual-line "evil-commands")
(declare-function evil-next-visual-line "evil-commands")
(declare-function evil-window-delete "evil-commands")

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

(provide 'my-evil)
