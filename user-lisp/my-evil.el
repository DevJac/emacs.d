;; -*- lexical-binding: t; -*-

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
