;; -*- lexical-binding: t; -*-

;;;###autoload
(defun my/line-wrap-reset ()
  "Reset wrapping modes."
  (toggle-truncate-lines -1)
  (toggle-word-wrap -1)
  (visual-line-mode -1)
  (visual-fill-column-mode -1)
  (visual-wrap-prefix-mode -1))

;;;###autoload
(defun my/line-wrap-truncate ()
  "Truncate long lines."
  (interactive)
  (my/line-wrap-reset)
  (toggle-truncate-lines 1))

;;;###autoload
(defun my/line-wrap-character ()
  "Wrap long lines at character boundaries."
  (interactive)
  (my/line-wrap-reset))

;;;###autoload
(defun my/line-wrap-word ()
  "Wrap at word boundaries, indenting continuations."
  (interactive)
  (my/line-wrap-reset)
  (toggle-word-wrap 1)
  (setq-local visual-wrap-extra-indent 8)
  (visual-wrap-prefix-mode 1))

;;;###autoload
(defun my/line-wrap-narrow ()
  "Word-wrap at fill-column, left-aligned."
  (interactive)
  (my/line-wrap-reset)
  (setq-local visual-fill-column-width nil
              visual-fill-column-center-text nil)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

;;;###autoload
(defun my/line-wrap-center ()
  "Word-wrap at fill-column, centered."
  (interactive)
  (my/line-wrap-reset)
  (setq-local visual-fill-column-width nil
              visual-fill-column-center-text t)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))
