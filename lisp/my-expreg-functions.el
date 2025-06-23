;;; Add org table fields to expreg regions.

(defun my/expreg--org-table-field ()
  "Identify org table fields for expreg."
  (when (org-at-table-p)
    (save-excursion
      (let ((beg (progn (org-table-beginning-of-field 0) (point)))
            (end (progn (org-table-end-of-field 0) (point))))
        (list `(org-table-field . ,(cons beg end)))))))

(defun my/expreg--org-src-block ()
  "Identify src blocks for expreg."
  (when (and (derived-mode-p 'org-mode)
             (org-in-src-block-p))
    (save-mark-and-excursion
      (org-babel-mark-block)
      (let ((beg (region-beginning))
            (end (region-end)))
        (list `(org-babel-src-block . ,(cons beg end)))))))

(add-to-list 'expreg-functions #'my/expreg--org-table-field)
(add-to-list 'expreg-functions #'my/expreg--org-src-block)
(setq-default expreg-functions expreg-functions)
