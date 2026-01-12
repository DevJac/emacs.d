(use-package expreg
  :ensure t)

(defun my/expreg--org-table-field ()
  "Identify org table fields for expreg."
  (when (org-at-table-p)
    (save-excursion
      (let ((beg (progn (org-table-beginning-of-field 0) (point)))
            (end (progn (org-table-end-of-field 0) (point))))
        (list `(org-table-field . ,(cons beg end)))))))

(defun my/expreg--org-block ()
  "Identify org blocks for expreg."
  (save-match-data
    (let* ((orig-point (point))
           (case-fold-search t)
           (lim-up (save-excursion (outline-previous-heading)))
           (lim-down (save-excursion (outline-next-heading)))
           (block-beg (save-excursion
                        (when (search-backward "#+begin_" lim-up 'NOERROR)
                          (forward-line 1)
                          (point))))
           (block-end (save-excursion
                        (when (search-forward "#+end_" lim-down 'NOERROR)
                          (beginning-of-line)
                          (point)))))
      (when (and
             block-beg
             block-end
             (<= block-beg orig-point)
             (<= orig-point block-end))
        (list `(org-block . ,(cons block-beg block-end)))))))

(add-to-list 'expreg-functions #'my/expreg--org-table-field)
(add-to-list 'expreg-functions #'my/expreg--org-block)
(setq-default expreg-functions expreg-functions)

;;; transient maps

(defvar my/expreg--repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'expreg-expand)
    (define-key map (kbd "V") #'expreg-contract)
    map)
  "Repeat map for expreg commands.")

(put 'expreg-expand 'repeat-map my/expreg--repeat-map)
(put 'expreg-contract 'repeat-map my/expreg--repeat-map)
