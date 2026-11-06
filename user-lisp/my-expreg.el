;; -*- lexical-binding: t; -*-

(declare-function expreg-contract "expreg")
(declare-function expreg-expand "expreg")
(declare-function outline-next-heading "outline")
(declare-function outline-previous-heading "outline")
(declare-function org-table-end-of-field "org-table")
(declare-function org-table-beginning-of-field "org-table")
(declare-function org-at-table-p "org-table")

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

;;; transient maps

(defvar my/expreg--repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'expreg-expand)
    (define-key map (kbd "V") #'expreg-contract)
    map)
  "Repeat map for expreg commands.")

(provide 'my-expreg)
