To install packages in Emacs
run (using M-x) `package-refresh-contents`
then run        `package-install-selected-packages`.

You may have to delete the `.elc` files for `org`, `org-drill`, or other packages.

For `doom-modeline` run `all-the-icons-install-fonts`.

## `gj` `gk` fix

Alter the function `evil-line-move` with the following:
```
(defun evil-line-move (count &optional noerror)
  "A wrapper for line motions which conserves the column.
Signals an error at buffer boundaries unless NOERROR is non-nil."
  (cond
   (noerror
    (condition-case nil
        (evil-line-move count)
      (error nil)))
   (t
    (evil-signal-without-movement
      (let ((opoint (point)))
        (condition-case err
            (with-no-warnings
              (cond (line-move-visual (line-move-visual count))
                    ((>= count 0) (next-line count))
                    (t (previous-line (abs count)))))
          ((beginning-of-buffer end-of-buffer)
           (let ((col (or goal-column
                          (if (consp temporary-goal-column)
                              (car temporary-goal-column)
                            temporary-goal-column))))
             (if line-move-visual
                 (vertical-motion (cons col 0))
               (line-move-finish col opoint (< count 0)))
             ;; Maybe we should just `ding'?
             (signal (car err) (cdr err))))))))))
```
