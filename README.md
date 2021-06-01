To install packages in Emacs
run (using M-x) `package-refresh-contents`
then run        `package-install-selected-packages`.

You may have to delete the `.elc` files for `org`, `org-drill`, or other packages.

## `gj` `gk` fix

Alter the function `evil-line-move` with the following:
```
              (if line-move-visual
                  (line-move-visual count)
                (funcall this-command (abs count))))
```
