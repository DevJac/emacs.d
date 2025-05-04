(defvar pur-mode-map (make-keymap)
  "Keymap for `pur-mode'.")

(define-minor-mode pur-mode
  "A lightweight modal editing mode."
  :lighter " PUR"
  :keymap pur-mode-map
  (if pur-mode
      (setq cursor-type 'box)
    (setq cursor-type 'bar)))

(defun pur-enter ()
  "Enter `pur-mode'."
  (interactive)
  (pur-mode 1))

(defun pur-exit ()
  "Exit `pur-mode'."
  (interactive)
  (pur-mode -1))

(defun pur-escape ()
  (interactive)
  (if (minibufferp)
      (keyboard-escape-quit)
    (pur-enter)))

(defun pur-change ()
  (interactive)
  (when (region-active-p)
    (call-interactively #'kill-region))
  (pur-exit))

(defun pur-insert-above ()
  (interactive)
  (call-interactively #'move-beginning-of-line)
  (call-interactively #'newline)
  (call-interactively #'previous-line)
  (pur-exit))

(defun pur-insert-below ()
  (interactive)
  (call-interactively #'move-end-of-line)
  (call-interactively #'newline)
  (pur-exit))

(defun pur-g ()
  (interactive)
  (deactivate-mark))

(defun pur--fill-typeable-keys (map)
  "Bind typeable characters in MAP to `ignore'."
  (dotimes (i (- 127 32))
    (let ((char (+ 32 i)))
      (define-key map (kbd (char-to-string (+ 32 i))) #'ignore)))
  (define-key map (kbd "RET") #'ignore))

(pur--fill-typeable-keys pur-mode-map)

(define-key global-map (kbd "<escape>") #'pur-escape)
(define-key pur-mode-map (kbd "i") #'pur-exit)
(define-key pur-mode-map (kbd "h") #'backward-char)
(define-key pur-mode-map (kbd "j") #'next-line)
(define-key pur-mode-map (kbd "k") #'previous-line)
(define-key pur-mode-map (kbd "l") #'forward-char)
(define-key pur-mode-map (kbd "n") #'next-line)
(define-key pur-mode-map (kbd "p") #'previous-line)
(define-key pur-mode-map (kbd "a") #'move-beginning-of-line)
(define-key pur-mode-map (kbd "e") #'move-end-of-line)
(define-key pur-mode-map (kbd "c") #'pur-change)
(define-key pur-mode-map (kbd "s") #'expreg-expand)
(define-key pur-mode-map (kbd "v") #'set-mark-command)
(define-key pur-mode-map (kbd "u") #'undo)
(define-key pur-mode-map (kbd "o") #'pur-insert-below)
(define-key pur-mode-map (kbd "O") #'pur-insert-above)
(define-key pur-mode-map (kbd "f") #'forward-word)
(define-key pur-mode-map (kbd "b") #'backward-word)
(define-key pur-mode-map (kbd "g") #'pur-g)
