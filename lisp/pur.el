(defvar pur-mode-map (make-keymap)
  "Keymap for `pur-mode'.")

(define-minor-mode pur-mode
  "A lightweight modal editing mode."
  :lighter " PUR"
  :keymap pur-mode-map)

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
(define-key pur-mode-map (kbd "l") #'forward-char)
(define-key pur-mode-map (kbd "j") #'forward-word)
(define-key pur-mode-map (kbd "k") #'backward-word)
(define-key pur-mode-map (kbd "n") #'next-line)
(define-key pur-mode-map (kbd "p") #'previous-line)
(define-key pur-mode-map (kbd "a") #'move-beginning-of-line)
(define-key pur-mode-map (kbd "e") #'move-end-of-line)
(define-key pur-mode-map (kbd "c") #'pur-change)
(define-key pur-mode-map (kbd "d") #'delete-char)
(define-key pur-mode-map (kbd "s") #'expreg-expand)
(define-key pur-mode-map (kbd "v") #'set-mark-command)
(define-key pur-mode-map (kbd "u") #'undo)
