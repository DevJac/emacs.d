(defvar pur-opt-in-modes '(prog-mode text-mode)
  "List of parent modes where `pur-mode' should activate.")

(define-globalized-minor-mode global-pur-mode pur-mode
  (lambda ()
    (when (apply #'derived-mode-p pur-opt-in-modes)
      (setq show-trailing-whitespace t)
      (pur-enabled-mode 1)
      (pur-mode 1))))

(define-minor-mode pur-enabled-mode
  "Minor mode to enable `pur-mode' through escape keybindings."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<escape>") #'pur-escape)
            map))

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
  "Escape from `pur-mode' or quit from minibuffer."
  (interactive)
  (if (minibufferp)
      (keyboard-escape-quit)
    (pur-enter)))

(defun pur-change ()
  "Change the selected region or exit `pur-mode'."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (call-interactively #'delete-char))
  (pur-exit))

(defun pur-insert-below ()
  "Insert a new line below and exit `pur-mode'."
  (interactive)
  (call-interactively #'move-end-of-line)
  (call-interactively #'newline)
  (pur-exit))

(defun pur-insert-above ()
  "Insert a new line above and exit `pur-mode'."
  (interactive)
  (call-interactively #'previous-line)
  (pur-insert-below)
  (pur-exit))

(defun pur-g ()
  "Deactivate the mark."
  (interactive)
  (deactivate-mark))

(defun pur-org-table-change-field ()
  "Blank the org table field then begin inserting text."
  (interactive)
  (org-table-blank-field)
  (pur-exit))

(defun pur-select-entire-lines ()
  "Modify the region to select entire lines."
  (interactive)
  (unless (region-active-p)
    (push-mark (point)))
  (if (< (point) (mark))
      (progn
        (call-interactively #'beginning-of-line)
        (exchange-point-and-mark)
        (call-interactively #'next-line)
        (call-interactively #'beginning-of-line)
        (exchange-point-and-mark))
    (progn
      (call-interactively #'next-line)
      (call-interactively #'beginning-of-line)
      (exchange-point-and-mark)
      (call-interactively #'beginning-of-line)
      (exchange-point-and-mark))))

(defun pur--fill-typeable-keys (map)
  "Bind typeable characters in MAP to `ignore'."
  (dotimes (i (- 127 32))
    (define-key map (char-to-string (+ 32 i)) #'ignore))
  (define-key map (kbd "RET") #'ignore))

(pur--fill-typeable-keys pur-mode-map)

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
(define-key pur-mode-map (kbd "w") #'expreg-expand)
(define-key pur-mode-map (kbd "s") #'expreg-contract)
(define-key pur-mode-map (kbd "v") #'set-mark-command)
(define-key pur-mode-map (kbd "V") #'pur-select-entire-lines)
(define-key pur-mode-map (kbd "u") #'undo)
(define-key pur-mode-map (kbd "o") #'pur-insert-below)
(define-key pur-mode-map (kbd "O") #'pur-insert-above)
(define-key pur-mode-map (kbd "f") #'forward-word)
(define-key pur-mode-map (kbd "b") #'backward-word)
(define-key pur-mode-map (kbd "g") #'pur-g)
(define-key pur-mode-map (kbd "x") #'execute-extended-command)
(define-key pur-mode-map (kbd ":") #'eval-expression)
(define-key pur-mode-map (kbd "[") #'backward-paragraph)
(define-key pur-mode-map (kbd "]") #'forward-paragraph)
(define-key pur-mode-map (kbd "SPC") mode-specific-map)
(define-key pur-mode-map (kbd "SPC o t c") #'pur-org-table-change-field)
