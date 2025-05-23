(defvar purr-opt-in-modes '(prog-mode text-mode)
  "List of parent modes where `purr-mode' should activate.")

(define-globalized-minor-mode global-purr-mode purr-mode
  (lambda ()
    (when (apply #'derived-mode-p purr-opt-in-modes)
      (setq show-trailing-whitespace t)
      (purr-enabled-mode 1)
      (purr-mode 1))))

(define-minor-mode purr-enabled-mode
  "Minor mode to enable `purr-mode' through escape keybindings."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<escape>") #'purr-escape)
            map))

(defvar purr-mode-map (make-keymap)
  "Keymap for `purr-mode'.")

(define-minor-mode purr-mode
  "A lightweight modal editing mode."
  :lighter " PURR"
  :keymap purr-mode-map
  (if purr-mode
      (setq cursor-type 'box)
    (setq cursor-type 'bar)))

(defun purr-enter ()
  "Enter `purr-mode'."
  (interactive)
  (purr-mode 1))

(defun purr-exit ()
  "Exit `purr-mode'."
  (interactive)
  (purr-mode -1))

(defun purr-escape ()
  "Escape from `purr-mode' or quit from minibuffer."
  (interactive)
  (if (minibufferp)
      (keyboard-escape-quit)
    (purr-enter)))

(defun purr-change ()
  "Change the selected region or exit `purr-mode'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-region)
    (call-interactively #'delete-char))
  (purr-exit))

(defun purr-insert-below ()
  "Insert a new line below and exit `purr-mode'."
  (interactive)
  (call-interactively #'move-end-of-line)
  (call-interactively #'newline)
  (purr-exit))

(defun purr-insert-above ()
  "Insert a new line above and exit `purr-mode'."
  (interactive)
  (call-interactively #'beginning-of-line)
  (call-interactively #'newline)
  (call-interactively #'previous-line)
  (purr-exit))

(defun purr-kill-region-if-active ()
  "Kill region, but only if it's active."
  (interactive)
  (when (use-region-p)
    (call-interactively #'kill-region)))

(defun purr-select-entire-lines ()
  "Modify the region to select entire lines."
  (interactive)
  (let ((sel (lambda ()
               ;; We're assuming point < mark
               (call-interactively #'beginning-of-line)
               (exchange-point-and-mark)
               (call-interactively #'end-of-line)
               (when (not (eobp))
                 (call-interactively #'next-line)
                 (call-interactively #'beginning-of-line)))))
    (unless (use-region-p)
      (push-mark (point)))
    (if (< (point) (mark))
        (progn
          (funcall sel)
          (exchange-point-and-mark))
      (progn
        (exchange-point-and-mark)
        (funcall sel)))))

(defun purr--fill-typeable-keys (map)
  "Bind typeable characters in MAP to `ignore'."
  (dotimes (i (- 127 32))
    (define-key map (char-to-string (+ 32 i)) #'ignore))
  (define-key map (kbd "RET") #'ignore))

(purr--fill-typeable-keys purr-mode-map)

;;; Basic purr keybinds

(define-key purr-mode-map (kbd "SPC") mode-specific-map)
(define-key purr-mode-map (kbd "i") #'purr-exit)
(define-key purr-mode-map (kbd "h") #'backward-char)
(define-key purr-mode-map (kbd "j") #'next-line)
(define-key purr-mode-map (kbd "k") #'previous-line)
(define-key purr-mode-map (kbd "l") #'forward-char)
(define-key purr-mode-map (kbd "n") #'next-line)
(define-key purr-mode-map (kbd "p") #'previous-line)
(define-key purr-mode-map (kbd "P") #'yank)
(define-key purr-mode-map (kbd "SPC P") #'consult-yank-from-kill-ring)
(define-key purr-mode-map (kbd "a") #'move-beginning-of-line)
(define-key purr-mode-map (kbd "e") #'move-end-of-line)
(define-key purr-mode-map (kbd "c") #'purr-change)
(define-key purr-mode-map (kbd "d") #'purr-kill-region-if-active)
(define-key purr-mode-map (kbd "y") #'kill-ring-save)
(define-key purr-mode-map (kbd "w") #'expreg-expand)
(define-key purr-mode-map (kbd "W") #'expreg-contract)
(define-key purr-mode-map (kbd "v") #'set-mark-command)
(define-key purr-mode-map (kbd "V") #'purr-select-entire-lines)
(define-key purr-mode-map (kbd "u") #'undo)
(define-key purr-mode-map (kbd "o") #'purr-insert-below)
(define-key purr-mode-map (kbd "O") #'purr-insert-above)
(define-key purr-mode-map (kbd "f") #'forward-word)
(define-key purr-mode-map (kbd "b") #'backward-word)
(define-key purr-mode-map (kbd "x") #'execute-extended-command)
(define-key purr-mode-map (kbd ":") #'eval-expression)
(define-key purr-mode-map (kbd ";") #'exchange-point-and-mark)
(define-key purr-mode-map (kbd "[") #'backward-paragraph)
(define-key purr-mode-map (kbd "]") #'forward-paragraph)

;;; Additional keybinds and functions

(defun purr-org-table-change-field ()
  "Blank the org table field then begin inserting text."
  (interactive)
  (org-table-blank-field)
  (purr-exit))

(define-key purr-mode-map (kbd "SPC o t c") #'purr-org-table-change-field)

(global-purr-mode 1)
