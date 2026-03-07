(use-package lua-mode
  :ensure t
  :init
  (setq lua-indent-level 4))

;; Enable org babel evaluation for Lua.
(require 'ob-lua)

;; Alternatively,
; (add-to-list 'org-babel-load-languages '(lua . t))
; (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
