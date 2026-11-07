;; -*- lexical-binding: t; -*-

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'gpt-6.1-sol)
  (setq gptel-backend
        (gptel-make-openai-oauth "ChatGPT")))
