(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'gpt-5.2)
  (setq gptel-backend
        (gptel-make-openai "ChatGPT" :stream t :key gptel-api-key)))
