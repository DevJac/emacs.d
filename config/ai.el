;; -*- lexical-binding: t; -*-

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'gpt-6.1-sol)
  (setq gptel-backend
        ;; We used to configure ChatGPT like this. We needed to pass
        ;; the custom request-params to enable ChatGPT-side web tools.
        ;; However macher also sets its own tools and overrides the
        ;; tools we set here, so instead we set these tools through an
        ;; advise wrapper.
        ;;
        ;; (gptel-make-openai-oauth "ChatGPT"
        ;;   :request-params
        ;;   '(:tools [(:type "web_search")]
        ;;     :tool_choice "auto"))))
        (gptel-make-openai-oauth "ChatGPT")))

(use-package macher
  :ensure t
  :config
  (macher-install)
  (macher-enable))

;; macher specifies its own tools in the request params, but we want
;; our own customizations in there too. Neither gptel or macher
;; provides a nice way to do this, so we override this function and
;; inject our own tool customizations.
(cl-defmethod gptel--request-data :around ((_backend gptel-openai-oauth) _prompts)
  (let ((data (cl-call-next-method)))
    (plist-put data :tools
               (vconcat (plist-get data :tools)
                        [(:type "web_search")]))))
