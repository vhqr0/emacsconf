(setq +package (append +package '(go-translate)))

(with-eval-after-load 'go-translate
  (setq gts-translate-list '(("en" "zh"))
        gts-default-translator
        (gts-translator :picker  (gts-prompt-picker)
                        :engines (list (gts-bing-engine))
                        :render  (gts-buffer-render)))
  (add-hook 'gts-after-buffer-prepared-hook 'evil-normal-state))

(global-set-key "\M-T" 'gts-do-translate)
