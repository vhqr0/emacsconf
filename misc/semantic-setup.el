(with-eval-after-load 'semantic
  (define-key semantic-mode-map (kbd "C-c , .") 'semantic-ia-fast-jump)
  (define-key semantic-mode-map (kbd "C-c , h") 'semantic-ia-show-summary)
  (define-key semantic-mode-map (kbd "C-c , d") 'semantic-ia-show-doc)
  (define-key semantic-mode-map (kbd "C-c , v") 'semantic-ia-show-variants)
  (define-key semantic-mode-map (kbd "C-c , c") 'semantic-ia-describe-class))

(setq semantic-new-buffer-setup-functions '((c-mode . semantic-default-c-setup)
                                            (c++-mode . semantic-default-c-setup)))
