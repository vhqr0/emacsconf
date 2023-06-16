(use-package semantic
  :ensure nil
  :defer t
  :init
  (setq semantic-new-buffer-setup-functions
        '((c-mode . semantic-default-c-setup)
          (c++-mode . semantic-default-c-setup)))
  :config
  (bind-keys :map semantic-mode-map
             ("C-c , ." . semantic-ia-fast-jump)
             ("C-c , h" . semantic-ia-show-summary)
             ("C-c , d" . semantic-ia-show-doc)
             ("C-c , v" . semantic-ia-show-variants)
             ("C-c , c" . semantic-ia-describe-class)))
