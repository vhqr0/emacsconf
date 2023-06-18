;; clangd
(use-package eglot
  :ensure nil
  :defer t
  :config
  ;; clangd
  (setq eglot-server-programs
        (cons '((c-mode c++-mode) . ("clangd" "--header-insertion=never"))
              eglot-server-programs))
  ;; pylsp
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (
                                    :pyflakes (:enabled :json-false)
                                    :pycodestyle (:enabled :json-false)
                                    :mccabe (:enabled :json-false)
                                    :flake8 (:enabled t)
                                    :yapf (:enabled t))))))
