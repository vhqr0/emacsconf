;; clangd
(with-eval-after-load 'eglot
  (setq eglot-server-programs
        (cons '((c-mode c++-mode) . ("clangd" "--header-insertion=never"))
              eglot-server-programs)))

;; pylsp
(setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (
                                  :pyflakes (:enabled :json-false)
                                  :flake8 (:enabled t)
                                  :yapf (:enabled t)))))
