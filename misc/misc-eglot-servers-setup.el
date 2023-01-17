;; clangd
(with-eval-after-load 'eglot
  (setq eglot-server-programs
        (cons '((c-mode c++-mode) . ("clangd" "--header-insertion=never"))
              eglot-server-programs)))
