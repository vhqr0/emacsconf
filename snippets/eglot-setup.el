(setq eglot-server-programs
      '(((c-mode c++-mode) . ("clangd" "--header-insertion=never"))
        (python-mode . ("pyright-langserver" "--stdio"))))
