(setq eglot-server-programs
      '(((c-mode c++-mode) . ("clangd" "--header-insertion=never"))
        (python-mode . ("pyright-langserver" "--stdio"))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq-local eglot-ignored-server-capabilities
                        '(:hoverProvider
                          :documentHighlightProvider))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local eglot-stay-out-of '(flymake)
                        eglot-ignored-server-capabilities
                        '(:hoverProvider
                          :signatureHelpProvider
                          :documentHighlightProvider))))
