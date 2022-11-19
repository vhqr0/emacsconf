(setq eglot-server-programs
      '(((c-mode c++-mode) . ("clangd" "--header-insertion=never"))
        (python-mode . ("pylsp"))
        ;; (python-mode . ("pyright-langserver" "--stdio"))
        (js-mode . ("typescript-language-server" "--stdio"))
        (mhtml-mode . ("vscode-html-language-server" "--stdio"))
        (css-mode . ("vscode-css-language-server" "--stdio"))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq-local eglot-ignored-server-capabilities
                        '(:hoverProvider
                          :documentHighlightProvider))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local eglot-ignored-server-capabilities
                        '(:hoverProvider
                          :signatureHelpProvider
                          :documentHighlightProvider))))

(add-hook 'js-mode-hook
          (lambda ()
            (setq-local eglot-ignored-server-capabilities
                        '(:hoverProvider
                          :documentHighlightProvider))))
