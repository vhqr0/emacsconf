;; (setq eglot-server-programs
;;       '(((c-mode c++-mode) . ("clangd" "--header-insertion=never"))))

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
