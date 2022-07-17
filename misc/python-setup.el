(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt")

(setq +package (append +package '(highlight-indentation elpy)))

(add-hook 'python-mode-hook 'highlight-indentation-mode)

(setq elpy-remove-modeline-lighter nil
      elpy-project-root-finder-functions
      '(elpy-project-find-git-root elpy-project-find-python-root))

(setq elpy-modules
      '(elpy-module-company
        elpy-module-eldoc
        elpy-module-pyvenv))

(with-eval-after-load 'elpy
  (define-key elpy-mode-map [remap external-format] 'elpy-format-code))
