(add-to-list '+package 'elpy)

(setq elpy-remove-modeline-lighter nil
      elpy-modules
      '(elpy-module-company
        elpy-module-eldoc
        elpy-module-flymake
        elpy-module-pyvenv
        elpy-module-django))

(with-eval-after-load 'elpy
  (define-key elpy-mode-map [remap format-dwim] 'elpy-format-code))
