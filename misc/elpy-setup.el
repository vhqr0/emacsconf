(use-package elpy
  :commands elpy-enable
  :init
  (setq elpy-remove-modeline-lighter nil
        elpy-modules
        '(elpy-module-company
          elpy-module-eldoc
          elpy-module-flymake
          elpy-module-pyvenv
          elpy-module-django))
  :config
  (bind-key [remap format-buffer] 'elpy-format-code elpy-mode-map))
