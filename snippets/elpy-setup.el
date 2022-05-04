(setq +package (append +package '(elpy)))

(setq elpy-remove-modeline-lighter nil
      elpy-project-root-finder-functions
      '(elpy-project-find-git-root elpy-project-find-python-root))

(setq elpy-modules
      '(elpy-module-company
        elpy-module-eldoc
        elpy-module-pyvenv))
