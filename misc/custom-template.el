(add-to-list '+package 'ef-themes)
(add-hook 'after-init-hook 'ef-themes-load-random)

(dolist (setup '("fonts" "latex" "pyim"))
  (load-file (expand-file-name (concat setup "-setup.el") +misc-directory)))
