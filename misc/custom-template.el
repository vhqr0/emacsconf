;;; ef-themes
;; (add-hook 'after-init-hook 'ef-themes-load-random)

;;; termbright
;; (add-hook 'after-init-hook (lambda () (load-theme 'termbright t)))

(dolist (setup '("fonts" "pyim"))
  (load-file (expand-file-name (concat setup "-setup.el") +misc-directory)))
