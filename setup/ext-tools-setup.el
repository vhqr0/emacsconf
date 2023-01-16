;;; -*- lexical-binding: t -*-



;;* git
(setq magit-section-visibility-indicator '("..." . t))
(define-key vc-prefix-map "j" 'magit-status)
(define-key vc-prefix-map "f" 'magit-file-dispatch)
(define-key vc-prefix-map "?" 'magit-dispatch)

;;* ls
(setq dired-listing-switches "-lha")

;;* grep
(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

;;* diff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;* ispell
(setq ispell-dictionary "american")
