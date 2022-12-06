(setq org-directory (expand-file-name "~/org")
      org-agenda-files (list org-directory)
      org-default-notes-file (expand-file-name "notes.org" org-directory))

(setq org-special-ctrl-a/e t
      evil-org-key-theme
      '(navigation return textobjects shift calendar))

(define-key ctl-x-l-map "a" 'org-agenda)
(define-key ctl-x-l-map "c" 'org-capture)
(define-key ctl-x-l-map "w" 'org-store-link)
(define-key ctl-x-l-map "l" 'counsel-org-roam)

(add-hook 'org-mode-hook 'evil-org-mode)

(with-eval-after-load 'org
  (org-roam-db-autosync-mode 1))

(provide 'org-setup)
