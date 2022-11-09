(setq org-directory (expand-file-name "~/org")
      org-agenda-files (list org-directory)
      org-default-notes-file (expand-file-name "notes.org" org-directory))

(with-eval-after-load 'org
  (org-roam-db-autosync-mode 1)
  (define-key org-mode-map [remap eldoc-doc-buffer] 'org-roam-buffer-toggle)
  (define-key org-mode-map [remap evil-ret] 'org-open-at-point)
  (define-key org-mode-map [remap org-set-tags-command] 'counsel-org-tag))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map [remap org-agenda-set-tags] 'counsel-org-tag-agenda))



(defvar org-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'org-agenda)
    (define-key map "c" 'counsel-org-capture)
    (define-key map "w" 'org-store-link)
    (define-key map "y" 'org-insert-link-global)
    (define-key map "o" 'org-open-at-point-global)
    (define-key map "l" 'counsel-org-link)
    (define-key map "f" 'counsel-org-file)
    (define-key map "i" 'counsel-org-entity)
    (define-key map "j" 'counsel-org-goto-all)
    (define-key map "J" 'counsel-org-agenda-headlines)
    (define-key map "r" 'counsel-org-roam)
    map))

(global-set-key (kbd "C-c o") org-prefix-map)

(with-eval-after-load 'evil-setup
  (define-key evil-leader-map "O" org-prefix-map))

(provide 'org-setup)
