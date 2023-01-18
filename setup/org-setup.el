;;; -*- lexical-binding: t -*-



;;* org

(setq org-directory (expand-file-name "org" user-emacs-directory)
      org-agenda-files (list org-directory)
      org-default-notes-file (expand-file-name "notes.org" org-directory))

(setq org-special-ctrl-a/e t
      evil-org-key-theme '(navigation return textobjects additional shift calendar))

(define-key ctl-x-l-map "a" 'org-agenda)
(define-key ctl-x-l-map "c" 'org-capture)
(define-key ctl-x-l-map "w" 'org-store-link)

(defun +org-setup ()
  (modify-syntax-entry ?< "." org-mode-syntax-table) ; inhibit show paren and electric pair
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook '+org-setup)
(add-hook 'org-mode-hook 'evil-org-mode)

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-tempo)

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))



;;* org-roam

;; use sqlite3 cli to avoid compilation
;; (add-to-list '+package 'emacsql-sqlite3)
;; (setq org-roam-database-connector 'sqlite3)

(setq org-roam-directory (expand-file-name "org-roam" user-emacs-directory))

(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(define-key ctl-x-l-map "n" 'counsel-org-roam)

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode 1))        ; Notice: load org-roam before edit roam files
