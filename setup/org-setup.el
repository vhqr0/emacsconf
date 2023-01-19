;;; -*- lexical-binding: t -*-



;;* maps

(define-key ctl-x-l-map "a" 'org-agenda)
(define-key ctl-x-l-map "c" 'org-capture)
(define-key ctl-x-l-map "w" 'org-store-link)
(define-key ctl-x-l-map "n" 'counsel-org-roam)



;;* dirs

(setq org-directory (expand-file-name "org" user-emacs-directory)
      org-roam-directory (expand-file-name "org-roam" user-emacs-directory))

(setq org-agenda-files (list org-directory)
      org-default-notes-file (expand-file-name "notes.org" org-directory))




;;* org

(setq org-special-ctrl-a/e t
      evil-org-key-theme '(navigation return textobjects additional shift calendar))

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

(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode 1))        ; Notice: load org-roam before edit roam files

;;** cli
;; (add-to-list '+package 'emacsql-sqlite3)
;; (setq org-roam-database-connector 'sqlite3)

;;** builtin (emacs29)
;; (add-to-list '+package 'emacsql-sqlite-builtin)
;; (setq org-roam-database-connector 'sqlite-builtin)
