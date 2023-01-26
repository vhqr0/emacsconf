(add-to-list '+package 'htmlize)
(add-to-list '+package 'evil-org)

(define-key ctl-x-l-map "a" 'org-agenda)
(define-key ctl-x-l-map "c" 'org-capture)
(define-key ctl-x-l-map "w" 'org-store-link)

(setq org-directory (expand-file-name "org" user-emacs-directory)
      org-agenda-files (list org-directory)
      org-default-notes-file (expand-file-name "notes.org" org-directory))

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

(defun +org-html-paragraph-around (func paragraph contents info)
  "Fix ox-html cjk export."
  (let ((fixed-contents
         (replace-regexp-in-string
          "\\([[:multibyte:]]\\) *\n *\\([[:multibyte:]]\\)"
          "\\1\\2"
          contents)))
    (funcall func paragraph fixed-contents info)))

(advice-add 'org-html-paragraph :around '+org-html-paragraph-around)
