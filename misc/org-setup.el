(use-package htmlize :defer t)

(use-package org
  :ensure nil
  :defer t
  :init
  (setq org-directory (expand-file-name "org" user-emacs-directory)
        org-agenda-files (list org-directory)
        org-default-notes-file (expand-file-name "notes.org" org-directory))
  (setq org-special-ctrl-a/e t)
  :config
  (defun +org-setup ()
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))
  (add-hook 'org-mode-hook '+org-setup)
  (add-to-list 'org-modules 'org-tempo)
  (defun +org-html-paragraph-around (func paragraph contents info)
    "Fix ox-html cjk export."
    (let ((fixed-contents
           (replace-regexp-in-string
            "\\([[:multibyte:]]\\) *\n *\\([[:multibyte:]]\\)"
            "\\1\\2"
            contents)))
      (funcall func paragraph fixed-contents info)))
  (advice-add 'org-html-paragraph :around '+org-html-paragraph-around))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :init
  (setq evil-org-key-theme
        '(navigation return textobjects additional shift calendar))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
