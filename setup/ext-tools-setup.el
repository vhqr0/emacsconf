;;; -*- lexical-binding: t -*-



;;* git
(use-package magit
  :bind (:map vc-prefix-map
              ("j" . magit-status)
              ("f" . magit-file-dispatch)
              ("?" . magit-dispatch))
  :init
  (setq magit-bind-magit-project-status nil
        magit-define-global-key-bindings nil
        magit-section-visibility-indicator '("..." . t))
  :config
  (bind-key "m" 'magit-project-status project-prefix-map)
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))

;;* shell
(use-package eshell
  :ensure nil
  :defer t
  :config
  (defun +eshell-export-pager ()
    (setenv "PAGER" (expand-file-name "pager.py" +misc-directory)))
  (add-hook 'eshell-mode-hook '+eshell-export-pager)
  ;; https://github.com/emacs-evil/evil-collection/issues/545
  (advice-add 'evil-collection-eshell-escape-stay :override 'ignore))

(use-package with-editor
  :hook (eshell-mode . with-editor-export-editor))

;;* ls
(use-package dired
  :ensure nil
  :defer t 
  :init
  (setq dired-dwim-target t
        dired-listing-switches "-lha"))

;;* grep
(use-package wgrep
  :defer t
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;;* diff
(use-package ediff
  :ensure nil
  :defer t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;* ispell
(use-package ispell
  :ensure nil
  :defer t
  :init
  (setq ispell-dictionary "american"))
