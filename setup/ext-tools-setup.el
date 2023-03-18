;;; -*- lexical-binding: t -*-



;;* git
(setq magit-section-visibility-indicator '("..." . t))
(define-key vc-prefix-map "j" 'magit-status)
(define-key vc-prefix-map "f" 'magit-file-dispatch)
(define-key vc-prefix-map "?" 'magit-dispatch)

;;* shell
(defun +eshell-export-pager ()
  (setenv "PAGER" (expand-file-name "pager.py" +misc-directory)))

(add-hook 'eshell-mode-hook '+eshell-export-pager)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

;; https://github.com/emacs-evil/evil-collection/issues/545
(advice-add 'evil-collection-eshell-escape-stay :override 'ignore)

;;* ls
(setq dired-dwim-target t
      dired-listing-switches "-lha")

;;* grep
(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

;;* diff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;* ispell
(setq ispell-dictionary "american")
