(use-package hy-mode
  :defer t
  :init
  (setq hy-jedhy--enable? nil
        hy-shell--interpreter-args nil)
  (autoload 'hy-syntax-propertize-function "hy-mode")
  :config
  (add-hook 'hy-mode-hook 'paredit-mode)
  (add-hook 'inferior-hy-mode 'paredit-mode)

  (evil-define-key 'normal hy-mode-map "gz" 'run-hy)

  (defun hy-shell-macroexpand-current-form ()
    (interactive)
    (save-selected-window
      (hy-shell--eval-1
        (format "(hy.macroexpand '%s)" (hy--current-form-string)))))
  (bind-key "C-c C-m" 'hy-shell-macroexpand-current-form hy-mode-map)

  (with-eval-after-load 'evil-eval
    (defun +hy-shell-send-region (start end)
      (save-selected-window
        (hy-shell--eval-1 (buffer-substring start end))))
    (add-to-list 'evil-eval-alist '(hy-mode . +hy-shell-send-region))))
