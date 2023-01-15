;; cc
(setq flymake-cc-command 'cc-x-flymake-cc-command)
(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c h") 'cc-x-help))

;; python
(setq python-indent-guess-indent-offset nil ; inhibit verbose
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt")
(defun +python-comment-inline-offset-setup ()
  (setq-local comment-inline-offset 2))
(add-hook 'python-mode-hook '+python-comment-inline-offset-setup)

;; web
(setq js-indent-level 2
      css-indent-offset 2)
(add-hook 'js-mode-hook 'emmet-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; markdown
(setq markdown-fontify-code-blocks-natively t)
