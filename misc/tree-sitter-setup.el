(setq +package (append +package '(tree-sitter-langs)))

(dolist (hook '(c-mode-common-hook
                python-mode-hook
                js-mode-hook
                mhtml-mode-hook
                css-mode-hook))
  (add-hook hook 'tree-sitter-hl-mode))

(with-eval-after-load 'tree-sitter
  (setcdr (assq 'tree-sitter-mode minor-mode-alist) '("")))
