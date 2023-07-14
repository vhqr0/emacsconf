(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  (with-eval-after-load 'evil-eval
    (add-to-list 'evil-eval-alist '(lisp-mode . sly-eval-region))))

(use-package sly-macrostep
  :after sly
  :config
  (bind-key "C-c e" 'macrostep-expand sly-editing-mode-map))
