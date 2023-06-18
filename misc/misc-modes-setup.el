;;; cmake-mode
(use-package cmake-mode
  :defer t
  :config
  (+company-set-backends 'cmake-mode-hook '(company-cmake)))

;;; web
(use-package emmet-mode
  :hook ((js-mode mhtml-mode css-mode web-mode) . emmet-mode))

(use-package web-mode
  :magic ("\\.djhtml\\'" . web-mode)
  :init
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2))

;;; lisp
(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  (with-eval-after-load 'evil-eval
    (add-to-list 'evil-eval-alist '(lisp-mode . sly-eval-region))))

(use-package sly-macrostep
  :after sly
  :config
  (bind-key "C-c e" 'macrostep-expand sly-editing-mode-map)
  (with-eval-after-load 'sly-mrepl
    (bind-key "C-c e" 'macrostep-expand sly-mrepl-mode-map)))

;;; latex
(use-package auctex :defer t)

;;; go
(use-package go-mode
  :defer t
  :config
  (bind-key [remap format-buffer] 'gofmt go-mode-map))

(use-package go-dlv :defer t)
