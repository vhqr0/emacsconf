;;; cmake-mode
(add-to-list '+package 'cmake-mode)
(with-eval-after-load 'cmake-mode
  (+company-set-backends 'cmake-mode-hook '(company-cmake)))

;;; web-mode
(add-to-list '+package 'web-mode)
(setq web-mode-code-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-hook 'web-mode-hook 'emmet-mode)

;;; go-mode
(add-to-list '+package 'go-mode)
(with-eval-after-load 'go-mode
  (define-key go-mode-map [remap format-dwim] 'gofmt))

;;; lisp
(add-to-list '+package 'sly)
(setq inferior-lisp-program "sbcl")

;;; latex
(add-to-list '+package 'auctex)
