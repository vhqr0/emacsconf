;;; cmake-mode
(add-to-list '+package 'cmake-mode)
(with-eval-after-load 'cmake-mode
  (+company-set-backends 'cmake-mode-hook '(company-cmake)))

;;; web
(add-to-list '+package 'emmet-mode)
(add-hook 'js-mode-hook 'emmet-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(add-to-list '+package 'web-mode)
(setq web-mode-code-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-hook 'web-mode-hook 'emmet-mode)

;;; lisp
(add-to-list '+package 'sly)
(setq inferior-lisp-program "sbcl")

;;; latex
(add-to-list '+package 'auctex)
