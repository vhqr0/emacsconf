;;; cmake-mode
(setq +package (append +package '(cmake-mode)))
(with-eval-after-load 'cmake-mode
  (company-set-backends 'cmake-mode-hook '(company-cmake)))

;;; web-mode
(setq +package (append +package '(web-mode)))
(setq web-mode-code-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-hook 'web-mode-hook 'emmet-mode)

;;; lisp
(setq +package (append +package '(sly)))
(setq inferior-lisp-program "sbcl")
