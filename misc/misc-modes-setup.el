;;; cmake-mode
(setq +package (append +package '(cmake-mode)))
(with-eval-after-load 'cmake-mode
  (set-company-backends 'cmake-mode-hook '(company-cmake)))

;;; web-mode
(setq +package (append +package '(web-mode)))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-hook 'web-mode-hook 'emmet-mode)
