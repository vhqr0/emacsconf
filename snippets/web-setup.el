(setq +package (append +package '(web-mode js2-mode emmet-mode company-web)))

(defun web-mode-setup ()
  (setq-local company-backends (append '(company-web-html) company-backends))
  (company-mode 1)
  (emmet-mode 1))

(defun js2-mode-setup ()
  (company-mode 1))

(add-hook 'web-mode-hook 'web-mode-setup)
(add-hook 'js2-mode-hook 'js2-mode-setup)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
