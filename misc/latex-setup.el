(dolist (pkg '(pdf-tools auctex cdlatex))
  (add-to-list '+package pkg))

(with-eval-after-load 'pdf-tools (pdf-tools-install))
(autoload 'pdf-view-mode "pdf-view" "pdf view mode" t)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
