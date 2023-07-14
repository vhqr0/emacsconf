(use-package pdf-tools
  :commands pdf-view-mode
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  :config
  (pdf-tools-install))
