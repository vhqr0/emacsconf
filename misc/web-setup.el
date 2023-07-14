(use-package emmet-mode
  :hook ((js-mode mhtml-mode css-mode web-mode) . emmet-mode))

(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :init
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2))
