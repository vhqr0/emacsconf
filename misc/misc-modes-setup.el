;;; cmake-mode
(use-package cmake-mode
  :defer t
  :config
  (+company-set-backends 'cmake-mode-hook '(company-cmake)))

;;; latex
(use-package auctex :defer t)

;;; go
(use-package go-mode
  :defer t
  :config
  (bind-key [remap format-buffer] 'gofmt go-mode-map))

(use-package go-dlv :defer t)
