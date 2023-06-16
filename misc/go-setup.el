(use-package go-mode
  :defer t
  :config
  (bind-key [remap format-buffer] 'gofmt go-mode-map)
  (load-file (expand-file-name "go-dlv.el" +misc-directory)))
