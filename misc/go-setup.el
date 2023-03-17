(add-to-list '+package 'go-mode)
(with-eval-after-load 'go-mode
  (define-key go-mode-map [remap format-dwim] 'gofmt)
  (load-file (expand-file-name "go-dlv.el" +misc-directory)))
