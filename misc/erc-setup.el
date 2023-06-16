(with-eval-after-load 'erc-backend
  (load-file (expand-file-name "erc-sasl.el" +misc-directory)))

(bind-key "<f9>" 'erc-track-switch-buffer)

(setq erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#security")))
