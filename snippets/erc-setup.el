(with-eval-after-load 'erc-backend
  (require 'erc-sasl))

(global-set-key (kbd "<f9>") 'erc-track-switch-buffer)

(setq erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#security")))
