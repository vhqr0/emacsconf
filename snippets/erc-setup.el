(with-eval-after-load 'erc-backend
  (require 'erc-sasl))

(define-key evil-leader-map "e" 'erc-track-switch-buffer)

(setq erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#security")))
