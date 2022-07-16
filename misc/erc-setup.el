(with-eval-after-load 'erc-backend
  (load-file (expand-file-name "misc/erc-sasl.el" user-emacs-directory)))

(global-set-key (kbd "<f9>") 'erc-track-switch-buffer)

(setq erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#security")))
