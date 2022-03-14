(global-set-key "\M-#" 'erc-track-switch-buffer)

(setq erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#security")))

(add-hook 'erc-mode-hook 'eve-change-mode-to-vi)
