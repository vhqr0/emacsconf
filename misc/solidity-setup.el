(use-package solidity-mode
  :defer t
  :config
  (defconst +solidity-c-style
    '((c-basic-offset . 4)
      (c-offsets-alist
       . ((substatement-open . 0) ; don't indent block's open brace after newline
          (statement-cont    . 0) ; *AD-HOC* correct indentation after struct or enum
          ))))
  (c-add-style "solidity" +solidity-c-style)
  (defun +solidity-set-style ()
    (subword-mode 1)
    (c-set-style "solidity"))
  (add-hook 'solidity-mode-hook '+solidity-set-style))
