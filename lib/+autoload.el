;;; +autoload.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "cc-x" "cc-x.el" (0 0 0 0))
;;; Generated autoloads from cc-x.el

(autoload 'cc-x-flymake-cc-command "cc-x" nil nil nil)

(autoload 'cc-help "cc-x" nil t nil)

(defvar gtags-mode nil "\
Non-nil if gtags mode is enabled.
See the `gtags-mode' command
for a description of this minor mode.")

(custom-autoload 'gtags-mode "cc-x" nil)

(autoload 'gtags-mode "cc-x" "\
Gtags completion at point backend and xref backend enabled mode.

This is a minor mode.  If called interactively, toggle the `gtags
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='gtags-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'gtags-mode 'safe-local-variable 'booleanp)

(register-definition-prefixes "cc-x" '("cc-command" "global-" "gtags-"))

;;;***

;;;### (autoloads nil "erc-sasl" "erc-sasl.el" (0 0 0 0))
;;; Generated autoloads from erc-sasl.el

(register-definition-prefixes "erc-sasl" '("erc-sasl-login"))

;;;***

;;;### (autoloads nil "eshell-z" "eshell-z.el" (0 0 0 0))
;;; Generated autoloads from eshell-z.el

(autoload 'eshell-z "eshell-z" "\
Switch to eshell and change directory to DIR.

\(fn DIR)" t nil)

(register-definition-prefixes "eshell-z" '("eshell" "pcomplete/z"))

;;;***

;;;### (autoloads nil "evil-setup" "evil-setup.el" (0 0 0 0))
;;; Generated autoloads from evil-setup.el

(register-definition-prefixes "evil-setup" '("counsel-prefix-map" "evil-"))

;;;***

;;;### (autoloads nil "listify" "listify.el" (0 0 0 0))
;;; Generated autoloads from listify.el

(autoload 'listify-tab-completion "listify" "\
Tab completion with `listify-completion-in-region'.

\(fn ARG)" t nil)

(register-definition-prefixes "listify" '("listify-"))

;;;***

;;;### (autoloads nil "simple-x" "simple-x.el" (0 0 0 0))
;;; Generated autoloads from simple-x.el

(autoload 'simple-x-default-keybindings "simple-x" nil nil nil)

(register-definition-prefixes "simple-x" '("dired-do-xdg-open" "eshell-dwim" "external-format" "fixup-whitespace-nospace-mode" "minibuffer-yank-symbol" "rotate-window" "xclip" "xdg-open"))

;;;***

(provide '+autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; +autoload.el ends here
