;;; +autoload.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "cc-x" "cc-x.el" (0 0 0 0))
;;; Generated autoloads from cc-x.el

(autoload 'cc-x-flymake-cc-command "cc-x" nil nil nil)

(autoload 'cc-help "cc-x" nil t nil)

(put 'global-compute-completion-table 'safe-local-variable 'booleanp)

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

;;;### (autoloads nil "counsel-org-roam" "counsel-org-roam.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from counsel-org-roam.el

(autoload 'counsel-org-roam "counsel-org-roam" nil t nil)

(register-definition-prefixes "counsel-org-roam" '("counsel-org-roam--capture"))

;;;***

;;;### (autoloads nil "evil-setup" "evil-setup.el" (0 0 0 0))
;;; Generated autoloads from evil-setup.el

(register-definition-prefixes "evil-setup" '("evil-" "god-C-c"))

;;;***

;;;### (autoloads nil "evil-tobj-plus" "evil-tobj-plus.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from evil-tobj-plus.el

(autoload 'evil-tobj-plus-default-keybindings "evil-tobj-plus" nil nil nil)

(register-definition-prefixes "evil-tobj-plus" '("evil-"))

;;;***

;;;### (autoloads nil "goto-line-preview" "goto-line-preview.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from goto-line-preview.el

(autoload 'goto-line-preview "goto-line-preview" "\
Preview goto line." t nil)

(autoload 'goto-line-preview-relative "goto-line-preview" "\
Preview goto line relative." t nil)

(define-obsolete-function-alias 'goto-line-preview-goto-line 'goto-line-preview "0.1.1")

(register-definition-prefixes "goto-line-preview" '("goto-line-preview-"))

;;;***

;;;### (autoloads nil "ivy-setup" "ivy-setup.el" (0 0 0 0))
;;; Generated autoloads from ivy-setup.el

(register-definition-prefixes "ivy-setup" '("counsel--set-variable" "ivy-"))

;;;***

;;;### (autoloads nil "modus-summertime" "modus-summertime.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from modus-summertime.el

(autoload 'modus-summertime-toggle "modus-summertime" nil t nil)

(register-definition-prefixes "modus-summertime" '("modus-summertime-"))

;;;***

;;;### (autoloads nil "org-setup" "org-setup.el" (0 0 0 0))
;;; Generated autoloads from org-setup.el

(register-definition-prefixes "org-setup" '("org-prefix-map"))

;;;***

;;;### (autoloads nil "simple-x" "simple-x.el" (0 0 0 0))
;;; Generated autoloads from simple-x.el

(autoload 'simple-x-default-keybindings "simple-x" nil nil nil)

(register-definition-prefixes "simple-x" '("dired-do-xdg-open" "eshell-dwim" "external-format" "fixup-whitespace-nospace-mode" "minibuffer-yank-symbol" "occur-at-point" "prettier-" "rotate-window" "xclip" "xdg-open"))

;;;***

;;;### (autoloads nil "symbol-overlay" "symbol-overlay.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from symbol-overlay.el

(autoload 'symbol-overlay-mode "symbol-overlay" "\
Minor mode for auto-highlighting symbol at point.

This is a minor mode.  If called interactively, toggle the
`Symbol-Overlay mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `symbol-overlay-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'symbol-overlay-put "symbol-overlay" "\
Toggle all overlays of symbol at point." t nil)

(autoload 'symbol-overlay-count "symbol-overlay" "\
Show count of symbol at point." t nil)

(autoload 'symbol-overlay-remove-all "symbol-overlay" "\
Remove all highlighted symbols in the buffer.
When called interactively, then also reset
`symbol-overlay-keywords-alist'." t nil)

(autoload 'symbol-overlay-save-symbol "symbol-overlay" "\
Copy symbol at point." t nil)

(autoload 'symbol-overlay-toggle-in-scope "symbol-overlay" "\
Toggle overlays to be showed in buffer or only in scope." t nil)

(autoload 'symbol-overlay-echo-mark "symbol-overlay" "\
Jump back to the mark." t nil)

(autoload 'symbol-overlay-jump-next "symbol-overlay" "\
Jump to the next location of symbol at point." t nil)

(autoload 'symbol-overlay-jump-prev "symbol-overlay" "\
Jump to the previous location of symbol at point." t nil)

(autoload 'symbol-overlay-jump-first "symbol-overlay" "\
Jump to the first location." t nil)

(autoload 'symbol-overlay-jump-last "symbol-overlay" "\
Jump to the last location." t nil)

(autoload 'symbol-overlay-jump-to-definition "symbol-overlay" "\
Jump to the definition of symbol at point.
The definition syntax should be defined in a function stored in
`symbol-overlay-definition-function' that returns the definition's regexp
with the input symbol." t nil)

(autoload 'symbol-overlay-switch-forward "symbol-overlay" "\
Switch forward to another symbol." t nil)

(autoload 'symbol-overlay-switch-backward "symbol-overlay" "\
Switch backward to another symbol." t nil)

(autoload 'symbol-overlay-isearch-literally "symbol-overlay" "\
Isearch symbol at point literally." t nil)

(autoload 'symbol-overlay-query-replace "symbol-overlay" "\
Query replace symbol at point." t nil)

(autoload 'symbol-overlay-rename "symbol-overlay" "\
Rename symbol at point on all its occurrences." t nil)

(register-definition-prefixes "symbol-overlay" '("symbol-overlay-"))

;;;***

;;;### (autoloads nil "visual-regexp" "visual-regexp.el" (0 0 0 0))
;;; Generated autoloads from visual-regexp.el

(autoload 'vr/mc-mark "visual-regexp" "\
Convert regexp selection to multiple cursors.

\(fn REGEXP START END)" t nil)

(autoload 'vr/replace "visual-regexp" "\
Regexp-replace with live visual feedback.

\(fn REGEXP REPLACE START END)" t nil)

(autoload 'vr/query-replace "visual-regexp" "\
Use `vr/query-replace' like you would use `query-replace-regexp'.

\(fn REGEXP REPLACE START END)" t nil)

(register-definition-prefixes "visual-regexp" '("vr--" "vr/"))

;;;***

(provide '+autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; +autoload.el ends here
