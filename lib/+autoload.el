;;; +autoload.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "eve" "eve.el" (0 0 0 0))
;;; Generated autoloads from eve.el

(autoload 'eve-change-mode-to-vi "eve" "\
Change mode to Vi." t nil)

(register-definition-prefixes "eve" '("eve-"))

;;;***

;;;### (autoloads nil "gtags" "gtags.el" (0 0 0 0))
;;; Generated autoloads from gtags.el

(defvar gtags-mode nil "\
Non-nil if gtags mode is enabled.
See the `gtags-mode' command
for a description of this minor mode.")

(custom-autoload 'gtags-mode "gtags" nil)

(autoload 'gtags-mode "gtags" "\
Gtags xref backend enabled mode.

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

(register-definition-prefixes "gtags" '("gtags-"))

;;;***

;;;### (autoloads nil "init-pyim" "init-pyim.el" (0 0 0 0))
;;; Generated autoloads from init-pyim.el

(with-eval-after-load 'pyim (require 'init-pyim))

(register-definition-prefixes "init-pyim" '("pyim-probe-eve-vi-mode"))

;;;***

;;;### (autoloads nil "linux-tools" "linux-tools.el" (0 0 0 0))
;;; Generated autoloads from linux-tools.el

(defvar xclip-program "xclip -selection clip")

(autoload 'xclip "linux-tools" "\


\(fn BEG END)" t nil)

(defvar xdg-open-program "xdg-open")

(autoload 'xdg-open "linux-tools" "\


\(fn &optional FILE)" t nil)

(autoload 'dired-do-xdg-open "linux-tools" nil t nil)

(defvar rg-program "rg")

(autoload 'rg "linux-tools" nil t nil)

;;;***

;;;### (autoloads nil "list-misc" "list-misc.el" (0 0 0 0))
;;; Generated autoloads from list-misc.el

(autoload 'list-kill-ring "list-misc" nil t nil)

(autoload 'list-imenu "list-misc" nil t nil)

(register-definition-prefixes "list-misc" '("list-imenu-select"))

;;;***

;;;### (autoloads nil "listify" "listify.el" (0 0 0 0))
;;; Generated autoloads from listify.el

(autoload 'listify-tab-completion "listify" "\
Tab completion with `listify-completion-in-region'." t nil)

(autoload 'listify-open "listify" "\
Open buffer or recent file with `listify-read'.
Open file in current directory if ARG not nil.

\(fn ARG)" t nil)

(autoload 'listify-history "listify" "\
View history with `listify-read'." t nil)

(register-definition-prefixes "listify" '("listify-"))

;;;***

;;;### (autoloads nil "terminalize" "terminalize.el" (0 0 0 0))
;;; Generated autoloads from terminalize.el

(defvar terminalize-mode nil "\
Non-nil if Terminalize mode is enabled.
See the `terminalize-mode' command
for a description of this minor mode.")

(custom-autoload 'terminalize-mode "terminalize" nil)

(autoload 'terminalize-mode "terminalize" "\
Terminalize emacs.

This is a minor mode.  If called interactively, toggle the
`Terminalize mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='terminalize-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "terminalize" '("terminalize-"))

;;;***

(provide '+autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; +autoload.el ends here
