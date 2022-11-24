;;; +autoload.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "ace-window" "ace-window.el" (0 0 0 0))
;;; Generated autoloads from ace-window.el

(autoload 'ace-select-window "ace-window" "\
Ace select window." t nil)

(autoload 'ace-delete-window "ace-window" "\
Ace delete window." t nil)

(autoload 'ace-swap-window "ace-window" "\
Ace swap window." t nil)

(autoload 'ace-delete-other-windows "ace-window" "\
Ace delete other windows." t nil)

(autoload 'ace-display-buffer "ace-window" "\
Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

\(fn BUFFER ALIST)" nil nil)

(autoload 'ace-window "ace-window" "\
Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

\(fn ARG)" t nil)

(defvar ace-window-display-mode nil "\
Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.")

(custom-autoload 'ace-window-display-mode "ace-window" nil)

(autoload 'ace-window-display-mode "ace-window" "\
Minor mode for showing the ace window key in the mode line.

This is a minor mode.  If called interactively, toggle the
`Ace-Window-Display mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ace-window-display-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ace-window" '("ace-window-mode" "aw-"))

;;;***

;;;### (autoloads nil "avy" "avy.el" (0 0 0 0))
;;; Generated autoloads from avy.el

(autoload 'avy-process "avy" "\
Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

\(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)" nil nil)

(autoload 'avy-goto-char "avy" "\
Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-char-in-line "avy" "\
Jump to the currently visible CHAR in the current line.

\(fn CHAR)" t nil)

(autoload 'avy-goto-char-2 "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

\(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil)

(autoload 'avy-goto-char-2-above "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR1 CHAR2 &optional ARG)" t nil)

(autoload 'avy-goto-char-2-below "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR1 CHAR2 &optional ARG)" t nil)

(autoload 'avy-isearch "avy" "\
Jump to one of the current isearch candidates." t nil)

(autoload 'avy-goto-word-0 "avy" "\
Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

\(fn ARG &optional BEG END)" t nil)

(autoload 'avy-goto-whitespace-end "avy" "\
Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

\(fn ARG &optional BEG END)" t nil)

(autoload 'avy-goto-word-1 "avy" "\
Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

\(fn CHAR &optional ARG BEG END SYMBOL)" t nil)

(autoload 'avy-goto-word-1-above "avy" "\
Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-word-1-below "avy" "\
Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1 "avy" "\
Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1-above "avy" "\
Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1-below "avy" "\
Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-subword-0 "avy" "\
Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

\(fn &optional ARG PREDICATE BEG END)" t nil)

(autoload 'avy-goto-subword-1 "avy" "\
Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-word-or-subword-1 "avy" "\
Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t nil)

(autoload 'avy-goto-line "avy" "\
Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

\(fn &optional ARG)" t nil)

(autoload 'avy-goto-line-above "avy" "\
Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

\(fn &optional OFFSET BOTTOM-UP)" t nil)

(autoload 'avy-goto-line-below "avy" "\
Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

\(fn &optional OFFSET BOTTOM-UP)" t nil)

(autoload 'avy-goto-end-of-line "avy" "\
Call `avy-goto-line' and move to the end of the line.

\(fn &optional ARG)" t nil)

(autoload 'avy-copy-line "avy" "\
Copy a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avy-move-line "avy" "\
Move a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avy-copy-region "avy" "\
Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

\(fn ARG)" t nil)

(autoload 'avy-move-region "avy" "\
Select two lines and move the text between them above the current line." t nil)

(autoload 'avy-kill-region "avy" "\
Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

\(fn ARG)" t nil)

(autoload 'avy-kill-ring-save-region "avy" "\
Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn ARG)" t nil)

(autoload 'avy-kill-whole-line "avy" "\
Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

\(fn ARG)" t nil)

(autoload 'avy-kill-ring-save-whole-line "avy" "\
Select line and save the whole selected line as if killed, but don’t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\(fn ARG)" t nil)

(autoload 'avy-setup-default "avy" "\
Setup the default shortcuts." nil nil)

(autoload 'avy-goto-char-timer "avy" "\
Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

(autoload 'avy-transpose-lines-in-region "avy" "\
Transpose lines in the active region." t nil)

(register-definition-prefixes "avy" '("avy-"))

;;;***

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

;;;### (autoloads nil "ivy-setup" "ivy-setup.el" (0 0 0 0))
;;; Generated autoloads from ivy-setup.el

(register-definition-prefixes "ivy-setup" '("counsel--set-variable" "ivy-"))

;;;***

;;;### (autoloads nil "link-hint" "link-hint.el" (0 0 0 0))
;;; Generated autoloads from link-hint.el

(autoload 'link-hint-define-type "link-hint" "\
Add a new type of link called NAME to link-hint.el.
PROPERTIES should be property value pairs to add to the symbol plist of
link-hint-NAME.

\(fn NAME &rest PROPERTIES)" nil nil)

(function-put 'link-hint-define-type 'lisp-indent-function 'defun)

(autoload 'link-hint-open-link "link-hint" "\
Use avy to open a visible link." t nil)

(autoload 'link-hint-copy-link "link-hint" "\
Copy a visible link of a supported type to the kill ring with avy.
`select-enable-clipboard' and `select-enable-primary' can be set to non-nil
values to copy the link to the clipboard and/or primary as well." t nil)

(autoload 'link-hint-open-multiple-links "link-hint" "\
Use avy to open multiple visible links at once." t nil)

(autoload 'link-hint-copy-multiple-links "link-hint" "\
Use avy to copy multiple visible links at once to the kill ring." t nil)

(autoload 'link-hint-open-all-links "link-hint" "\
Open all visible links." t nil)

(autoload 'link-hint-copy-all-links "link-hint" "\
Copy all visible links." t nil)

(autoload 'link-hint-open-link-at-point "link-hint" "\
Open the link with the highest priority at the point." t nil)

(autoload 'link-hint-copy-link-at-point "link-hint" "\
Copy the link with the highest priority at the point." t nil)

(register-definition-prefixes "link-hint" '("link-hint-"))

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

;;;### (autoloads nil "workspace" "workspace.el" (0 0 0 0))
;;; Generated autoloads from workspace.el

(autoload 'workspace-switch-to-workspace "workspace" "\
Read a workspace by NAME to switch in FRAME.
If FRAME is nil, use selected frame.

\(fn NAME &optional FRAME)" t nil)

(autoload 'workspace-switch-to-next-workspace "workspace" "\
Switch to next workspace in FRAME.
If FRAME is nil, use selected frame.

\(fn &optional FRAME)" t nil)

(autoload 'workspace-switch-to-workspace-undo "workspace" "\
Switch to recent switched workspace in FRAME.

\(fn &optional FRAME)" t nil)

(autoload 'workspace-remove-workspace "workspace" "\
Read a workspace by NAME to delete.
If DO-CONFIRM, confirm before delete.

\(fn NAME &optional DO-CONFIRM)" t nil)

(autoload 'workspace-rename-workspace "workspace" "\
Rename current WORKSPACE to NAME.
If DO-DUPLICATE, duplicate a new workspace to rename.
If DO-CONFIRM, confirm before cover other workspace.

\(fn WORKSPACE NAME &optional DO-DUPLICATE DO-CONFIRM)" t nil)

(autoload 'workspace-add-buffer "workspace" "\
Add current BUFFER-OR-NAME to current WORKSPACE.
DO-CONFIRM: compatible to other functions to warn when failed.

\(fn WORKSPACE &optional BUFFER-OR-NAME DO-CONFIRM)" t nil)

(autoload 'workspace-remove-buffer "workspace" "\
Read a BUFFER-OR-NAME to remove from current WORKSPACE buffers.
DO-CONFIRM: compatible to other functions to warn when failed.

\(fn WORKSPACE &optional BUFFER-OR-NAME DO-CONFIRM)" t nil)

(autoload 'workspace-switch-to-buffer "workspace" "\
Read a buffer to switch from current workspace buffers.
If OTHER-WINDOW, switch to buffer other window.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'workspace-switch-to-buffer-other-window "workspace" "\
Other window version of `workspace-switch-to-buffer'." t nil)

(autoload 'workspace-find-file "workspace" "\
Read a file to find from current workspace files.
If OTHER-WINDOW, find file other window.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'workspace-find-file-other-window "workspace" "\
Other window version of `workspace-find-file'." t nil)

(autoload 'workspace-save-file "workspace" "\
Save `workspace-alist' and their files to FILE.
If DO-CONFIRM, confirm before delete exists file.

\(fn FILE &optional DO-CONFIRM)" t nil)

(autoload 'workspace-load-file "workspace" "\
Load `workspace-alist' from FILE and recovery workspace files.
If DO-CONFIRM, confirm save current `workspace-alist' before load.

\(fn FILE &optional DO-CONFIRM)" t nil)

(defvar workspace-mode nil "\
Non-nil if Workspace mode is enabled.
See the `workspace-mode' command
for a description of this minor mode.")

(custom-autoload 'workspace-mode "workspace" nil)

(autoload 'workspace-mode "workspace" "\
Workspace Mode.
This mode is aim to display workspace information in modeline lighter.
It is not necessary but recommended to turn on this mode when you use workspace.

This is a minor mode.  If called interactively, toggle the
`Workspace mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='workspace-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar workspace-prefix-map (let ((map (make-sparse-keymap))) (define-key map "m" 'workspace-mode) (define-key map "s" 'workspace-switch-to-workspace) (define-key map "n" 'workspace-switch-to-next-workspace) (define-key map "u" 'workspace-switch-to-workspace-undo) (define-key map "K" 'workspace-remove-workspace) (define-key map "R" 'workspace-rename-workspace) (define-key map "a" 'workspace-add-buffer) (define-key map "k" 'workspace-remove-buffer) (define-key map "b" 'workspace-switch-to-buffer) (define-key map "4b" 'workspace-switch-to-buffer-other-window) (define-key map "f" 'workspace-find-file) (define-key map "4f" 'workspace-find-file-other-window) (define-key map "S" 'workspace-save-file) (define-key map "L" 'workspace-load-file) map))

(register-definition-prefixes "workspace" '("workspace-"))

;;;***

(provide '+autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; +autoload.el ends here
