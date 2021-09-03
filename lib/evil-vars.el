;;; evil-vars.el --- Settings and variables -*- lexical-binding: t -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.14.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(declare-function evil-add-command-properties "evil-common"
                  (command &rest properties))

;;; Hooks

(defvar evil-after-load-hook nil
  "Functions to be run when loading of Evil is finished.
This hook can be used the execute some initialization routines
when Evil is completely loaded.")

;;; Initialization

(defvar evil-pending-custom-initialize nil
  "A list of pending initializations for custom variables.
Each element is a triple (FUNC VAR VALUE). When Evil is
completely loaded then the functions (funcall FUNC VAR VALUE) is
called for each element. FUNC should be a function suitable for
the :initialize property of `defcustom'.")

(defun evil-custom-initialize-pending-reset (var value)
  "Add a pending customization with `custom-initialize-reset'."
  (push (list 'custom-initialize-reset var value)
        evil-pending-custom-initialize))

(defun evil-run-pending-custom-initialize ()
  "Executes the pending initializations.
See `evil-pending-custom-initialize'."
  (dolist (init evil-pending-custom-initialize)
    (apply (car init) (cdr init)))
  (remove-hook 'evil-after-load-hook 'evil-run-pending-custom-initialize))
(add-hook 'evil-after-load-hook 'evil-run-pending-custom-initialize)

;;; Setters

(defun evil-set-custom-state-maps (var pending-var key _make newlist)
  "Changes the list of special keymaps.
VAR         is the variable containing the list of keymaps.
PENDING-VAR is the variable containing the list of the currently pending
            keymaps.
KEY         the special symbol to be stored in the keymaps.
MAKE        the creation function of the special keymaps.
NEWLIST     the list of new special keymaps."
  (set-default pending-var newlist)
  (when (default-boundp var)
    (dolist (map (default-value var))
      (when (and (boundp (car map))
                 (keymapp (default-value (car map))))
        (define-key (default-value (car map)) (vector key) nil))))
  (set-default var newlist)
  (evil-update-pending-maps))

(defun evil-update-pending-maps (&optional _file)
  "Tries to set pending special keymaps.
This function should be called from an `after-load-functions'
hook."
  (let ((maps '((evil-make-overriding-map . evil-pending-overriding-maps)
                (evil-make-intercept-map . evil-pending-intercept-maps))))
    (while maps
      (let* ((map (pop maps))
             (make (car map))
             (pending-var (cdr map))
             (pending (symbol-value pending-var))
             newlist)
        (while pending
          (let* ((map (pop pending))
                 (kmap (and (boundp (car map))
                            (keymapp (symbol-value (car map)))
                            (symbol-value (car map))))
                 (state (cdr map)))
            (if kmap
                (funcall make kmap state)
              (push map newlist))))
        (set-default pending-var newlist)))))

;;; Customization group

(defgroup evil nil
  "Extensible vi layer."
  :group 'emulations
  :prefix 'evil-)

(defcustom evil-shift-width 4
  "\\<evil-normal-state-map>
The number of columns by which a line is shifted.
This applies to the shifting operators \\[evil-shift-right] and \
\\[evil-shift-left]."
  :type 'integer
  :group 'evil)
(make-variable-buffer-local 'evil-shift-width)

(defcustom evil-shift-round t
  "\\<evil-normal-state-map>
Whether shifting rounds to the nearest multiple.
If non-nil, \\[evil-shift-right] and \\[evil-shift-left] adjust line
indentation to the nearest multiple of `evil-shift-width'."
  :type 'boolean
  :group 'evil)
(make-variable-buffer-local 'evil-shift-round)

(defcustom evil-indent-convert-tabs t
  "\\<evil-normal-state-map>
If non-nil, the \\[evil-indent] operator converts between leading tabs and spaces.
Whether tabs are converted to spaces or vice versa depends on the
value of `indent-tabs-mode'."
  :type 'boolean
  :group 'evil)

(defcustom evil-default-cursor t
  "The default cursor.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'evil)

(defvar evil-force-cursor nil
  "Overwrite the current states default cursor.")

(defcustom evil-respect-visual-line-mode nil
  "\\<evil-motion-state-map>
Whether movement commands respect `visual-line-mode'.
If non-nil, `visual-line-mode' is generally respected when it is
on.  In this case, motions such as \\[evil-next-line] and
\\[evil-previous-line] navigate by visual lines (on the screen) rather
than \"physical\" lines (defined by newline characters).  If nil,
the setting of `visual-line-mode' is ignored.

This variable must be set before Evil is loaded."
  :type 'boolean
  :group 'evil)

(defcustom evil-kbd-macro-suppress-motion-error nil
  "\\<evil-motion-state-map>
Whether left/right motions signal errors in keyboard macros.
This variable only affects beginning-of-line or end-of-line errors
regarding the motions \\[evil-backward-char] and \\[evil-forward-char]
respectively.  This may be desired since such errors cause macro
definition or execution to be terminated.  There are four
possibilities:

- `record': errors are suppressed when recording macros, but not when
  replaying them.
- `replay': errors are suppressed when replaying macros, but not when
  recording them.
- `t': errors are suppressed in both cases.
- `nil': errors are never suppressed."
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Record" :value record)
                (const :tag "Replay" :value replay)
                (const :tag "Both" :value t))
  :group 'evil)

(defcustom evil-mode-line-format 'before
  "The position of the state tag in the mode line.
If set to `before' or `after', the tag is placed at the beginning
or the end of the mode-line, respectively.  If nil, there is no
tag.  Otherwise it should be a cons cell (WHERE . WHICH), where
WHERE is either `before' or `after', and WHICH is a symbol in
`mode-line-format'.  The tag is then placed before or after that
symbol, respectively."
  :type '(radio :value 'before
                (const before)
                (const after)
                (cons :tag "Next to symbol"
                      (choice :value after
                              (const before)
                              (const after))
                      symbol))
  :group 'evil)

(defcustom evil-mouse-word 'evil-word
  "The thing-at-point symbol for double click selection.
The double-click starts visual state in a special word selection
mode. This symbol is used to determine the words to be
selected. Possible values are `evil-word' or `evil-WORD'."
  :type 'symbol
  :group 'evil)

(defcustom evil-bigword "^ \t\r\n"
  "The set of characters to be interpreted as WORD boundaries.
This is enclosed with square brackets and used as a regular
expression.  By default, whitespace characters are considered
WORD boundaries."
  :type 'string
  :group 'evil)
(make-variable-buffer-local 'evil-bigword)

(defcustom evil-regexp-search t
  "\\<evil-motion-state-map>
Whether to use regular expressions for searching in \
\\[evil-search-forward] and \\[evil-search-backward]."
  :type  'boolean
  :group 'evil)

(defcustom evil-search-wrap t
  "\\<evil-motion-state-map>
Whether search with \\[evil-search-forward] and \
\\[evil-search-backward] wraps around the buffer.
If this is non-nil, search stops at the buffer boundaries."
  :type  'boolean
  :group 'evil)

(defcustom evil-flash-delay 2
  "\\<evil-motion-state-map>
Time in seconds to flash search matches after \\[evil-search-next] and \
\\[evil-search-previous]."
  :type  'number
  :group 'evil)

(defcustom evil-auto-balance-windows t
  "If non-nil window creation and deletion trigger rebalancing."
  :type 'boolean
  :group 'evil)

(defcustom evil-split-window-below nil
  "If non-nil split windows are created below."
  :type 'boolean
  :group 'evil)

(defcustom evil-vsplit-window-right nil
  "If non-nil vertically split windows with are created to the right."
  :type 'boolean
  :group 'evil)

(defcustom evil-esc-delay 0.01
  "The time, in seconds, to wait for another key after escape.
If no further event arrives during this time, the event is
translated to `ESC'.  Otherwise, it is translated according to
`input-decode-map'.  This does not apply in Emacs state, and may
also be inhibited by setting `evil-inhibit-esc'."
  :type 'number
  :group 'evil)

(defvar evil-esc-mode nil
  "Non-nil if `evil-esc-mode' is enabled.")

(defvar evil-esc-map nil
  "Original ESC prefix map in `input-decode-map'.
Used by `evil-esc-mode'.")

(defvar evil-inhibit-esc nil
  "If non-nil, the \\e event will never be translated to 'escape.")

(defcustom evil-intercept-esc 'always
  "Whether Evil should intercept the escape key.
In the terminal, escape and a meta key sequence both generate the
same event.  In order to distingush these, Evil uses
`input-decode-map'.  It is not necessary to do this in a graphical
Emacs session.  However, if you prefer to use `C-[' as escape (which
is identical to the terminal escape key code), this interception must
also happen in graphical Emacs sessions.  Set this variable to
`always', t (only in the terminal) or nil (never intercept)."
  :type '(radio (const :tag "Never" :value nil)
                (const :tag "In terminal only" :value t)
                (const :tag "Always" :value always))
  :group 'evil)

(defcustom evil-echo-state t
  "Whether to signal the current state in the echo area."
  :type 'boolean
  :group 'evil)

(defcustom evil-default-state 'normal
  "The default Evil state.
This is the state a buffer starts in when it is not otherwise
configured (see `evil-set-initial-state' and
`evil-buffer-regexps').  The value may be one of `normal',
`insert', `visual', `replace', `operator', `motion' and `emacs'."
  :type  'symbol
  :group 'evil)

(defcustom evil-buffer-regexps
  '(("^ \\*load\\*" . nil))
  "Regular expressions determining the initial state for a buffer.
Entries have the form (REGEXP . STATE), where REGEXP is a regular
expression matching the buffer's name and STATE is one of `normal',
`insert', `visual', `replace', `operator', `motion', `emacs' and
`nil'.  If STATE is `nil', Evil is disabled in the buffer."
  :type '(alist :key-type string :value-type symbol)
  :group 'evil)

(defcustom evil-emacs-state-modes
  '(bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    calc-mode
    debugger-mode
    ediff-mode
    ediff-meta-mode
    etags-select-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    occur-mode
    proced-mode
    tar-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-hg-log-view-mode
    vc-svn-log-view-mode)
  "Modes that should come up in Emacs state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-insert-state-modes
  '(comint-mode
    inferior-emacs-lisp-mode
    inferior-python-mode
    shell-mode
    term-mode)
  "Modes that should come up in Insert state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-motion-state-modes
  '(apropos-mode
    Buffer-menu-mode
    compilation-mode
    diff-mode
    dictionary-mode
    help-mode
    Info-mode
    Man-mode
    woman-mode)
  "Modes that should come up in Motion state."
  :type  '(repeat symbol)
  :group 'evil)

(defvar evil-pending-overriding-maps nil
  "An alist of pending overriding maps.")

(defvar evil-pending-intercept-maps nil
  "An alist of pending intercept maps.")

(defcustom evil-overriding-maps '()
  "Keymaps that should override Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be overridden. If STATE is nil, all states are
overridden."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil
  :set #'(lambda (var values)
           (set-default var values)
           (evil-set-custom-state-maps 'evil-overriding-maps
                                       'evil-pending-overriding-maps
                                       'override-state
                                       'evil-make-overriding-map
                                       values))
  :initialize 'evil-custom-initialize-pending-reset)

(add-hook 'after-load-functions #'evil-update-pending-maps)

(defcustom evil-intercept-maps
  '((edebug-mode-map . nil))
  "Keymaps that should intercept Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be intercepted. If STATE is nil, all states are
intercepted."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil
  :set #'(lambda (var values)
           (set-default var values)
           (evil-set-custom-state-maps 'evil-intercept-maps
                                       'evil-pending-intercept-maps
                                       'intercept-state
                                       'evil-make-intercept-map
                                       values))
  :initialize 'evil-custom-initialize-pending-reset)

;;; Variables

(defmacro evil-define-local-var (symbol &optional initvalue docstring)
  "Define SYMBOL as permanent buffer local variable, and return SYMBOL.
The parameters are the same as for `defvar', but the variable
SYMBOL is made permanent buffer local."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(evil-define-local-var evil-scroll-count 0
  "Holds last used prefix for `evil-scroll-up'
and `evil-scroll-down'.
Determines how many lines should be scrolled.
Default value is 0 - scroll half the screen.")

(evil-define-local-var evil-state nil
  "The current Evil state.
To change the state, use `evil-change-state'
or call the state function (e.g., `evil-normal-state').")

;; these may be used inside `evil-define-state'
(evil-define-local-var evil-next-state nil
  "The Evil state being switched to.")

(evil-define-local-var evil-previous-state-alist nil
  "For Each evil state the Evil state being switched from.")

(evil-define-local-var evil-previous-state nil
  "The Evil state being switched from.")

(defvar evil-execute-in-emacs-state-buffer nil
  "The buffer of the latest `evil-execute-in-emacs-state'.
When this command is being executed the current buffer is stored
in this variable. This is necessary in case the Emacs-command to
be called changes the current buffer.")

(evil-define-local-var evil-mode-line-tag nil
  "Mode-Line indicator for the current state.")
(put 'evil-mode-line-tag 'risky-local-variable t)

(defvar evil-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar evil-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar evil-minor-mode-keymaps-alist nil
  "Association list of Evil states to minor-mode keymap alists.
Entries have the form (STATE . MODE-MAP-ALIST), where
MODE-MAP-ALIST is an alist taking the form of
`minor-mode-map-alist'.")

(defvar evil-state-properties nil
  "Specifications made by `evil-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `evil-state-property'.")

(evil-define-local-var evil-mode-map-alist nil
  "Association list of keymaps to use for Evil modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")

(defvar evil-command-properties nil
  "Specifications made by `evil-define-command'.")

(defvar evil-change-commands '(evil-change)
  "Commands that wrap or replace `evil-change'.
This list exists to apply an inconsistency with vim's change command
to commands that wrap or redefine it. See emacs-evil/evil#916.")

(defvar evil-transient-vars '(cua-mode transient-mark-mode select-active-regions)
  "List of variables pertaining to Transient Mark mode.")

(defvar evil-transient-vals nil
  "Association list of old values for Transient Mark mode variables.
Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
whether the variable was previously buffer-local.")

(evil-define-local-var evil-no-display nil
  "If non-nil, various Evil displays are inhibited.
Use the macro `evil-without-display' to set this variable.")

(defvar evil-type-properties nil
  "Specifications made by `evil-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defvar evil-interactive-alist nil
  "Association list of Evil-specific interactive codes.")

(evil-define-local-var evil-motion-marker nil
  "Marker for storing the starting position of a motion.")

(evil-define-local-var evil-this-type nil
  "Current motion type.")

(evil-define-local-var evil-this-type-modified nil
  "Non-nil iff current motion type has been modified by the user.
If the type has been modified, this variable contains the new
type.")

(evil-define-local-var evil-this-register nil
  "Current register.")

(defvar evil-last-=-register-input nil
  "Most recent input from the `=' register. A string.")

(defvar evil-this-macro nil
  "Current macro register.")

(evil-define-local-var evil-this-operator nil
  "Current operator.")

(evil-define-local-var evil-this-motion nil
  "Current motion.")

(evil-define-local-var evil-this-motion-count nil
  "Current motion count.")

(defvar evil-last-register nil
  "The last executed register.")

(defvar evil-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defvar evil-inhibit-operator-value nil
  "This variable is used to transfer the value
of `evil-inhibit-operator' from one local scope to another.")

;; used by `evil-define-operator'
(defvar evil-operator-range-beginning nil
  "Beginning of `evil-operator-range'.")

(defvar evil-operator-range-end nil
  "End of `evil-operator-range'.")

(defvar evil-operator-range-type nil
  "Type of `evil-operator-range'.")

(defvar evil-operator-range-motion nil
  "Motion of `evil-operator-range'.")

(defvar evil-restriction-stack nil
  "List of previous restrictions.
Using `evil-with-restriction' stores the previous values of
`point-min' and `point-max' as a pair in this list.")

(evil-define-local-var evil-markers-alist
  '((?\( . evil-backward-sentence)
    (?\) . evil-forward-sentence)
    (?{ . evil-backward-paragraph)
    (?} . evil-forward-paragraph)
    (?' . evil-jump-backward-swap)
    (?` . evil-jump-backward-swap)
    (?< . evil-visual-beginning)
    (?> . evil-visual-goto-end))
  "Association list for markers.
Entries have the form (CHAR . DATA), where CHAR is the marker's
name and DATA is either a marker object as returned by `make-marker',
a variable, a movement function, or a cons cell (STRING NUMBER),
where STRING is a file path and NUMBER is a buffer position.
The global value of this variable holds markers available from
every buffer, while the buffer-local value holds markers available
only in the current buffer.")

(defconst evil-suppress-map (make-keymap)
  "Full keymap disabling default bindings to `self-insert-command'.")
(suppress-keymap evil-suppress-map t)

(defvar evil-read-key-map (make-sparse-keymap)
  "Keymap active during `evil-read-key'.
This keymap can be used to bind some commands during the
execution of `evil-read-key' which is usually used to read a
character argument for some commands, e.g. `evil-replace'.")

;; TODO: customize size of ring
(defvar evil-repeat-ring (make-ring 10)
  "A ring of repeat-informations to repeat the last command.")

(defvar evil-repeat-types
  '((t . evil-repeat-keystrokes)
    (change . evil-repeat-changes)
    (motion . evil-repeat-motion)
    (insert-at-point . evil-repeat-insert-at-point)
    (ignore . nil))
  "An alist of defined repeat-types.")

(defvar evil-recording-repeat nil
  "Whether we are recording a repeat.")

(defvar evil-recording-current-command nil
  "Whether we are recording the current command for repeat.")

(defvar evil-repeat-changes nil
  "Accumulated buffer changes for changed-based commands.")

(defvar evil-repeat-info nil
  "Information accumulated during current repeat.")

(defvar evil-repeat-buffer nil
  "The buffer in which the repeat started.
If the buffer is changed, the repeat is cancelled.")

(defvar evil-repeat-pos nil
  "The position of point at the beginning of an change-tracking
  editing command.")

(defvar evil-repeat-keys nil
  "The keys that invoked the current command.")

(defvar evil-last-repeat nil
  "Information about the latest repeat command.
This is a list of three elements (POINT COUNT UNDO-POINTER),
where POINT is the position of point before the latest repeat,
COUNT the count-argument of the latest repeat command and
UNDO-POINTER the head of the undo-list before the last command
has been repeated.")

(defvar evil-repeat-count nil
  "The explicit count when repeating a command.")

(defvar evil-maybe-remove-spaces nil
  "Flag to determine if newly inserted spaces should be removed.
See the function `evil-maybe-remove-spaces'.")

(evil-define-local-var evil-insert-count nil
  "The explicit count passed to an command starting Insert state.")

(evil-define-local-var evil-insert-vcount nil
  "The information about the number of following lines the
insertion should be repeated. This is list (LINE COLUMN COUNT)
where LINE is the line-number where the original insertion
started and COLUMN is either a number or function determining the
column where the repeated insertions should take place. COUNT is
number of repeats (including the original insertion).")

(defvar evil-insert-skip-empty-lines nil
  "Non-nil of the current insertion should not take place on
  lines at which the insertion point is behind the end of the
  line.")

(evil-define-local-var evil-insert-lines nil
  "Non-nil if the current insertion command is a line-insertion
command o or O.")

(evil-define-local-var evil-insert-repeat-info nil
  "Repeat information accumulated during an insertion.")

(evil-define-local-var evil-replace-alist nil
  "Association list of characters overwritten in Replace state.
The format is (POS . CHAR).")

(evil-define-local-var evil-echo-area-message nil
  "Previous value of `current-message'.")

(defvar evil-write-echo-area nil
  "If set to t inside `evil-save-echo-area', then the echo area
is not restored.")

(defvar evil-last-find nil
  "A pair (FUNCTION . CHAR) describing the lastest character
  search command.")

(defvar evil-last-paste nil
  "Information about the latest paste.
This should be a list (CMD COUNT POINT BEG END FIRSTVISUAL) where
CMD is the last paste-command (`evil-paste-before',
`evil-paste-after' or `evil-visual-paste'), COUNT is the repeat
count of the paste, POINT is the position of point before the
paste, BEG end END are the region of the inserted
text. FIRSTVISUAL is t if and only if the previous command was
the first visual paste (i.e. before any paste-pop).")

(evil-define-local-var evil-last-undo-entry nil
  "Information about the latest undo entry in the buffer.
This should be a pair (OBJ . CONS) where OBJ is the entry as an
object, and CONS is a copy of the entry.")

(evil-define-local-var evil-current-insertion nil
  "Information about the latest insertion in insert state.
This should be a pair (BEG . END) that describes the
buffer-region of the newly inserted text.")

(defvar evil-last-insertion nil
  "The last piece of inserted text.")

(defvar evil-last-small-deletion nil
  "The last piece of deleted text.
The text should be less than a line.")

(defvar evil-was-yanked-without-register t
  "Whether text being saved to the numbered-register ring was
not deleted and not yanked to a specific register.")

(defvar evil-paste-count nil
  "The count argument of the current paste command.")

(defvar evil-temporary-undo nil
  "When undo is disabled in current buffer.
Certain commands depending on undo use this variable
instead of `buffer-undo-list'.")

(evil-define-local-var evil-undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")

(defvar evil-in-single-undo nil
  "Set to non-nil if the current undo steps are connected.")

(defvar evil-flash-timer nil
  "Timer for flashing search results.")

(defvar evil-search-prompt nil
  "String to use for search prompt.")

(defvar evil-search-forward-history nil
  "History of forward searches.")

(defvar evil-search-backward-history nil
  "History of backward searches.")

(defvar evil-inner-text-objects-map (make-sparse-keymap)
  "Keymap for inner text objects.")

(defvar evil-outer-text-objects-map (make-sparse-keymap)
  "Keymap for outer text objects.")

(defvar evil-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(evil-define-local-var evil-input-method nil
  "Input method used in Insert state and Emacs state.")

;;; Visual state

(evil-define-local-var evil-visual-beginning nil
  "The beginning of the Visual selection, a marker.")

(evil-define-local-var evil-visual-end nil
  "The end of the Visual selection, a marker.")

(evil-define-local-var evil-visual-point nil
  "The position of point in Visual state, a marker.")

(evil-define-local-var evil-visual-mark nil
  "The position of mark in Visual state, a marker.")

(evil-define-local-var evil-visual-previous-mark nil
  "The position of mark before Visual state, a marker.")

(evil-define-local-var evil-visual-selection nil
  "The kind of Visual selection.
This is a selection as defined by `evil-define-visual-selection'.")

;; we could infer the direction by comparing `evil-visual-mark'
;; and `evil-visual-point', but destructive operations may
;; displace the markers
(evil-define-local-var evil-visual-direction 0
  "Whether point follows mark in Visual state.
Negative if point precedes mark, otherwise positive.
See also the function `evil-visual-direction'.")

(evil-define-local-var evil-visual-properties nil
  "Property list of miscellaneous Visual properties.")

(evil-define-local-var evil-visual-region-expanded nil
  "Whether the region matches the Visual selection.
That is, whether the positions of point and mark have been
expanded to coincide with the selection's boundaries.
This makes the selection available to functions acting
on Emacs' region.")

(evil-define-local-var evil-visual-overlay nil
  "Overlay for highlighting the Visual selection.
Not used for blockwise selections, in which case
see `evil-visual-block-overlays'.")

(evil-define-local-var evil-visual-block-overlays nil
  "Overlays for Visual Block selection, one for each line.
They are reused to minimize flicker.")

(defvar evil-visual-alist nil
  "Association list of Visual selection functions.
Elements have the form (NAME . FUNCTION).")

(evil-define-local-var evil-visual-x-select-timer nil
  "Timer for updating the X selection in visual state.")

(defvar evil-visual-x-select-timeout 0.1
  "Time in seconds for the update of the X selection.")

(defconst evil-version
  (eval-when-compile
    (with-temp-buffer
      (let ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file))))
        ;; git repository
        (if (and (file-exists-p (concat dir "/.git"))
                 (ignore-errors
                   (zerop (call-process "git" nil '(t nil) nil
                                        "rev-parse"
                                        "--short" "HEAD"))))
            (progn
              (goto-char (point-min))
              (concat "evil-git-"
                      (buffer-substring (point-min)
                                        (line-end-position))))
          ;; no repo, use plain version
          "1.14.0"))))
  "The current version of Evil")

(defun evil-version ()
  (interactive)
  (message "Evil version %s" evil-version))

(provide 'evil-vars)

;;; evil-vars.el ends here
