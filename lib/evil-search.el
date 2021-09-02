;;; evil-search.el --- Search and substitute -*- lexical-binding: t -*-

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

(require 'evil-core)
(require 'evil-common)
(require 'evil-ex)

;;; Code:

(defun evil-push-search-history (string forward)
  "Push STRING into the appropriate search history (determined by FORWARD)."
  (let* ((history-var (if forward
                          'evil-search-forward-history
                        'evil-search-backward-history))
         (history (symbol-value history-var)))
    (unless (equal (car-safe history) string)
      (set history-var (cons string history)))))

(defun evil-search-incrementally (forward regexp-p)
  "Search incrementally for user-entered text."
  (let ((evil-search-prompt (evil-search-prompt forward))
        (isearch-search-fun-function 'evil-isearch-function)
        (point (point))
        search-nonincremental-instead)
    (setq isearch-forward forward)
    (evil-save-echo-area
      (evil-without-input-method-hooks
       ;; set the input method locally rather than globally to ensure that
       ;; isearch clears the input method when it's finished
       (setq current-input-method evil-input-method)
       (if forward
           (isearch-forward regexp-p)
         (isearch-backward regexp-p))
       (evil-push-search-history isearch-string forward)
       (setq current-input-method nil))
      (when (/= (point) point)
        ;; position the point at beginning of the match only if the call to
        ;; `isearch' has really moved the point. `isearch' doesn't move the
        ;; point only if "C-g" is hit twice to exit the search, in which case we
        ;; shouldn't move the point either.
        (when (and forward isearch-other-end)
          (goto-char isearch-other-end))
        (when (and (eq point (point))
                   (not (string= isearch-string "")))
          (if forward
              (isearch-repeat-forward)
            (isearch-repeat-backward))
          (isearch-exit)
          (when (and forward isearch-other-end)
            (goto-char isearch-other-end)))
        (evil-flash-search-pattern
         (evil-search-message isearch-string forward))))))

(defun evil-flash-search-pattern (string &optional all)
  "Flash last search matches for duration of `evil-flash-delay'.
If ALL is non-nil, flash all matches. STRING is a message
to display in the echo area."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'evil-isearch-function)
        (isearch-case-fold-search case-fold-search)
        (disable #'(lambda (&optional _arg) (evil-flash-hook t))))
    (when evil-flash-timer
      (cancel-timer evil-flash-timer))
    (unless (or (null string)
                (string= string ""))
      (evil-echo-area-save)
      (evil-echo "%s" string)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (when all
        (setq isearch-lazy-highlight-wrapped nil
              isearch-lazy-highlight-start (point)
              isearch-lazy-highlight-end (point))
        (isearch-lazy-highlight-new-loop)
        (unless isearch-lazy-highlight-overlays
          (isearch-lazy-highlight-update)))
      (add-hook 'pre-command-hook #'evil-flash-hook nil t)
      (add-hook 'evil-operator-state-exit-hook #'evil-flash-hook nil t)
      (add-hook 'pre-command-hook #'evil-clean-isearch-overlays nil t)
      (setq evil-flash-timer
            (run-at-time evil-flash-delay nil disable)))))

(defun evil-clean-isearch-overlays ()
  "Clean isearch overlays unless `this-command' is search."
  (remove-hook 'pre-command-hook #'evil-clean-isearch-overlays t)
  (unless (memq this-command
                '(evil-search-backward
                  evil-search-forward
                  evil-search-next
                  evil-search-previous
                  evil-search-word-backward
                  evil-search-word-forward))
    (isearch-clean-overlays)))
(put 'evil-clean-isearch-overlays 'permanent-local-hook t)

(defun evil-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting
            ;; if the next command is also a search command
            (not (memq this-command
                       '(evil-search-backward
                         evil-search-forward
                         evil-search-next
                         evil-search-previous
                         evil-search-word-backward
                         evil-search-word-forward))))
    (evil-echo-area-restore)
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (lazy-highlight-cleanup t)
    (when evil-flash-timer
      (cancel-timer evil-flash-timer)))
  (remove-hook 'pre-command-hook #'evil-flash-hook t)
  (remove-hook 'evil-operator-state-exit-hook #'evil-flash-hook t))
(put 'evil-flash-hook 'permanent-local-hook t)

(defun evil-search-with-predicate (search-fun pred string bound noerror count)
  "Execute a search with a predicate function.
SEARCH-FUN is a search function (e.g. `re-search-forward') and
PREDICATE is a two-argument function satisfying the interface of
`isearch-filter-predicate', or `nil'.  STRING, BOUND, NOERROR and
COUNT are passed unchanged to SEARCH-FUN.  The first match
satisfying the predicate (or `nil') is returned."
  (catch 'done
    (while t
      (let ((result (funcall search-fun string bound noerror count)))
        (cond
         ((not result) (throw 'done nil))
         ((not pred) (throw 'done result))
         ((funcall pred (match-beginning 0) (match-end 0)) (throw 'done result)))))))

(defun evil-search-function (&optional forward regexp-p wrap predicate)
  "Return a search function.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, the input is a regular expression.
If WRAP is non-nil, the search wraps around the top or bottom
of the buffer.
If PREDICATE is non-nil, it must be a function accepting two
arguments: the bounds of a match, returning non-nil if that match is
acceptable."
  `(lambda (string &optional bound noerror count)
     (let ((start (point))
           (search-fun ',(if regexp-p
                             (if forward
                                 're-search-forward
                               're-search-backward)
                           (if forward
                               'search-forward
                             'search-backward)))
           result)
       (setq result (evil-search-with-predicate
                     search-fun ,predicate string
                     bound ,(if wrap t 'noerror) count))
       (when (and ,wrap (null result))
         (goto-char ,(if forward '(point-min) '(point-max)))
         (unwind-protect
             (setq result (evil-search-with-predicate
                           search-fun ,predicate string bound noerror count))
           (unless result
             (goto-char start))))
       result)))

(defun evil-isearch-function ()
  "Return a search function for use with isearch.
Based on `isearch-regexp' and `isearch-forward'."
  (evil-search-function isearch-forward evil-regexp-search evil-search-wrap 'isearch-filter-predicate))

(defun evil-search (string forward &optional regexp-p start)
  "Search for STRING and highlight matches.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, STRING is taken to be a regular expression.
START is the position to search from; if unspecified, it is
one more than the current position."
  (when (and (stringp string)
             (not (string= string "")))
    (let* ((orig (point))
           (start (or start
                      (if forward
                          (min (point-max) (1+ orig))
                        orig)))
           (isearch-regexp regexp-p)
           (isearch-forward forward)
           (case-fold-search
            (unless (and search-upper-case
                         (not (isearch-no-upper-case-p string nil)))
              case-fold-search))
           (search-func (evil-search-function
                         forward regexp-p evil-search-wrap 'isearch-filter-predicate)))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length string) nil string)
      ;; position to search from
      (goto-char start)
      (setq isearch-string string)
      (isearch-update-ring string regexp-p)
      (condition-case nil
          (funcall search-func string)
        (search-failed
         (goto-char orig)
         (user-error "\"%s\": %s not found"
                     string (if regexp-p "pattern" "string"))))
      ;; always position point at the beginning of the match
      (goto-char (match-beginning 0))
      ;; determine message for echo area
      (cond
       ((and forward (< (point) start))
        (setq string "Search wrapped around BOTTOM of buffer"))
       ((and (not forward) (> (point) start))
        (setq string "Search wrapped around TOP of buffer"))
       (t
        (setq string (evil-search-message string forward))))
      (evil-flash-search-pattern string t))))

(defun evil-search-word (forward unbounded symbol)
  "Search for word near point.
If FORWARD is nil, search backward, otherwise forward. If SYMBOL
is non-nil then the functions searches for the symbol at point,
otherwise for the word at point."
  (let ((string (car-safe regexp-search-ring)))
    (setq isearch-forward forward)
    (cond
     ((and (memq last-command
                 '(evil-search-word-forward
                   evil-search-word-backward))
           (stringp string)
           (not (string= string "")))
      (evil-search string forward t))
     (t
      (setq string (evil-find-thing forward (if symbol 'symbol 'evil-word)))
      (cond
       ((null string)
        (user-error "No word under point"))
       (unbounded
        (setq string (regexp-quote string)))
       (t
        (setq string
              (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                      (regexp-quote string)))))
      (evil-push-search-history string forward)
      (evil-search string forward t)))))

(defun evil--find-thing (forward thing)
  "Return a cons of THING near point as a string and its position.
THING should be a symbol understood by `thing-at-point',
e.g. 'symbol or 'word.  If FORWARD is nil, search backward,
otherwise forward.  Returns nil if nothing is found."
  (let ((move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp))
        string)
    (save-excursion
      (setq string (thing-at-point thing))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (null string) (not (funcall end)))
        (funcall move)
        (setq string (thing-at-point thing)))
      (when (stringp string)
        (set-text-properties 0 (length string) nil string))
      (when (> (length string) 0)
        (cons string (point))))))

(defun evil-find-thing (forward thing)
  "Return a THING near point as a string.
THING should be a symbol understood by `thing-at-point',
e.g. 'symbol or 'word.  If FORWARD is nil, search backward,
otherwise forward.  Returns nil if nothing is found."
  (car (evil--find-thing forward thing)))

(defun evil-find-word (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (evil-find-thing forward 'word))

(defun evil-find-symbol (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (evil-find-thing forward 'symbol))

(defun evil-search-prompt (forward)
  "Return the search prompt for the given direction."
  (if forward "/" "?"))

(defun evil-search-message (string forward)
  "Prefix STRING with the search prompt."
  (format "%s%s" (evil-search-prompt forward) string))

(defadvice isearch-message-prefix (around evil activate)
  "Use `evil-search-prompt'."
  (if evil-search-prompt
      (setq ad-return-value evil-search-prompt)
    ad-do-it))

(defadvice isearch-delete-char (around evil activate)
  "Exit search if no search string."
  (cond
   ((and evil-search-prompt (string= isearch-string ""))
    (let (search-nonincremental-instead)
      (setq isearch-success nil)
      (isearch-exit)))
   (t
    ad-do-it)))

(defadvice isearch-lazy-highlight-search (around evil activate)
  "Never wrap the search in this context."
  (let (evil-search-wrap)
    ad-do-it))

(provide 'evil-search)

;;; evil-search.el ends here
