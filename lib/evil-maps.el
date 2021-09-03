;;; evil-maps.el --- Default keymaps -*- lexical-binding: t -*-

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

(require 'evil-states)
(require 'evil-commands)
(require 'evil-common)

;;; Code:

;;; Normal state

(define-key evil-normal-state-map "i" 'evil-insert)
(define-key evil-normal-state-map "I" 'evil-insert-line)
(define-key evil-normal-state-map "a" 'evil-append)
(define-key evil-normal-state-map "A" 'evil-append-line)
(define-key evil-normal-state-map "o" 'evil-open-below)
(define-key evil-normal-state-map "O" 'evil-open-above)
(define-key evil-normal-state-map "gi" 'evil-insert-resume)
(define-key evil-normal-state-map "gI" 'evil-insert-0-line)

(define-key evil-normal-state-map "c" 'evil-change)
(define-key evil-normal-state-map "C" 'evil-change-line)
(define-key evil-normal-state-map "d" 'evil-delete)
(define-key evil-normal-state-map "D" 'evil-delete-line)
(define-key evil-normal-state-map "p" 'evil-paste-after)
(define-key evil-normal-state-map "P" 'evil-paste-before)
(define-key evil-normal-state-map "\C-n" 'evil-paste-pop-next)
(define-key evil-normal-state-map "\C-p" 'evil-paste-pop)

(define-key evil-normal-state-map "=" 'evil-indent)
(define-key evil-normal-state-map "<" 'evil-shift-left)
(define-key evil-normal-state-map ">" 'evil-shift-right)
(define-key evil-normal-state-map "~" 'evil-invert-char)
(define-key evil-normal-state-map "g~" 'evil-invert-case)
(define-key evil-normal-state-map "gu" 'evil-downcase)
(define-key evil-normal-state-map "gU" 'evil-upcase)

(define-key evil-normal-state-map "J" 'evil-join)
(define-key evil-normal-state-map "s" 'evil-substitute)
(define-key evil-normal-state-map "S" 'evil-change-whole-line)
(define-key evil-normal-state-map "x" 'evil-delete-char)
(define-key evil-normal-state-map "X" 'evil-delete-backward-char)
(define-key evil-normal-state-map "r" 'evil-replace)
(define-key evil-normal-state-map "R" 'evil-replace-state)

(define-key evil-normal-state-map "u" 'evil-undo)
(define-key evil-normal-state-map "\C-r" 'evil-redo)
(define-key evil-normal-state-map "." 'evil-repeat)
(define-key evil-normal-state-map "q" 'evil-record-macro)
(define-key evil-normal-state-map "@" 'evil-execute-macro)
(define-key evil-normal-state-map "\"" 'evil-use-register)

(define-key evil-normal-state-map "ZZ" 'evil-save-modified-and-close)
(define-key evil-normal-state-map "ZQ" 'evil-quit)
(define-key evil-normal-state-map (kbd "DEL") 'evil-backward-char)
(define-key evil-normal-state-map [escape] 'evil-force-normal-state)

;; window commands
(define-prefix-command 'evil-window-map)
(define-key evil-window-map "b" 'evil-window-bottom-right)
(define-key evil-window-map "c" 'evil-window-delete)
(define-key evil-window-map "h" 'evil-window-left)
(define-key evil-window-map "H" 'evil-window-move-far-left)
(define-key evil-window-map "j" 'evil-window-down)
(define-key evil-window-map "J" 'evil-window-move-very-bottom)
(define-key evil-window-map "k" 'evil-window-up)
(define-key evil-window-map "K" 'evil-window-move-very-top)
(define-key evil-window-map "l" 'evil-window-right)
(define-key evil-window-map "L" 'evil-window-move-far-right)
(define-key evil-window-map "n" 'evil-window-new)
(define-key evil-window-map "o" 'delete-other-windows)
(define-key evil-window-map "p" 'evil-window-mru)
(define-key evil-window-map "r" 'evil-window-rotate-downwards)
(define-key evil-window-map "R" 'evil-window-rotate-upwards)
(define-key evil-window-map "s" 'evil-window-split)
(define-key evil-window-map "t" 'evil-window-top-left)
(define-key evil-window-map "v" 'evil-window-vsplit)
(define-key evil-window-map "w" 'evil-window-next)
(define-key evil-window-map "W" 'evil-window-prev)
(define-key evil-window-map "=" 'balance-windows)
(define-key evil-window-map "f" 'ffap-other-window)

;;; Motion state

(evil-redirect-digit-argument evil-motion-state-map "0" 'evil-beginning-of-line)
(define-key evil-motion-state-map "1" 'digit-argument)
(define-key evil-motion-state-map "2" 'digit-argument)
(define-key evil-motion-state-map "3" 'digit-argument)
(define-key evil-motion-state-map "4" 'digit-argument)
(define-key evil-motion-state-map "5" 'digit-argument)
(define-key evil-motion-state-map "6" 'digit-argument)
(define-key evil-motion-state-map "7" 'digit-argument)
(define-key evil-motion-state-map "8" 'digit-argument)
(define-key evil-motion-state-map "9" 'digit-argument)

(define-key evil-motion-state-map "v" 'evil-visual-char)
(define-key evil-motion-state-map "V" 'evil-visual-line)
(define-key evil-motion-state-map "\C-v" 'evil-visual-block)
(define-key evil-motion-state-map "gv" 'evil-visual-restore)

(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "j" 'evil-next-line)
(define-key evil-motion-state-map "k" 'evil-previous-line)
(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-motion-state-map "w" 'evil-forward-word-begin)
(define-key evil-motion-state-map "W" 'evil-forward-WORD-begin)
(define-key evil-motion-state-map "b" 'evil-backward-word-begin)
(define-key evil-motion-state-map "B" 'evil-backward-WORD-begin)
(define-key evil-motion-state-map "e" 'evil-forward-word-end)
(define-key evil-motion-state-map "E" 'evil-forward-WORD-end)
(define-key evil-motion-state-map "ge" 'evil-backward-word-end)
(define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)
(define-key evil-motion-state-map "gg" 'evil-goto-first-line)
(define-key evil-motion-state-map "G" 'evil-goto-line)
(define-key evil-motion-state-map "H" 'evil-window-top)
(define-key evil-motion-state-map "L" 'evil-window-bottom)
(define-key evil-motion-state-map "M" 'evil-window-middle)
(define-key evil-motion-state-map "%" 'evil-jump-item)
(define-key evil-motion-state-map "^" 'evil-first-non-blank)
(define-key evil-motion-state-map "$" 'evil-end-of-line)
(define-key evil-motion-state-map "{" 'evil-backward-paragraph)
(define-key evil-motion-state-map "}" 'evil-forward-paragraph)

(define-key evil-motion-state-map "f" 'evil-find-char)
(define-key evil-motion-state-map "F" 'evil-find-char-backward)
(define-key evil-motion-state-map "t" 'evil-find-char-to)
(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)
(define-key evil-motion-state-map ";" 'evil-repeat-find-char)
(define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map "/" 'evil-search-forward)
(define-key evil-motion-state-map "?" 'evil-search-backward)
(define-key evil-motion-state-map "*" 'evil-search-word-forward)
(define-key evil-motion-state-map "#" 'evil-search-word-backward)
(define-key evil-motion-state-map "n" 'evil-search-next)
(define-key evil-motion-state-map "N" 'evil-search-previous)
(define-key evil-motion-state-map "m" 'evil-set-marker)
(define-key evil-motion-state-map "`" 'evil-goto-mark)
(define-key evil-motion-state-map "'" 'evil-goto-mark-line)

(define-key evil-motion-state-map "y" 'evil-yank)
(define-key evil-motion-state-map "Y" 'evil-yank-line)

(define-key evil-motion-state-map "zt" 'evil-scroll-line-to-top)
(define-key evil-motion-state-map "zz" 'evil-scroll-line-to-center)
(define-key evil-motion-state-map "zb" 'evil-scroll-line-to-bottom)
(define-key evil-motion-state-map "zl" 'evil-scroll-column-right)
(define-key evil-motion-state-map "zh" 'evil-scroll-column-left)

(define-key evil-motion-state-map "gt" 'tab-bar-switch-to-next-tab)
(define-key evil-motion-state-map "gT" 'tab-bar-switch-to-prev-tab)

(define-key evil-motion-state-map "\C-w" 'evil-window-map)
(define-key evil-motion-state-map "\C-y" 'evil-scroll-line-up)
(define-key evil-motion-state-map "\C-e" 'evil-scroll-line-down)
(define-key evil-motion-state-map "\C-u" 'evil-scroll-up)
(define-key evil-motion-state-map "\C-d" 'evil-scroll-down)
(define-key evil-motion-state-map "\C-b" 'evil-scroll-page-up)
(define-key evil-motion-state-map "\C-f" 'evil-scroll-page-down)
(define-key evil-motion-state-map "\C-o" 'evil-jump-backward)
(define-key evil-motion-state-map "\C-i" 'evil-jump-forward)

(define-key evil-motion-state-map "\\" 'evil-execute-in-emacs-state)
(define-key evil-motion-state-map (kbd "RET") 'evil-ret)
(define-key evil-motion-state-map "\C-z" 'evil-emacs-state)

;; text objects
(define-key evil-outer-text-objects-map "w" 'evil-a-word)
(define-key evil-outer-text-objects-map "W" 'evil-a-WORD)
(define-key evil-outer-text-objects-map "s" 'evil-a-sentence)
(define-key evil-outer-text-objects-map "p" 'evil-a-paragraph)
(define-key evil-outer-text-objects-map "b" 'evil-a-paren)
(define-key evil-outer-text-objects-map "(" 'evil-a-paren)
(define-key evil-outer-text-objects-map ")" 'evil-a-paren)
(define-key evil-outer-text-objects-map "[" 'evil-a-bracket)
(define-key evil-outer-text-objects-map "]" 'evil-a-bracket)
(define-key evil-outer-text-objects-map "B" 'evil-a-curly)
(define-key evil-outer-text-objects-map "{" 'evil-a-curly)
(define-key evil-outer-text-objects-map "}" 'evil-a-curly)
(define-key evil-outer-text-objects-map "<" 'evil-an-angle)
(define-key evil-outer-text-objects-map ">" 'evil-an-angle)
(define-key evil-outer-text-objects-map "'" 'evil-a-single-quote)
(define-key evil-outer-text-objects-map "\"" 'evil-a-double-quote)
(define-key evil-outer-text-objects-map "`" 'evil-a-back-quote)
(define-key evil-outer-text-objects-map "t" 'evil-a-tag)
(define-key evil-outer-text-objects-map "o" 'evil-a-symbol)
(define-key evil-inner-text-objects-map "w" 'evil-inner-word)
(define-key evil-inner-text-objects-map "W" 'evil-inner-WORD)
(define-key evil-inner-text-objects-map "s" 'evil-inner-sentence)
(define-key evil-inner-text-objects-map "p" 'evil-inner-paragraph)
(define-key evil-inner-text-objects-map "b" 'evil-inner-paren)
(define-key evil-inner-text-objects-map "(" 'evil-inner-paren)
(define-key evil-inner-text-objects-map ")" 'evil-inner-paren)
(define-key evil-inner-text-objects-map "[" 'evil-inner-bracket)
(define-key evil-inner-text-objects-map "]" 'evil-inner-bracket)
(define-key evil-inner-text-objects-map "B" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "{" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "}" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "<" 'evil-inner-angle)
(define-key evil-inner-text-objects-map ">" 'evil-inner-angle)
(define-key evil-inner-text-objects-map "'" 'evil-inner-single-quote)
(define-key evil-inner-text-objects-map "\"" 'evil-inner-double-quote)
(define-key evil-inner-text-objects-map "`" 'evil-inner-back-quote)
(define-key evil-inner-text-objects-map "t" 'evil-inner-tag)
(define-key evil-inner-text-objects-map "o" 'evil-inner-symbol)

;;; Visual state

(define-key evil-visual-state-map "A" 'evil-append)
(define-key evil-visual-state-map "I" 'evil-insert)
(define-key evil-visual-state-map "o" 'exchange-point-and-mark)
(define-key evil-visual-state-map "O" 'evil-visual-exchange-corners)
(define-key evil-visual-state-map "R" 'evil-change-whole-line)
(define-key evil-visual-state-map "u" 'evil-downcase)
(define-key evil-visual-state-map "U" 'evil-upcase)
(define-key evil-visual-state-map "a" evil-outer-text-objects-map)
(define-key evil-visual-state-map "i" evil-inner-text-objects-map)
(define-key evil-visual-state-map [remap evil-repeat] 'undefined)
(define-key evil-visual-state-map [escape] 'evil-exit-visual-state)

;;; Operator-Pending state

(define-key evil-operator-state-map "a" evil-outer-text-objects-map)
(define-key evil-operator-state-map "i" evil-inner-text-objects-map)

;;; Insert state

(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-insert-state-map "\C-r" 'evil-paste-from-register)
(define-key evil-insert-state-map "\C-z" 'evil-emacs-state)

;;; Replace state

(define-key evil-replace-state-map (kbd "DEL") 'evil-replace-backspace)
(define-key evil-replace-state-map [escape] 'evil-normal-state)

;;; Emacs state

(define-key evil-emacs-state-map "\C-z" 'evil-exit-emacs-state)

;; evil-read-key
(define-key evil-read-key-map (kbd "ESC") #'keyboard-quit)
(define-key evil-read-key-map (kbd "C-g") #'keyboard-quit)
(define-key evil-read-key-map (kbd "C-q") #'evil-read-quoted-char)

(provide 'evil-maps)

;;; evil-maps.el ends here
