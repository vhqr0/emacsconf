;;; evil-integration.el --- Integrate Evil with other modules -*- lexical-binding: t -*-

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

;;; Commentary:

;; This provides evil integration for various emacs modes.
;; Additional keybindings (or default state) should go into evil-keybindings.el.

;;; Code:

(require 'evil-common)
(require 'evil-repeat)

(mapc #'evil-declare-ignore-repeat
      '(undefined
        company-abort
        company-select-next
        company-select-previous
        company-select-next-or-abort
        company-select-previous-or-abort
        company-show-doc-buffer
        company-show-location
        company-search-candidates
        company-filter-candidates))

(mapc #'evil-declare-change-repeat
      '(quoted-insert
        dabbrev-expand
        company-complete-number
        company-complete-selection
        company-complete-common))

(mapc #'evil-declare-abort-repeat
      '(execute-extended-command
        eval-expression
        save-buffer
        undo))

(dolist (cmd '(keyboard-quit keyboard-escape-quit))
  (evil-set-command-property cmd :suppress-operator t))

;; Calling `keyboard-quit' should cancel repeat
(defadvice keyboard-quit (before evil activate)
  (when (fboundp 'evil-repeat-abort)
    (evil-repeat-abort)))

(provide 'evil-integration)

;;; evil-integration.el ends here
