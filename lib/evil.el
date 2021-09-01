;;; evil.el --- extensible vi layer

;; The following list of authors was kept up to date until the beginning of
;; 2017, when evil moved under new maintainers. For authors since then, please
;; consult the git logs.

;; Maintainers: The emacs-evil team. <https://github.com/orgs/emacs-evil/people>
;;      To get in touch, please use the bug tracker or the
;;      mailing list (see below).
;; Created: 2011-03-01
;; Version: 1.14.0
;; Keywords: emulation, vim
;; URL: https://github.com/emacs-evil/evil
;;      Repository: https://github.com/emacs-evil/evil.git
;;      EmacsWiki: http://www.emacswiki.org/emacs/Evil
;; Bug tracker: https://github.com/emacs-evil/evil/issues
;;      If you have bug reports, suggestions or patches, please
;;      create an issue at the bug tracker (open for everyone).
;;      Other discussions (tips, extensions) go to the mailing list.
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;;      You don't have to subscribe to post; we usually reply
;;      within a few days and CC our replies back to you.
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

;; Evil is an extensible vi layer for Emacs. It emulates the main
;; features of Vim, and provides facilities for writing custom
;; extensions.
;;
;; Evil lives in a Git repository. To obtain Evil, do
;;
;;      git clone git://github.com/emacs-evil/evil.git
;;
;; Move Evil to ~/.emacs.d/evil (or somewhere else in the `load-path').
;; Then add the following lines to ~/.emacs:
;;
;;      (add-to-list 'load-path "~/.emacs.d/evil")
;;      (require 'evil)
;;      (evil-mode 1)
;;
;; Evil requires undo-redo (Emacs 28), undo-fu or undo-tree for redo
;; functionality.  Otherwise, Evil uses regular Emacs undo.
;;
;;     https://gitlab.com/ideasman42/emacs-undo-fu
;;     https://melpa.org/#/undo-fu
;;     https://gitlab.com/tsc25/undo-tree
;;     https://elpa.gnu.org/packages/undo-tree.html
;;
;;; Code:

(require 'evil-vars)
(require 'evil-common)
(require 'evil-core)
(require 'evil-states)
(require 'evil-repeat)
(require 'evil-macros)
(require 'evil-search)
(require 'evil-ex)
(require 'evil-types)
(require 'evil-commands)
(require 'evil-jumps)
(require 'evil-maps)
(require 'evil-integration)

(run-hooks 'evil-after-load-hook)

(provide 'evil)

;;; evil.el ends here
