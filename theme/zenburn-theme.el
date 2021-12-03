;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2020 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.8.0-snapshot

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme zenburn "The Zenburn color theme.")

(defgroup zenburn-theme nil
  "Zenburn theme."
  :group 'faces
  :prefix "zenburn-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")
  :tag "Zenburn theme")

;;;###autoload
(defcustom zenburn-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenburn-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar zenburn-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar zenburn-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar zenburn-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom zenburn-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

;;; Color Palette

(defvar zenburn-default-colors-alist
  '(("zenburn-fg-1"     . "#656555")
    ("zenburn-fg-05"    . "#989890")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg+2"     . "#FFFFFD")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#2B2B2B")
    ("zenburn-bg-08"    . "#303030")
    ("zenburn-bg-05"    . "#383838")
    ("zenburn-bg"       . "#3F3F3F")
    ("zenburn-bg+05"    . "#494949")
    ("zenburn-bg+1"     . "#4F4F4F")
    ("zenburn-bg+2"     . "#5F5F5F")
    ("zenburn-bg+3"     . "#6F6F6F")
    ("zenburn-red-6"    . "#6C3333")
    ("zenburn-red-5"    . "#7C4343")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red+2"    . "#ECB3B3")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-green-5"  . "#2F4F2F")
    ("zenburn-green-4"  . "#3F5F3F")
    ("zenburn-green-3"  . "#4F6F4F")
    ("zenburn-green-2"  . "#5F7F5F")
    ("zenburn-green-1"  . "#6F8F6F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+3"   . "#BDE0F3")
    ("zenburn-blue+2"   . "#ACE0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3"))
  "List of Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append zenburn-default-colors-alist
                           zenburn-override-colors-alist))
         (z-variable-pitch (if zenburn-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,zenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,zenburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(cursor ((t (:foreground ,zenburn-fg :background ,zenburn-fg+1))))
   `(widget-field ((t (:foreground ,zenburn-fg :background ,zenburn-bg+3))))
   `(escape-glyph ((t (:foreground ,zenburn-yellow :weight bold))))
   `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(header-line ((t (:foreground ,zenburn-yellow
                                  :background ,zenburn-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((t (:background ,zenburn-bg-05))))
   `(success ((t (:foreground ,zenburn-green :weight bold))))
   `(warning ((t (:foreground ,zenburn-orange :weight bold))))
   `(tooltip ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,zenburn-green))))
   `(compilation-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,zenburn-blue))))
   `(compilation-info ((t (:foreground ,zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,zenburn-green))))
   `(compilation-line-face ((t (:foreground ,zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,zenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,zenburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,zenburn-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,zenburn-fg-1))))
   `(completions-common-part ((t (:foreground ,zenburn-blue))))
   `(completions-first-difference ((t (:foreground ,zenburn-fg+1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,zenburn-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,zenburn-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,zenburn-green+4))))
;;;;; display-fill-column-indicator
   `(fill-column-indicator ((,class :foreground ,zenburn-bg-05 :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,zenburn-fg))))
   `(grep-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,zenburn-blue))))
   `(grep-match-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(match ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,zenburn-cyan    :foreground ,zenburn-bg-1))))
   `(hi-green   ((t (:background ,zenburn-green+4 :foreground ,zenburn-bg-1))))
   `(hi-pink    ((t (:background ,zenburn-magenta :foreground ,zenburn-bg-1))))
   `(hi-yellow  ((t (:background ,zenburn-yellow  :foreground ,zenburn-bg-1))))
   `(hi-blue-b  ((t (:foreground ,zenburn-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,zenburn-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,zenburn-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg+2))))
   `(isearch-fail ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05))))

   `(menu ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,zenburn-green+1
                           :background ,zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,zenburn-green-2
                      :background ,zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,zenburn-bg-1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,zenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,zenburn-red))))
   `(vertical-border ((t (:foreground ,zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,zenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-green-2))))
   `(font-lock-constant-face ((t (:foreground ,zenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,zenburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,zenburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,zenburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,zenburn-red))))
   `(font-lock-type-face ((t (:foreground ,zenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,zenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,zenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,zenburn-bg+3 :background ,zenburn-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,zenburn-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,zenburn-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,zenburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,zenburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,zenburn-orange))))
   `(font-latex-script-char-face ((t (:foreground ,zenburn-orange))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,zenburn-orange :background ,zenburn-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   `(company-tooltip-mouse ((t (:background ,zenburn-bg-1))))
   `(company-tooltip-common ((t (:foreground ,zenburn-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,zenburn-green+2))))
   `(company-scrollbar-fg ((t (:background ,zenburn-bg-1))))
   `(company-scrollbar-bg ((t (:background ,zenburn-bg+2))))
   `(company-preview ((t (:background ,zenburn-green+2))))
   `(company-preview-common ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,zenburn-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,zenburn-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,zenburn-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,zenburn-green+4))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,zenburn-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,zenburn-red))))
   `(diff-header ((,class (:background ,zenburn-bg+2))
                  (t (:background ,zenburn-fg :foreground ,zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))
      (t (:background ,zenburn-fg :foreground ,zenburn-bg :weight bold))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange) :inherit unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red) :inherit unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,zenburn-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,zenburn-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,zenburn-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,zenburn-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,zenburn-yellow  :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,zenburn-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,zenburn-bg+05))))
   `(magit-section-heading             ((t (:foreground ,zenburn-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,zenburn-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,zenburn-bg+05 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,zenburn-bg+05 :weight bold
                                                        :foreground ,zenburn-orange))))
   `(magit-diff-added                  ((t (:background ,zenburn-green-2))))
   `(magit-diff-added-highlight        ((t (:background ,zenburn-green))))
   `(magit-diff-removed                ((t (:background ,zenburn-red-4))))
   `(magit-diff-removed-highlight      ((t (:background ,zenburn-red-3))))
   `(magit-diff-hunk-heading           ((t (:background ,zenburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,zenburn-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,zenburn-bg+2
                                                        :foreground ,zenburn-orange))))
   `(magit-diff-lines-heading          ((t (:background ,zenburn-orange
                                                        :foreground ,zenburn-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,zenburn-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added              ((t (:foreground ,zenburn-green+4))))
   `(magit-diffstat-removed            ((t (:foreground ,zenburn-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,zenburn-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,zenburn-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,zenburn-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,zenburn-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,zenburn-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,zenburn-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,zenburn-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,zenburn-orange))))
   `(magit-log-date      ((t (:foreground ,zenburn-fg-1))))
   `(magit-log-graph     ((t (:foreground ,zenburn-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,zenburn-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,zenburn-green))))
   `(magit-sequence-part ((t (:foreground ,zenburn-yellow))))
   `(magit-sequence-head ((t (:foreground ,zenburn-blue))))
   `(magit-sequence-drop ((t (:foreground ,zenburn-red))))
   `(magit-sequence-done ((t (:foreground ,zenburn-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,zenburn-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,zenburn-green))))
   `(magit-bisect-skip ((t (:foreground ,zenburn-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,zenburn-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
   `(magit-blame-hash    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
   `(magit-blame-name    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(magit-blame-date    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(magit-blame-summary ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,zenburn-bg+3))))
   `(magit-hash           ((t (:foreground ,zenburn-bg+3))))
   `(magit-tag            ((t (:foreground ,zenburn-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,zenburn-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,zenburn-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,zenburn-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,zenburn-blue   :weight bold))))
   `(magit-refname        ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,zenburn-green))))
   `(magit-signature-bad       ((t (:foreground ,zenburn-red))))
   `(magit-signature-untrusted ((t (:foreground ,zenburn-yellow))))
   `(magit-signature-expired   ((t (:foreground ,zenburn-orange))))
   `(magit-signature-revoked   ((t (:foreground ,zenburn-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,zenburn-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,zenburn-green))))
   `(magit-reflog-amend        ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,zenburn-green))))
   `(magit-reflog-checkout     ((t (:foreground ,zenburn-blue))))
   `(magit-reflog-reset        ((t (:foreground ,zenburn-red))))
   `(magit-reflog-rebase       ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,zenburn-green))))
   `(magit-reflog-remote       ((t (:foreground ,zenburn-cyan))))
   `(magit-reflog-other        ((t (:foreground ,zenburn-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,zenburn-green+1))))
   `(message-header-other ((t (:foreground ,zenburn-green))))
   `(message-header-to ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,zenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,zenburn-green))))
   `(message-mml ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,zenburn-fg :background ,zenburn-bg+3 :weight bold))))
;;;;; term
   `(term-color-black ((t (:foreground ,zenburn-bg
                                       :background ,zenburn-bg-1))))
   `(term-color-red ((t (:foreground ,zenburn-red-2
                                     :background ,zenburn-red-4))))
   `(term-color-green ((t (:foreground ,zenburn-green
                                       :background ,zenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,zenburn-orange
                                        :background ,zenburn-yellow))))
   `(term-color-blue ((t (:foreground ,zenburn-blue-1
                                      :background ,zenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,zenburn-magenta
                                         :background ,zenburn-red))))
   `(term-color-cyan ((t (:foreground ,zenburn-cyan
                                      :background ,zenburn-blue))))
   `(term-color-white ((t (:foreground ,zenburn-fg
                                       :background ,zenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,zenburn-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,zenburn-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,zenburn-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,zenburn-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,zenburn-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,zenburn-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,zenburn-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,zenburn-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,zenburn-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,zenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,zenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,zenburn-red))))
   `(whitespace-line ((t (:background ,zenburn-bg :foreground ,zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,zenburn-orange :foreground ,zenburn-orange))))
   `(whitespace-indentation ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))
   `(whitespace-empty ((t (:background ,zenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))
   ))

;;; Theme Variables
(zenburn-with-color-variables
  (custom-theme-set-variables
   'zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,zenburn-bg ,zenburn-red ,zenburn-green ,zenburn-yellow
                                          ,zenburn-blue ,zenburn-magenta ,zenburn-cyan ,zenburn-fg])
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,zenburn-red-1)
       ( 40. . ,zenburn-red)
       ( 60. . ,zenburn-orange)
       ( 80. . ,zenburn-yellow-2)
       (100. . ,zenburn-yellow-1)
       (120. . ,zenburn-yellow)
       (140. . ,zenburn-green-2)
       (160. . ,zenburn-green)
       (180. . ,zenburn-green+1)
       (200. . ,zenburn-green+2)
       (220. . ,zenburn-green+3)
       (240. . ,zenburn-green+4)
       (260. . ,zenburn-cyan)
       (280. . ,zenburn-blue-2)
       (300. . ,zenburn-blue-1)
       (320. . ,zenburn-blue)
       (340. . ,zenburn-blue+1)
       (360. . ,zenburn-magenta)))
   `(vc-annotate-very-old-color ,zenburn-magenta)
   `(vc-annotate-background ,zenburn-bg-1)
   ))

;;; Footer

(provide-theme 'zenburn)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; zenburn-theme.el ends here
