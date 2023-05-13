;;; termbright-theme.el --- a more usable theme for white-on-black terminals

;;; Commentary:

;; Modified version of [https://github.com/bmastenbrook/termbright-theme-el].

;;; Code:

(deftheme termbright
  "termbright, a more usable theme for white-on-black terminals")

(custom-theme-set-faces
 'termbright

 '(highlight ((t (:foreground "black" :background "green"))))
 '(shadow ((t (:foreground "green"))))
 '(region ((t (:foreground "white" :background "blue"))))
 '(secondary-selection ((t (:foreground "black" :background "cyan"))))

 '(font-lock-builtin-face ((t (:weight bold :foreground "magenta"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "green" :weight bold))))
 '(font-lock-comment-face ((t (:weight bold :foreground "green"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "magenta"))))
 '(font-lock-doc-face ((t (:foreground "green"))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "yellow"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "cyan"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "magenta" :weight bold))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "yellow" :weight bold))))
 '(font-lock-variable-name-face ((t (:weight bold :foreground "red"))))
 '(font-lock-warning-face ((t (:foreground "red" :underline t :weight bold))))

 '(button ((t (:underline (:color foreground-color :style line)))))
 '(link ((t (:foreground "cyan" :underline t :weight bold))))
 '(link-visited ((t (:foreground "magenta4" :underline (:color foreground-color :style line)))))
 '(tooltip ((t (:foreground "black" :background "lightyellow"))))

 '(fringe ((t (:background "grey95"))))
 '(line-number ((t (:foreground "green"))))
 '(line-number-current-line ((t (:foreground "red" :weight bold))))
 '(trailing-whitespace ((t (:background "red1"))))

 '(header-line ((t (:underline (:color foreground-color :style line) :inverse-video nil :background "blue" :foreground "white" :weight bold))))
 '(mode-line ((t (:weight bold :foreground "white" :background "blue"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:background "green" :foreground "black"))))
 '(mode-line-inactive ((t (:background "blue" :foreground "white" :weight bold))))

 '(minibuffer-prompt ((t (:foreground "white" :weight bold))))
 '(match ((t (:foreground "black" :background "yellow"))))
 '(lazy-highlight ((t (:background "turquoise3"))))
 '(isearch ((t (:foreground "cyan1" :background "magenta4"))))
 '(isearch-fail ((t (:background "red"))))
 '(query-replace ((t (:background "magenta4" :foreground "cyan1"))))
 '(next-error ((t (:background "blue" :foreground "white"))))
 )

(provide-theme 'termbright)

;;; termbright-theme.el ends here
