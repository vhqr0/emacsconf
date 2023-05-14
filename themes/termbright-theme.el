;;; termbright-theme.el --- a more usable theme for white-on-black terminals

;;; Commentary:

;; Modified version of [https://github.com/bmastenbrook/termbright-theme-el].

;;; Code:

(deftheme termbright
  "termbright, a more usable theme for white-on-black terminals")

(defvar termbright-bgblue0  '((t (:foreground "white"       :background "blue"                                 ))))
(defvar termbright-bgblue1  '((t (:foreground "brightwhite" :background "blue"        :weight bold             ))))
(defvar termbright-bgblue2  '((t (:foreground "brightwhite" :background "blue"        :weight bold :underline t))))
(defvar termbright-bgblack0 '((t (:foreground "white"       :background "brightblack"                          ))))
(defvar termbright-bgblack1 '((t (:foreground "brightwhite" :background "brightblack" :weight bold             ))))
(defvar termbright-bgblack2 '((t (:foreground "brightwhite" :background "brightblack" :weight bold :underline t))))

(defvar termbright-shadow0  '((t (:foreground "green"                                  ))))
(defvar termbright-shadow1  '((t (:foreground "green"                      :underline t))))
(defvar termbright-red0     '((t (:foreground "brightred"     :weight bold             ))))
(defvar termbright-red1     '((t (:foreground "brightred"     :weight bold :underline t))))
(defvar termbright-yellow0  '((t (:foreground "brightyellow"  :weight bold             ))))
(defvar termbright-yellow1  '((t (:foreground "brightyellow"  :weight bold :underline t))))
(defvar termbright-magenta0 '((t (:foreground "brightmagenta" :weight bold             ))))
(defvar termbright-magenta1 '((t (:foreground "brightmagenta" :weight bold :underline t))))
(defvar termbright-cyan0    '((t (:foreground "brightcyan"    :weight bold             ))))
(defvar termbright-cyan1    '((t (:foreground "brightcyan"    :weight bold :underline t))))
(defvar termbright-green0   '((t (:foreground "brightgreen"   :weight bold             ))))
(defvar termbright-green1   '((t (:foreground "brightgreen"   :weight bold :underline t))))
(defvar termbright-white0   '((t (:foreground "brightwhite"   :weight bold             ))))
(defvar termbright-white1   '((t (:foreground "brightwhite"   :weight bold :underline t))))

(custom-theme-set-faces
 'termbright

 `(region              ,termbright-bgblue0)
 `(highlight           ,termbright-bgblack2)
 `(isearch             ,termbright-bgblack2)
 `(secondary-selection ,termbright-bgblack0)
 `(match               ,termbright-bgblack0)
 `(lazy-highlight      ,termbright-bgblack0)
 `(query-replace       ,termbright-bgblack0)

 `(shadow              ,termbright-shadow0)
 `(minibuffer-prompt   ,termbright-white0)
 `(isearch-fail        ,termbright-red0)
 `(trailing-whitespace ,termbright-red1)
 `(button              ,termbright-green1)
 `(link                ,termbright-green1)
 `(link-visited        ,termbright-shadow1)

 `(font-lock-warning-face              ,termbright-red1)
 `(font-lock-preprocessor-face         ,termbright-green0)
 `(font-lock-doc-face                  ,termbright-shadow0)
 `(font-lock-doc-markup-face           ,termbright-green0)
 `(font-lock-comment-face              ,termbright-shadow0)
 `(font-lock-comment-delimiter-face    ,termbright-green0)
 `(font-lock-string-face               ,termbright-shadow0)
 `(font-lock-escape-face               ,termbright-green0)
 `(font-lock-regexp-grouping-backslash ,termbright-green0)
 `(font-lock-regexp-grouping-construct ,termbright-green0)
 `(font-lock-negation-char-face        ,termbright-green0)
 `(font-lock-constant-face             ,termbright-green0)
 `(font-lock-number-face               ,termbright-green0)
 `(font-lock-function-name-face        ,termbright-yellow1)
 `(font-lock-variable-name-face        ,termbright-yellow0)
 `(font-lock-property-name-face        ,termbright-yellow0)
 `(font-lock-type-face                 ,termbright-magenta0)
 `(font-lock-keyword-face              ,termbright-cyan0)
 `(font-lock-builtin-face              ,termbright-cyan0)

 `(header-line              ,termbright-bgblue2)
 `(mode-line                ,termbright-bgblue1)
 `(mode-line-inactive       ,termbright-bgblue0)
 `(mode-line-buffer-id      ,termbright-white0)
 `(mode-line-emphasis       ,termbright-white1)
 `(mode-line-highlight      ,termbright-white1)
 `(line-number              ,termbright-shadow0)
 `(line-number-current-line ,termbright-green0)

 ;; company
 `(company-tooltip           ,termbright-bgblack0)
 `(company-tooltip-selection ,termbright-bgblack1)

 ;; magit
 `(magit-section-highlight      ,termbright-bgblack0)
 `(magit-diff-context-highlight ,termbright-bgblack0)
 )

(provide-theme 'termbright)

;;; termbright-theme.el ends here
