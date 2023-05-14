;;; termbright-theme.el --- a more usable theme for white-on-black terminals

;;; Commentary:

;; Modified version of [https://github.com/bmastenbrook/termbright-theme-el].

;;; Code:

(deftheme termbright
  "termbright, a more usable theme for white-on-black terminals")

(defvar termbright-region    '((t (:foreground "white"     :background "blue"     ))))
(defvar termbright-highlight '((t (:foreground "black"     :background "lightgrey"))))
(defvar termbright-selection '((t (:foreground "black"     :background "dimgrey"  ))))

(defvar termbright-shadow   '((t (:foreground "darkgreen"                        ))))
(defvar termbright-shadow1  '((t (:foreground "darkgreen"            :underline t))))
(defvar termbright-red      '((t (:foreground "red"     :weight bold             ))))
(defvar termbright-red1     '((t (:foreground "red"     :weight bold :underline t))))
(defvar termbright-yellow   '((t (:foreground "yellow"  :weight bold             ))))
(defvar termbright-yellow1  '((t (:foreground "yellow"  :weight bold :underline t))))
(defvar termbright-magenta  '((t (:foreground "magenta" :weight bold             ))))
(defvar termbright-magenta1 '((t (:foreground "magenta" :weight bold :underline t))))
(defvar termbright-cyan     '((t (:foreground "cyan"    :weight bold             ))))
(defvar termbright-cyan1    '((t (:foreground "cyan"    :weight bold :underline t))))
(defvar termbright-green    '((t (:foreground "green"   :weight bold             ))))
(defvar termbright-green1   '((t (:foreground "green"   :weight bold :underline t))))
(defvar termbright-white    '((t (:foreground "white"   :weight bold             ))))
(defvar termbright-white1   '((t (:foreground "white"   :weight bold :underline t))))

(defvar termbright-headerline        '((t (:foreground "white" :background "blue" :weight bold :underline t))))
(defvar termbright-modeline          '((t (:foreground "white" :background "blue" :weight bold             ))))
(defvar termbright-modeline-inactive '((t (:foreground "white" :background "blue"                          ))))

(custom-theme-set-faces
 'termbright

 `(region              ,termbright-region)
 `(highlight           ,termbright-highlight)
 `(secondary-selection ,termbright-selection)
 `(shadow              ,termbright-shadow)
 `(match               ,termbright-selection)
 `(lazy-highlight      ,termbright-selection)
 `(minibuffer-prompt   ,termbright-white)
 `(isearch             ,termbright-highlight)
 `(isearch-fail        ,termbright-red)
 `(query-replace       ,termbright-selection)
 `(trailing-whitespace ,termbright-red1)
 `(button              ,termbright-green1)
 `(link                ,termbright-green1)
 `(link-visited        ,termbright-shadow1)

 `(font-lock-warning-face              ,termbright-red1)
 `(font-lock-preprocessor-face         ,termbright-green)
 `(font-lock-doc-face                  ,termbright-shadow)
 `(font-lock-doc-markup-face           ,termbright-green)
 `(font-lock-comment-face              ,termbright-shadow)
 `(font-lock-comment-delimiter-face    ,termbright-green)
 `(font-lock-string-face               ,termbright-shadow)
 `(font-lock-escape-face               ,termbright-green)
 `(font-lock-regexp-grouping-backslash ,termbright-green)
 `(font-lock-regexp-grouping-construct ,termbright-green)
 `(font-lock-negation-char-face        ,termbright-green)
 `(font-lock-constant-face             ,termbright-green)
 `(font-lock-number-face               ,termbright-green)
 `(font-lock-function-name-face        ,termbright-yellow1)
 `(font-lock-variable-name-face        ,termbright-yellow)
 `(font-lock-property-name-face        ,termbright-yellow)
 `(font-lock-type-face                 ,termbright-magenta)
 `(font-lock-keyword-face              ,termbright-cyan)
 `(font-lock-builtin-face              ,termbright-cyan)

 `(line-number              ,termbright-shadow)
 `(line-number-current-line ,termbright-green)
 `(header-line              ,termbright-headerline)
 `(mode-line                ,termbright-modeline)
 `(mode-line-inactive       ,termbright-modeline-inactive)
 `(mode-line-buffer-id      ,termbright-white)
 `(mode-line-emphasis       ,termbright-white1)
 `(mode-line-highlight      ,termbright-white1)
 )

(provide-theme 'termbright)

;;; termbright-theme.el ends here
