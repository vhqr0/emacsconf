;;; termbright-theme.el --- a more usable theme for white-on-black terminals

;;; Commentary:

;; Modified version of [https://github.com/bmastenbrook/termbright-theme-el].

;;; Code:

(deftheme termbright
  "termbright, a more usable theme for white-on-black terminals")

(dolist (color '("red" "green" "yellow" "blue" "magenta" "cyan"))
  (let* ((bgsym0 (intern (format "termbright-bg%s0" color)))
         (bgsym1 (intern (format "termbright-bg%s1" color)))
         (bgsym2 (intern (format "termbright-bg%s2" color)))
         (fgsym0 (intern (format "termbright-%s0"   color)))
         (fgsym1 (intern (format "termbright-%s1"   color)))
         (fgsym2 (intern (format "termbright-%s2"   color)))
         (fgsym3 (intern (format "termbright-%s3"   color))))
    (eval
     `(progn
        (defvar ,bgsym0 '((t (:foreground "white"                  :background ,color                          ))))
        (defvar ,bgsym1 '((t (:foreground "brightwhite"            :background ,color :weight bold             ))))
        (defvar ,bgsym2 '((t (:foreground "brightwhite"            :background ,color :weight bold :underline t))))
        (defvar ,fgsym0 '((t (:foreground ,color                                                               ))))
        (defvar ,fgsym1 '((t (:foreground ,color                                                   :underline t))))
        (defvar ,fgsym2 '((t (:foreground ,(concat "bright" color)                    :weight bold             ))))
        (defvar ,fgsym3 '((t (:foreground ,(concat "bright" color)                    :weight bold :underline t))))))
    (dolist (sym (list bgsym0 bgsym1 bgsym2 fgsym0 fgsym1 fgsym2 fgsym3))
      (eval
       `(defface ,(intern (concat (symbol-name sym) "-face"))
          ,sym ,(concat "termbright test face: " (symbol-name sym)))))))

(defvar termbright-bgblack0  '((t (:foreground "white"       :background "brightblack"                          ))))
(defvar termbright-bgblack1  '((t (:foreground "brightwhite" :background "brightblack" :weight bold             ))))
(defvar termbright-bgblack2  '((t (:foreground "brightwhite" :background "brightblack" :weight bold :underline t))))
(defvar termbright-white0    '((t (:foreground "white"                                                          ))))
(defvar termbright-white1    '((t (:foreground "white"                                              :underline t))))
(defvar termbright-white2    '((t (:foreground "brightwhite"                           :weight bold             ))))
(defvar termbright-white3    '((t (:foreground "brightwhite"                           :weight bold :underline t))))
(dolist (sym '(termbright-bgblack0 termbright-bgblack1 termbright-bgblack2
                                   termbright-white0 termbright-white1 termbright-white2 termbright-white3))
  (eval
   `(defface ,(intern (concat (symbol-name sym) "-face"))
      ,sym ,(concat "termbright test face: " (symbol-name sym)))))

(custom-theme-set-faces
 'termbright

 `(region              ,termbright-bgblue0)
 `(match               ,termbright-bgyellow1)
 `(highlight           ,termbright-bgblack2)
 `(isearch             ,termbright-bgblack2)
 `(secondary-selection ,termbright-bgblack0)
 `(lazy-highlight      ,termbright-bgblack0)
 `(query-replace       ,termbright-bgblack0)

 `(shadow              ,termbright-green0)
 `(minibuffer-prompt   ,termbright-white2)
 `(isearch-fail        ,termbright-red2)
 `(trailing-whitespace ,termbright-red3)
 `(button              ,termbright-green3)
 `(link                ,termbright-green3)
 `(link-visited        ,termbright-green1)

 `(font-lock-warning-face              ,termbright-red3)
 `(font-lock-preprocessor-face         ,termbright-green2)
 `(font-lock-doc-face                  ,termbright-green0)
 `(font-lock-doc-markup-face           ,termbright-green2)
 `(font-lock-comment-face              ,termbright-green0)
 `(font-lock-comment-delimiter-face    ,termbright-green2)
 `(font-lock-string-face               ,termbright-green0)
 `(font-lock-escape-face               ,termbright-green2)
 `(font-lock-regexp-grouping-backslash ,termbright-green2)
 `(font-lock-regexp-grouping-construct ,termbright-green2)
 `(font-lock-negation-char-face        ,termbright-green2)
 `(font-lock-constant-face             ,termbright-green2)
 `(font-lock-number-face               ,termbright-green2)
 `(font-lock-function-name-face        ,termbright-yellow3)
 `(font-lock-variable-name-face        ,termbright-yellow2)
 `(font-lock-property-name-face        ,termbright-yellow2)
 `(font-lock-type-face                 ,termbright-magenta2)
 `(font-lock-keyword-face              ,termbright-cyan2)
 `(font-lock-builtin-face              ,termbright-cyan2)

 `(tab-bar                  ,termbright-bgblack0)
 `(tab-bar-tab              ,termbright-bgblue2)
 `(tab-bar-tab-inactive     ,termbright-bgblue0)
 `(header-line              ,termbright-bgblue2)
 `(mode-line                ,termbright-bgblue1)
 `(mode-line-inactive       ,termbright-bgblue0)
 `(mode-line-buffer-id      ,termbright-white2)
 `(mode-line-emphasis       ,termbright-white3)
 `(mode-line-highlight      ,termbright-white3)
 `(line-number              ,termbright-green0)
 `(line-number-current-line ,termbright-green2)

 ;; diff
 `(diff-header            ,termbright-bgblack1)
 `(diff-added             ,termbright-green2)
 `(diff-refine-added      ,termbright-bggreen1)
 `(diff-indicator-added   ,termbright-bggreen1)
 `(diff-removed           ,termbright-red2)
 `(diff-refine-removed    ,termbright-bgred1)
 `(diff-indicator-removed ,termbright-bgred1)
 `(diff-changed           ,termbright-yellow2)
 `(diff-refine-changed    ,termbright-bgyellow1)
 `(diff-indicator-changed ,termbright-bgyellow1)

 ;; company
 `(company-tooltip           ,termbright-bgblack0)
 `(company-tooltip-selection ,termbright-bgblack1)

 ;; magit
 `(magit-section-highlight      ,termbright-bgblack0)
 `(magit-diff-context-highlight ,termbright-bgblack0)
 )

(provide-theme 'termbright)

;;; termbright-theme.el ends here
