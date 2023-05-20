;;; termbright-theme.el --- a more usable theme for white-on-black terminals

;;; Commentary:

;; Modified version of [https://github.com/bmastenbrook/termbright-theme-el].

;;; Code:

(deftheme termbright
  "termbright, a more usable theme for white-on-black terminals")

;; specialized: replace background from black to brightblack
(defvar termbright-bgblack0  '((t (:foreground "white"       :background "brightblack"                          ))))
(defvar termbright-bgblack1  '((t (:foreground "brightwhite" :background "brightblack" :weight bold             ))))
(defvar termbright-bgblack2  '((t (:foreground "brightwhite" :background "brightblack" :weight bold :underline t))))

;; specialized: replace foreground from black to brightblack
(defvar termbright-black0    '((t (:foreground "brightblack"                                                    ))))
(defvar termbright-black1    '((t (:foreground "brightblack"                                        :underline t))))

;; specialized:
;; replace background from white to brightwhite
;; replace foreground from white/brightwhite to black/brightblack
(defvar termbright-bgwhite0  '((t (:foreground "black"       :background "brightwhite"                          ))))
(defvar termbright-bgwhite1  '((t (:foreground "brightblack" :background "brightwhite" :weight bold             ))))
(defvar termbright-bgwhite2  '((t (:foreground "brightblack" :background "brightwhite" :weight bold :underline t))))

(dolist (color '("red" "green" "yellow" "blue" "magenta" "cyan" "black" "white"))
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

(defvar termbright-match      termbright-bgyellow2)
(defvar termbright-highlight0 termbright-bgblack0)
(defvar termbright-highlight1 termbright-bgblack1)
(defvar termbright-highlight2 termbright-bgblack2)
(defvar termbright-warn       termbright-red3)
(defvar termbright-shadow0    termbright-green0)
(defvar termbright-shadow1    termbright-green2)
(defvar termbright-link0      termbright-green1)
(defvar termbright-link1      termbright-green3)
(defvar termbright-light0     termbright-white2)
(defvar termbright-light1     termbright-white3)
(defvar termbright-index0     termbright-yellow2)
(defvar termbright-index1     termbright-yellow3)
(defvar termbright-ui0        termbright-bggreen0)
(defvar termbright-ui1        termbright-bggreen1)
(defvar termbright-ui2        termbright-bggreen2)
(defvar termbright-added0     termbright-green0)
(defvar termbright-added1     termbright-bggreen1)
(defvar termbright-removed0   termbright-red0)
(defvar termbright-removed1   termbright-bgred1)
(defvar termbright-changed0   termbright-yellow0)
(defvar termbright-changed1   termbright-bgyellow1)

(custom-theme-set-faces
 'termbright

 `(highlight                ,termbright-highlight2)
 `(region                   ,termbright-highlight0)
 `(secondary-selection      ,termbright-highlight0)
 `(match                    ,termbright-match)
 `(lazy-highlight           ,termbright-highlight0)
 `(isearch                  ,termbright-highlight2)
 `(isearch-fail             ,termbright-warn)
 `(shadow                   ,termbright-shadow0)
 `(minibuffer-prompt        ,termbright-light0)
 `(link                     ,termbright-link1)
 `(link-visited             ,termbright-link0)
 `(trailing-whitespace      ,termbright-warn)
 `(line-number              ,termbright-shadow0)
 `(line-number-current-line ,termbright-shadow1)
 `(tab-bar                  ,termbright-highlight0)
 `(tab-bar-tab              ,termbright-ui2)
 `(tab-bar-tab-inactive     ,termbright-ui0)
 `(mode-line                ,termbright-ui1)
 `(mode-line-inactive       ,termbright-ui0)

 `(font-lock-warning-face              ,termbright-warn)
 `(font-lock-preprocessor-face         ,termbright-shadow1)
 `(font-lock-doc-face                  ,termbright-shadow0)
 `(font-lock-doc-markup-face           ,termbright-shadow1)
 `(font-lock-comment-face              ,termbright-shadow0)
 `(font-lock-comment-delimiter-face    ,termbright-shadow1)
 `(font-lock-string-face               ,termbright-shadow0)
 `(font-lock-escape-face               ,termbright-shadow1)
 `(font-lock-regexp-grouping-backslash ,termbright-shadow1)
 `(font-lock-regexp-grouping-construct ,termbright-shadow1)
 `(font-lock-negation-char-face        ,termbright-shadow1)
 `(font-lock-constant-face             ,termbright-shadow1)
 `(font-lock-number-face               ,termbright-shadow1)
 `(font-lock-function-name-face        ,termbright-index1)
 `(font-lock-variable-name-face        ,termbright-index0)
 `(font-lock-property-name-face        ,termbright-index0)
 `(font-lock-type-face                 ,termbright-light0)
 `(font-lock-keyword-face              ,termbright-light0)
 `(font-lock-builtin-face              ,termbright-light0)

 ;; diff
 `(diff-header            ,termbright-highlight1)
 `(diff-added             ,termbright-added0)
 `(diff-refine-added      ,termbright-added1)
 `(diff-indicator-added   ,termbright-added1)
 `(diff-removed           ,termbright-removed0)
 `(diff-refine-removed    ,termbright-removed1)
 `(diff-indicator-removed ,termbright-removed1)
 `(diff-changed           ,termbright-changed0)
 `(diff-refine-changed    ,termbright-changed1)
 `(diff-indicator-changed ,termbright-changed1)

 ;; company
 `(company-tooltip           ,termbright-highlight0)
 `(company-tooltip-selection ,termbright-highlight1)

 ;; magit
 `(magit-section-highlight      ,termbright-highlight0)
 `(magit-diff-context-highlight ,termbright-highlight0)
 )

(provide-theme 'termbright)

;;; termbright-theme.el ends here
