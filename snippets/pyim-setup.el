(setq +package (append +package '(pyim pyim-basedict posframe popup go-translate)))

(with-eval-after-load 'pyim
  (define-key pyim-mode-map "." 'pyim-page-next-page)
  (define-key pyim-mode-map "," 'pyim-page-previous-page)
  (pyim-scheme-add
   '(zirjma
     :document "zirjma"
     :class shuangpin
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :prefer-trigger-chars nil
     :keymaps
     (("a" "a" "a")
      ("b" "b" "ou")
      ("c" "c" "iao")
      ("d" "d" "uang" "iang")
      ("e" "e" "e")
      ("f" "f" "en")
      ("g" "g" "eng")
      ("h" "h" "ang")
      ("i" "ch" "i")
      ("j" "j" "an")
      ("k" "k" "ao")
      ("l" "l" "ai")
      ("m" "m" "ian")
      ("n" "n" "in")
      ("o" "o" "uo" "o")
      ("p" "p" "un")
      ("q" "q" "iu")
      ("r" "r" "uan" "er")
      ("s" "s" "iong" "ong")
      ("t" "t" "ue" "ve")
      ("u" "sh" "u")
      ("v" "zh" "v" "ui")
      ("w" "w" "ia" "ua")
      ("x" "x" "ie")
      ("y" "y" "uai" "ing")
      ("z" "z" "ei")
      ("aa" "a")
      ("ah" "ang")
      ("ai" "ai")
      ("aj" "an")
      ("ak" "ao")
      ("al" "ai")
      ("an" "an")
      ("ao" "ao")
      ("ee" "e")
      ("ef" "en")
      ("eg" "eng")
      ("ei" "ei")
      ("en" "en")
      ("er" "er")
      ("ez" "ei")
      ("ob" "ou")
      ("oo" "o")
      ("ou" "ou"))))
  (setq pyim-default-scheme 'zirjma)
  (add-to-list 'pyim-punctuation-dict '("\\" "、"))
  (setq pyim-enable-shortcode nil
        pyim-pinyin-fuzzy-alist nil)
  (pyim-basedict-enable))

(setq default-input-method "pyim")

(setq gts-translate-list '(("en" "zh")))
(global-set-key "\M-T" 'gts-do-translate)
