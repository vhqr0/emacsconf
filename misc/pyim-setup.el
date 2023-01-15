(dolist (pkg '(pyim pyim-basedict posframe popup))
  (add-to-list '+package pkg))



(defvar +pyim-zirjma-keymaps
  '(("a"    "a"    "a"          )
    ("b"    "b"    "ou"         )
    ("c"    "c"    "iao"        )
    ("d"    "d"    "uang" "iang")
    ("e"    "e"    "e"          )
    ("f"    "f"    "en"         )
    ("g"    "g"    "eng"        )
    ("h"    "h"    "ang"        )
    ("i"    "ch"   "i"          )
    ("j"    "j"    "an"         )
    ("k"    "k"    "ao"         )
    ("l"    "l"    "ai"         )
    ("m"    "m"    "ian"        )
    ("n"    "n"    "in"         )
    ("o"    "o"    "uo"   "o"   )
    ("p"    "p"    "un"         )
    ("q"    "q"    "iu"         )
    ("r"    "r"    "uan"  "er"  )
    ("s"    "s"    "iong" "ong" )
    ("t"    "t"    "ue"   "ve"  )
    ("u"    "sh"   "u"          )
    ("v"    "zh"   "v"    "ui"  )
    ("w"    "w"    "ia"   "ua"  )
    ("x"    "x"    "ie"         )
    ("y"    "y"    "uai"  "ing" )
    ("z"    "z"    "ei"         )
    ("aa"   "a"                 )
    ("ah"   "ang"               )
    ("ai"   "ai"                )
    ("aj"   "an"                )
    ("ak"   "ao"                )
    ("al"   "ai"                )
    ("an"   "an"                )
    ("ao"   "ao"                )
    ("ee"   "e"                 )
    ("ef"   "en"                )
    ("eg"   "eng"               )
    ("ei"   "ei"                )
    ("en"   "en"                )
    ("er"   "er"                )
    ("ez"   "ei"                )
    ("ob"   "ou"                )
    ("oo"   "o"                 )
    ("ou"   "ou"                )))

(defvar +pyim-punctuation-dict
  '(("'"  "‘"  "’")
    ("\"" "“"  "”")
    ("("  "（"    )
    (")"  "）"    )
    ("["  "【"    )
    ("]"  "】"    )
    ("{"  "『"    )
    ("}"  "』"    )
    ("<"  "《"    )
    (">"  "》"    )
    ("_"  "——"    )
    ("?"  "？"    )
    ("!"  "！"    )
    (","  "，"    )
    ("."  "。"    )
    (";"  "；"    )
    (":"  "："    )
    ("\\" "、"    )))



(setq default-input-method "pyim")

(setq pyim-default-scheme 'zirjma
      pyim-enable-shortcode nil
      pyim-pinyin-fuzzy-alist nil
      pyim-punctuation-dict +pyim-punctuation-dict)

(with-eval-after-load 'pyim
  (define-key pyim-mode-map "." 'pyim-page-next-page)
  (define-key pyim-mode-map "," 'pyim-page-previous-page)
  (pyim-scheme-add
   `(zirjma
     :document "zirjma"
     :class shuangpin
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :prefer-triggers nil
     :cregexp-support-p t
     :keymaps ,+pyim-zirjma-keymaps))
  (pyim-basedict-enable))
