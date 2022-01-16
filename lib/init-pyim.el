(require 'eve)
(require 'pyim)
(require 'pyim-basedict)

(defun pyim-probe-eve-vi-mode ()
  eve-vi-mode)

(setq-default pyim-english-input-switch-functions
              '(pyim-probe-eve-vi-mode))

(pyim-scheme-add
 '(zirjma
   :document "自然码"
   :class shuangpin
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars  "abcdefghijklmnopqrstuvwxyz"
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
    ("aj" "an")
    ("ak" "ao")
    ("al" "ai")
    ("ee" "e")
    ("ef" "en")
    ("eg" "eng")
    ("er" "er")
    ("ez" "ei")
    ("ob" "ou")
    ("oo" "o"))))

(setq pyim-default-scheme 'zirjma)

(pyim-basedict-enable)

(provide 'init-pyim)

;;;###autoload
(with-eval-after-load 'pyim
  (require 'init-pyim))
