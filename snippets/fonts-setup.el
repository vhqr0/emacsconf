(let ((font (font-spec :name "DejaVu Sans Mono")))
  (when (find-font font)
    (set-face-attribute 'default nil :font font)))

(let ((font (font-spec :name "WenQuanYi Micro Hei")))
    (when (find-font font)
      (dolist (charset '(cjk-misc han bopomofo kana hangul))
        (set-fontset-font t charset font))))
