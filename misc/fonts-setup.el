(defvar +fonts-en
  (if (eq system-type 'windows-nt)
      "Courier New"
    "DejaVu Sans Mono"))
(defvar +fonts-cjk
  (if (eq system-type 'windows-nt)
      "微软雅黑"
    "WenQuanYi Micro Hei"))

(let ((font (font-spec :name +fonts-en)))
  (when (find-font font)
    (set-face-attribute 'default nil :font font)))

(let ((font (font-spec :name +fonts-cjk)))
  (dolist (charset '(cjk-misc han bopomofo kana hangul))
    (set-fontset-font t charset font)))
