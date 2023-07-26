(defvar +fonts-en "Iosevka")
(defvar +fonts-cjk "LXGW WenKai Mono")

(let ((font (font-spec :name +fonts-en)))
  (when (find-font font)
    (set-face-attribute 'default nil :font font)))

(let ((font (font-spec :name +fonts-cjk)))
  (dolist (chars '(cjk-misc han bopomofo kana hangul))
    (set-fontset-font t chars font)))
