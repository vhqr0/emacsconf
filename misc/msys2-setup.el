;; should run emacs in mingw64-shell

(setq package-check-signature nil)

(setq xdg-open-program "c:/Windows/system32/cmd.exe //c start")

(prefer-coding-system 'utf-8-unix)

(let ((font (font-spec :name "Courier New")))
  (when (find-font font)
    (set-face-attribute 'default nil :font font)))

(let ((font (font-spec :name "微软雅黑")))
  (dolist (charset '(cjk-misc han bopomofo kana hangul))
    (set-fontset-font t charset font)))
