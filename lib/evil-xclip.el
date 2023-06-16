;;; evil-xclip.el --- Xclip operator of evil. -*- lexical-binding: t *-

(require 'evil)

(eval-when-compile
  (require 'evil))

(defvar xclip-command
  (if (eq system-type 'windows-nt)
      "clip.exe"
    "xclip -selection clip"))

(defun xclip-region (start end)
  (call-shell-region start end xclip-command))

(evil-define-operator evil-operator-xclip (start end)
  :move-point nil
  (interactive "<r>")
  (xclip-region start end))

(provide 'evil-xclip)
