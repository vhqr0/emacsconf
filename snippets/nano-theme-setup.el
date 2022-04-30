;; package-install nano-theme
;; apt install fonts-roboto

(when (display-graphic-p)
  (require 'nano-theme)
  (nano-mode)
  (nano-light))
