;;; evil-grep.el --- :grep. -*- lexical-binding: t *-

(require 'evil)

(eval-when-compile
  (require 'evil))

;; -n: display line number, disabled in non-terminal by default
;; -H: display file name, disabled in single-file-search by default
;; --no-heading: display file name inline, disabled by default
(defvar evil-grep-prefix "rg -n -H --no-heading ")

(declare-function grep--save-buffers "grep")

(evil-define-command evil-grep (arg)
  (interactive "<sh>")
  (require 'grep)
  (grep--save-buffers)
  (compilation-start (concat evil-grep-prefix arg) 'grep-mode))

(provide 'evil-grep)
