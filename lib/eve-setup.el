(require 'eve)

(eval-when-compile
  (require 'view))

(global-set-key "\C-z" 'eve-change-mode-to-vi)

(setq view-read-only t)

(with-eval-after-load 'view
  (define-key view-mode-map "g" nil)
  (dolist (key '("_" "j" "k" "h" "l" "w" "W" "b" "B" "e" "E" "U"
                 "0" "^" "$" "gg" "G" "{" "}" "[" "]" "(" ")" "`" "'"
                 "f" "F" "t" "T" ";" "," "/" "?" "n" "N" "gf" "gw" "gj" "g/"))
    (define-key view-mode-map key (intern (concat "eve-" key))))
  (define-key view-mode-map "y"  'eve-operator)
  (define-key view-mode-map "-"  'eve-operator)
  (define-key view-mode-map "."  'repeat)
  (define-key view-mode-map "m"  'point-to-register)
  (define-key view-mode-map ":"  'execute-extended-command)
  (define-key view-mode-map "v"  'set-mark-command)
  (define-key view-mode-map "V"  "0vj")
  (define-key view-mode-map "gt" 'tab-next)
  (define-key view-mode-map "gT" 'tab-previous)
  (define-key view-mode-map "gn" "\C-c\C-n")
  (define-key view-mode-map "gp" "\C-c\C-p"))

(define-key special-mode-map "n" 'next-line)
(define-key special-mode-map "p" 'previous-line)

(defvar eve-jk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j" "n")
    (define-key map "k" "p")
    (define-key map ":" 'execute-extended-command)
    map))

(define-minor-mode eve-jk-mode
  "Eve jk mode."
  :keymap eve-jk-mode-map
  :lighter " <JK>")

(defun eve-setup ()
  (cond ((derived-mode-p 'special-mode 'compilation-mode 'dired-mode)
         (eve-jk-mode 1))
        ((derived-mode-p 'prog-mode 'text-mode 'fundamental-mode)
         (eve-change-mode-to-vi))))

(defun eve-setup-view ()
  (if view-mode
      (when (or eve-vi-mode eve-insert-mode)
        (eve-change-mode-to-emacs))
    (eve-setup)))

(add-hook 'after-change-major-mode-hook 'eve-setup)
(add-hook 'view-mode-hook 'eve-setup-view)

(provide 'eve-setup)
