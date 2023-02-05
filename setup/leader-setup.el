;;; -*- lexical-binding: t -*-



;;* remap

(define-key god-leader-map "h" help-map) ; for spc h f/o
(define-key god-leader-map "g" goto-map) ; for spc g i



;;* layout

(define-key god-leader-map "1"       'delete-other-windows)
(define-key god-leader-map "2"       'split-window-below)
(define-key god-leader-map "3"       'split-window-right)
(define-key god-leader-map "q"       'quit-window)
(define-key god-leader-map "o"       'other-window)
(define-key god-leader-map "0"       'delete-window)
(define-key god-leader-map "9"       'rotate-window) ; simple-x
(define-key god-leader-map "\t"      'evil-jump-backward)
(define-key god-leader-map [tab]     'evil-jump-backward)
(define-key god-leader-map [backtab] 'evil-jump-forward)



;;* pair

(defun +insert-pair-1 (&optional arg) (interactive "P") (insert-pair (or arg '(1))))
(dolist (pair insert-pair-alist)
  (define-key god-leader-map `[,(car pair)] '+insert-pair-1))



;;* others

(define-key god-leader-map "\s" 'execute-extended-command)
(define-key god-leader-map "b"  'switch-to-buffer)
(define-key god-leader-map "f"  'find-file)
(define-key god-leader-map "d"  'dired)
(define-key god-leader-map "j"  'dired-jump)
(define-key god-leader-map "e"  'eshell-dwim) ; simple-x
(define-key god-leader-map ";"  'eval-expression)
(define-key god-leader-map "!"  'shell-command)
(define-key god-leader-map "&"  'async-shell-command)
(define-key god-leader-map "$"  'ispell-word)
(define-key god-leader-map "%"  'query-replace-regexp)
(define-key god-leader-map ","  'xref-pop-marker-stack)
(define-key god-leader-map "."  'xref-find-definitions)
(define-key god-leader-map "?"  'xref-find-references)
