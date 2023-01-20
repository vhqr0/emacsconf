;;; -*- lexical-binding: t -*-



;;* maps

(define-key evil-leader-map "x" 'god-C-x) ; god
(define-key evil-leader-map "c" 'god-C-c) ; god
(define-key evil-leader-map "h" help-map)
(define-key evil-leader-map "s" search-map)
(define-key evil-leader-map "g" goto-map)
(define-key evil-leader-map "a" abbrev-map)
(define-key evil-leader-map "r" ctl-x-r-map)
(define-key evil-leader-map "y" ctl-x-x-map)
(define-key evil-leader-map "l" ctl-x-l-map) ; setup
(define-key evil-leader-map "n" narrow-map)
(define-key evil-leader-map "v" vc-prefix-map)
(define-key evil-leader-map "p" projectile-command-map) ; projectile
(define-key evil-leader-map "4" ctl-x-4-map)
(define-key evil-leader-map "5" ctl-x-5-map)
(define-key evil-leader-map "t" tab-prefix-map)
(define-key evil-leader-map "\r" mule-keymap)



;;* layout

(define-key evil-leader-map "1" 'delete-other-windows)
(define-key evil-leader-map "2" 'split-window-below)
(define-key evil-leader-map "3" 'split-window-right)
(define-key evil-leader-map "q" 'quit-window)
(define-key evil-leader-map "o" 'other-window)
(define-key evil-leader-map "0" 'delete-window)
(define-key evil-leader-map "9" 'rotate-window) ; simple-x
(define-key evil-leader-map "u" 'winner-undo)   ; winner
(define-key evil-leader-map "U" 'winner-redo)   ; winner
(define-key evil-leader-map "k" 'kill-buffer)
(define-key evil-leader-map [left] 'previous-buffer)
(define-key evil-leader-map [right] 'next-buffer)
(define-key evil-leader-map "\t" 'evil-jump-backward)
(define-key evil-leader-map [tab] 'evil-jump-backward)
(define-key evil-leader-map [backtab] 'evil-jump-forward)



;;* pair

(defun +insert-pair-1 (&optional arg) (interactive "P") (insert-pair (or arg '(1))))
(dolist (pair insert-pair-alist)
  (define-key evil-leader-map `[,(car pair)] '+insert-pair-1))

(define-key evil-leader-map ")" 'move-past-close-and-reindent)



;;* others

(define-key evil-leader-map "\C-u" 'universal-argument)
(define-key evil-leader-map "\s" 'execute-extended-command)
(define-key evil-leader-map "b" 'switch-to-buffer)
(define-key evil-leader-map "f" 'find-file)
(define-key evil-leader-map "d" 'dired)
(define-key evil-leader-map "j" 'dired-jump)
(define-key evil-leader-map "z" 'repeat)
(define-key evil-leader-map ";" 'eval-expression)
(define-key evil-leader-map "e" 'eshell-dwim) ; simple-x
(define-key evil-leader-map "!" 'shell-command)
(define-key evil-leader-map "&" 'async-shell-command)
(define-key evil-leader-map "$" 'ispell-word)
(define-key evil-leader-map "^" 'delete-indentation)
(define-key evil-leader-map "%" 'query-replace-regexp)
(define-key evil-leader-map "," 'xref-pop-marker-stack)
(define-key evil-leader-map "." 'xref-find-definitions)
(define-key evil-leader-map "?" 'xref-find-references)
