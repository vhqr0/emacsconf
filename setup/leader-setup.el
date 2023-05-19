;;; -*- lexical-binding: t -*-



;;* keymaps

(defvar +leader-prefix-map (make-sparse-keymap))

(defvar +shift-translation-alist
  '((?a  . ?A) (?b  . ?B) (?c  . ?C ) (?d  . ?D )
    (?e  . ?E) (?f  . ?F) (?g  . ?G ) (?h  . ?H )
    (?i  . ?I) (?j  . ?J) (?k  . ?K ) (?l  . ?L )
    (?m  . ?M) (?n  . ?N) (?o  . ?O ) (?p  . ?P )
    (?q  . ?Q) (?r  . ?R) (?s  . ?S ) (?t  . ?T )
    (?u  . ?U) (?v  . ?V) (?w  . ?W ) (?x  . ?X )
    (?y  . ?Y) (?z  . ?Z) (?1  . ?! ) (?2  . ?@ )
    (?3  . ?#) (?4  . ?$) (?5  . ?% ) (?6  . ?^ )
    (?7  . ?&) (?8  . ?*) (?9  . ?\() (?0  . ?\))
    (?`  . ?~) (?-  . ?_) (?=  . ?+ ) (?\[ . ?{ )
    (?\] . ?}) (?\\ . ?|) (?\; . ?: ) (?'  . ?\")
    (?,  . ?<) (?.  . ?>) (?/  . ?? )            ))

(defvar +shift-prefix-map
  (let ((map (make-sparse-keymap)))
    (dolist (cons +shift-translation-alist)
      (let ((from (car cons))
            (to (cdr cons)))
        (define-key map (vector from)
                    `(menu-item
                      "" nil :filter
                      (lambda (cmd) (key-binding (vector ,to)))))))
    map))



;;* universal arguments

(defun +leader-universal-arguments ()
  (interactive)
  (let ((arg (if current-prefix-arg
                 (prefix-numeric-value current-prefix-arg)
               1)))
    (setq prefix-arg (list (* 4 arg))))
  (set-transient-map +leader-prefix-map))

(define-key +leader-prefix-map "u" '+leader-universal-arguments)



;;* god C-c

(defun +god-C-c--execute (bind)
  (interactive)
  (cond ((commandp bind)
         (setq this-command bind
               real-this-command bind)
         (if (commandp bind t)
             (call-interactively bind)
           (execute-kbd-macro bind))
         bind)
        ((keymapp bind)
         (set-transient-map bind))))

(defun +god-C-c ()
  (interactive)
  (let ((char (read-char "C-c C-")))
    (cond ((+god-C-c--execute (key-binding (kbd (format "C-c C-%c" char)))))
          ((+god-C-c--execute (key-binding (kbd (format "C-c %c" char)))))
          (t
           (user-error "no key binding on 'C-c C-%c' or 'C-c %c'" char char)))))

(define-key +leader-prefix-map "c" '+god-C-c)



;;* sub prefixes

(define-key +leader-prefix-map "`" 'tmm-menubar)

(define-key +leader-prefix-map "h" help-map)
(define-key +leader-prefix-map "g" goto-map)
(define-key +leader-prefix-map "s" search-map)
(define-key +leader-prefix-map "n" narrow-map)
(define-key +leader-prefix-map "a" abbrev-map)
(define-key +leader-prefix-map "v" vc-prefix-map)
(define-key +leader-prefix-map "p" project-prefix-map)
(define-key +leader-prefix-map "l" ctl-x-l-map) ; default-setup
(define-key +leader-prefix-map "r" ctl-x-r-map)
(define-key +leader-prefix-map "x" ctl-x-x-map)
(define-key +leader-prefix-map "4" ctl-x-4-map)
(define-key +leader-prefix-map "5" ctl-x-5-map)
(define-key +leader-prefix-map "t" tab-prefix-map)
(with-eval-after-load 'evil             ; evil
  (define-key +leader-prefix-map "w" evil-window-map))



;;* layout

(define-key +leader-prefix-map "1"     'delete-other-windows)
(define-key +leader-prefix-map "2"     'split-window-below)
(define-key +leader-prefix-map "3"     'split-window-right)
(define-key +leader-prefix-map "q"     'quit-window)
(define-key +leader-prefix-map "o"     'other-window)
(define-key +leader-prefix-map "0"     'delete-window)
(define-key +leader-prefix-map "9"     'rotate-window) ; simple-x
(define-key +leader-prefix-map [left]  'winner-undo)   ; winner
(define-key +leader-prefix-map [right] 'winner-redo)   ; winner



;;* others

(define-key +leader-prefix-map "\s" 'execute-extended-command)
(define-key +leader-prefix-map "z"  'repeat)
(define-key +leader-prefix-map "b"  'switch-to-buffer)
(define-key +leader-prefix-map "k"  'kill-buffer)
(define-key +leader-prefix-map "f"  'find-file)
(define-key +leader-prefix-map "j"  'dired-jump)
(define-key +leader-prefix-map "e"  'eshell-dwim) ; simple-x
(define-key +leader-prefix-map "="  'format-dwim) ; simple-x
(define-key +leader-prefix-map ";"  'eval-expression)
(define-key +leader-prefix-map "#"  'server-edit)
(define-key +leader-prefix-map "!"  'shell-command)
(define-key +leader-prefix-map "&"  'async-shell-command)
(define-key +leader-prefix-map "$"  'ispell-word)
(define-key +leader-prefix-map "%"  'query-replace-regexp)
(define-key +leader-prefix-map ","  'xref-pop-marker-stack)
(define-key +leader-prefix-map "."  'xref-find-definitions)
(define-key +leader-prefix-map "?"  'xref-find-references)
