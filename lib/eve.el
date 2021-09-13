;;; eve.el --- Emacs Vi Emu. -*- lexical-binding: t -*-

;;; Commentary:
;; Add this code to your init file:
;; (global-set-key (kbd "C-z") 'eve-change-mode-to-vi)

;;; Code:



(defvar eve-insert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j"    'eve-jk)
    (define-key map "\M-z" 'eve-change-mode-to-vi)
    (define-key map "\C-z" 'eve-change-mode-to-emacs)
    map)
  "Eve insert mode map.")

(defvar eve-vi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'undefined)
    (define-key map "\s"   'scroll-up-command)
    (define-key map "\d"   'scroll-down-command)
    (define-key map "\C-z" 'eve-change-mode-to-emacs)

    (define-key map "1" 'eve-digit-arg)
    (define-key map "2" 'eve-digit-arg)
    (define-key map "3" 'eve-digit-arg)
    (define-key map "4" 'eve-digit-arg)
    (define-key map "5" 'eve-digit-arg)
    (define-key map "6" 'eve-digit-arg)
    (define-key map "7" 'eve-digit-arg)
    (define-key map "8" 'eve-digit-arg)
    (define-key map "9" 'eve-digit-arg)

    (define-key map "c"  'eve-command-arg)
    (define-key map "y"  'eve-command-arg)
    (define-key map "d"  'eve-command-arg)
    (define-key map "="  'eve-command-arg)
    (define-key map "#"  'eve-command-arg)
    (define-key map "@"  'eve-command-arg)
    (define-key map "gc" 'eve-gc)
    (define-key map "gy" 'eve-gy)
    (define-key map "s"  'eve-s)
    (define-key map "C"  "c$")
    (define-key map "Y"  "y$")
    (define-key map "D"  "d$")

    (define-key map "i" 'eve-i)
    (define-key map "I" 'eve-I)
    (define-key map "a" 'eve-a)
    (define-key map "A" 'eve-A)
    (define-key map "o" 'eve-o)
    (define-key map "O" 'eve-O)

    (define-key map "." 'eve-repeat)
    (define-key map "u" 'undo)
    (define-key map "m" 'point-to-register)
    (define-key map ":" 'execute-extended-command)

    (define-key map "j"  'eve-j)
    (define-key map "k"  'eve-k)
    (define-key map "h"  'eve-h)
    (define-key map "l"  'eve-l)
    (define-key map "w"  'eve-w)
    (define-key map "W"  'eve-W)
    (define-key map "b"  'eve-b)
    (define-key map "B"  'eve-B)
    (define-key map "e"  'eve-e)
    (define-key map "E"  'eve-E)
    (define-key map "U"  'eve-U)
    (define-key map "0"  'eve-0)
    (define-key map "^"  'eve-^)
    (define-key map "$"  'eve-$)
    (define-key map "gg" 'eve-gg)
    (define-key map "G"  'eve-G)
    (define-key map "f"  'eve-f)
    (define-key map "F"  'eve-F)
    (define-key map "t"  'eve-t)
    (define-key map "T"  'eve-T)
    (define-key map ";"  'eve-\;)
    (define-key map ","  'eve-\,)
    (define-key map "/"  'eve-/)
    (define-key map "?"  'eve-?)
    (define-key map "n"  'eve-n)
    (define-key map "N"  'eve-N)
    (define-key map "`"  'eve-\`)
    (define-key map "'"  'eve-\')

    (define-key map "p" 'eve-p)
    (define-key map "P" 'eve-P)
    (define-key map "x" 'eve-x)
    (define-key map "X" 'eve-X)
    (define-key map "r" 'eve-r)
    (define-key map "J"  "j\M-^")
    map)
  "Eve vi mode map.")

(defvar-local eve-d-com nil
  "Last (m-com val com) used by eve-repeat.")

(defvar eve-in-repeat nil
  "Set to t while repeating to determine if read-xxx.")



(define-minor-mode eve-insert-mode
  "Eve insert mode."
  :keymap eve-insert-mode-map)

(define-minor-mode eve-vi-mode
  "Eve vi mode."
  :keymap eve-vi-mode-map)

(defvar-local eve-insert-point (make-marker)
  "Save last insert start point.")

(defvar-local eve-insert-last nil
  "Save last insert string.")

(defvar-local eve-current-mode nil)

(defun eve-change-mode (new-mode)
  "Change mode to `new-mode'.
`new-mode' is vi-mode, insert-mode or emacs-mode."
  (let (id)
    (unless (eq new-mode eve-current-mode)
      (cond ((eq new-mode 'vi-mode)
             (when (eq eve-current-mode 'insert-mode)
               (setq eve-insert-last
                     (buffer-substring-no-properties
                      (point) eve-insert-point))
               (let ((i-com (nth 0 eve-d-com))
                     (val (nth 1 eve-d-com)))
                 (when (and val (> val 1))
                   (setq eve-d-com `(,i-com ,(1- val)))
                   (eve-repeat)
                   (setq eve-d-com `(,i-com ,val))))
               (eve-insert-mode -1))
             (eve-vi-mode 1)
             (setq id "<V>"))
            ((eq new-mode 'insert-mode)
             (when (eq eve-current-mode 'vi-mode)
               (move-marker eve-insert-point (point))
               (eve-vi-mode -1))
             (eve-insert-mode 1)
             (setq id "<I>"))
            (t
             (eve-vi-mode -1)
             (eve-insert-mode -1)
             (setq id "<E>")))
      (setq eve-current-mode new-mode)
      (setq mode-line-buffer-identification
            `(,(concat id " %17b")))
      (force-mode-line-update))))

;;;###autoload
(defun eve-change-mode-to-vi ()
  "Change mode to vi."
  (interactive)
  (eve-change-mode 'vi-mode))

(defun eve-change-mode-to-insert ()
  "Change mode to insert."
  (interactive)
  (eve-change-mode 'insert-mode))

(defun eve-change-mode-to-emacs ()
  "Change mode to emacs."
  (interactive)
  (eve-change-mode 'emacs-mode))

(defun eve-jk ()
  "Like vim's :imap jk <esc>."
  (interactive)
  (if (and (eq eve-current-mode 'insert-mode)
           (not executing-kbd-macro)
           (not defining-kbd-macro)
           (not (sit-for 0.1 'no-redisplay)))
      (let ((next-char (read-event)))
        (if (eq next-char ?k)
            (progn
              (eve-change-mode-to-vi)
              (delete-trailing-whitespace
               (line-beginning-position)
               (line-end-position))
              (unless (bolp) (left-char)))
          (insert ?j)
          (push next-char unread-command-events)))
    (insert ?j)))



(defun eve-prefix-arg-val (char val com)
  "Process reading val."
  (while (<= ?0 char ?9)
    (setq val (+ (* (if (numberp val) val 0) 10) (- char ?0))
          char (read-char)))
  (setq prefix-arg
        (if com
            `(,val . ,com)
          val))
  (push char unread-command-events))

(defun eve-prefix-arg-com (char val com)
  "Process reading com."
  (if com
      (if (or (eq char com)
              (and (eq com ?#) (eq char ?c))
              (and (eq com ?@) (eq char ?y)))
          (eve-line
           `(,val . ,com))
        (error ""))
    (setq prefix-arg `(,val . ,char))))

(defun eve-digit-arg (arg)
  "Preprocess reading val."
  (interactive "P")
  (eve-prefix-arg-val
   last-command-event nil (cdr-safe arg)))

(defun eve-command-arg (arg)
  "Preprocess reading com."
  (interactive "P")
  (eve-prefix-arg-com last-command-event
                      (if (numberp arg) arg (car-safe arg))
                      (cdr-safe arg)))

(defun eve-gc (arg)
  "`eve-command-arg' wrapper for gc."
  (interactive "P")
  (let ((last-command-event ?#))
    (eve-command-arg arg)))

(defun eve-gy (arg)
  "`eve-command-arg' wrapper for gy."
  (interactive "P")
  (let ((last-command-event ?@))
    (eve-command-arg arg)))

(defun eve-getval (arg)
  "Get val from `prefix-arg'."
  (if (numberp arg)
      arg
    (or (car-safe arg) 1)))

(defun eve-getcom (arg)
  "Get com from `prefix-arg'."
  (cdr-safe arg))



(defvar eve-eval-functions
  "Alist of (major-mode . eval-region-function), used by `eve-eval-region'."
  '((emacs-lisp-mode . eval-region)
    (lisp-interaction-mode . eval-region)
    (python-mode . python-shell-send-region)))

(defun eve-eval-region (beg end)
  "Eval region by `major-mode'."
  (let ((func (cdr (assq major-mode eve-eval-functions))))
    (when func
      (funcall func beg end))))



(defvar-local eve-com-point (make-marker)
  "Normally set by motion, and then call `eve-exec-com'.
The region is `eve-com-point' and `point'.")

(defun eve-exec-com (m-com val com)
  "Call function by `com' and set `eve-d-com' for `eve-repeat'."
  (let ((func (assq com '((?c . kill-region)
                          (?d . kill-region)
                          (?y . copy-region-as-kill)
                          (?= . indent-region)
                          (?# . comment-or-uncomment-region)
                          (?@ . eve-eval-region)
                          (?s . eve-surround-region)))))
    (when func
      (funcall (cdr func) eve-com-point (point))
      (deactivate-mark)
      (if (eq com ?c)
          (progn
            (setq eve-d-com '(eve-i 1))
            (eve-change-mode-to-insert))
        (setq eve-d-com `(,m-com ,val ,com))))))



(defmacro eve-loop (count &rest body)
  (declare (indent 1))
  `(let ((count ,count))
     (while (> count 0)
       ,@body
       (setq count (1- count)))))

(defun eve-repeat (&optional arg)
  "Repeat by `eve-d-com'."
  (interactive "P")
  (eve-loop (or arg 1)
    (let ((eve-in-repeat t)
          (m-com (nth 0 eve-d-com))
          (val (nth 1 eve-d-com))
          (com (nth 2 eve-d-com)))
      (if m-com
          (funcall m-com `(,val . ,com))
        (error "No previous commad to repeat.")))))



(defmacro eve-define-insert (func &rest body)
  "Dispatch to `eve-tobj' when there is a com.
Eval `body' and insert or yank if in repeat."
  (declare (indent 1))
  `(defun ,func (arg)
     "Generated by `eve-define-insert'."
     (interactive "P")
     (let ((val (eve-getval arg))
           (com (eve-getcom arg)))
       (cond (com (eve-tobj arg))
             (eve-in-repeat
              (eve-loop val
                ,@body
                (insert eve-insert-last)))
             (t
              (setq eve-d-com (list ',func val))
              ,@body
              (eve-change-mode-to-insert))))))

(eve-define-insert eve-i)

(eve-define-insert eve-a
  (unless (eolp) (forward-char)))

(eve-define-insert eve-I
  (back-to-indentation))

(eve-define-insert eve-A
  (end-of-line))

(eve-define-insert eve-o
  (end-of-line)
  (newline 1)
  (indent-for-tab-command))

(eve-define-insert eve-O
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))



(defvar-local eve-save-point (make-marker)
  "Save last position by motion, and restore after `eve-exec-com'.")

(defvar eve-tobj-last nil)

(defun eve-tobj (arg)
  "Like vim's inner text object."
  (interactive "P")
  (let ((val (eve-getval arg))
        (com (eve-getcom arg)))
    (unless eve-in-repeat
      (setq eve-tobj-last (read-char)))
    (let (range)
      (setq range (cdr (assq eve-tobj-last
                             '((?w . word)
                               (?o . sexp)
                               (?f . defun)
                               (?p . paragraph)
                               (?P . page)
                               (?h . buffer)
                               (?b . pair)))))
      (cond ((eq range 'pair)
             (setq range
                   (save-excursion
                     (backward-up-list)
                     (bounds-of-thing-at-point 'sexp))))
            (range
             (setq range (bounds-of-thing-at-point range))))
      (when range
        (move-marker eve-save-point (point))
        (move-marker eve-com-point (car range))
        (goto-char (cdr range))
        (eve-exec-com 'eve-tobj val com)
        (goto-char eve-save-point)))))



(defmacro eve-define-exclusive-motion (func &rest body)
  "For h, l, w, W, b, B, F, T, /, ?, 0, ^, $, `."
  (declare (indent 1))
  `(defun ,func (arg)
     "Generated by `eve-define-exclusive-motion'."
     (interactive "P")
     (let ((val (eve-getval arg))
           (com (eve-getcom arg)))
       (when com
         (move-marker eve-save-point (point))
         (move-marker eve-com-point (point)))
       ,@body
       (when com
         (eve-exec-com ',func val com)
         (goto-char eve-save-point)))))

(defmacro eve-define-inclusive-motion (func &rest body)
  "For e, E, U, f, t."
  (declare (indent 1))
  `(defun ,func (arg)
     "Generated by `eve-define-inclusive-motion'."
     (interactive "P")
     (let ((val (eve-getval arg))
           (com (eve-getcom arg)))
       (when com
         (move-marker eve-save-point (point))
         (move-marker eve-com-point (point)))
       ,@body
       (when com
         (forward-word)
         (eve-exec-com ',func val com)
         (goto-char eve-save-point)))))

(defmacro eve-define-line-motion (func &rest body)
  "For j, k, gg, G, '."
  (declare (indent 1))
  `(defun ,func (arg)
     (interactive "P")
     "Generated by `eve-define-line-motion'."
     (let ((val (eve-getval arg))
           (com (eve-getcom arg))
           beg end tmp)
       (when com
         (move-marker eve-save-point (point))
         (setq beg (point)))
       ,@body
       (when com
         (setq end (point))
         (when (> beg end)
           (setq tmp beg beg end end tmp))
         (goto-char beg)
         (if (memq com '(?c ?s))
             (back-to-indentation)
           (beginning-of-line))
         (move-marker eve-com-point (point))
         (goto-char end)
         (end-of-line)
         (when (and (memq com '(?d ?y))
                    (not (eobp)))
           (forward-char))
         (eve-exec-com ',func val com)
         (goto-char eve-save-point)))))



;; j, k, h, l, w, W, b, B, e, E, 0, ^, $, gg, G, `, '

(eve-define-line-motion eve-line
  (line-move (1- val)))

(eve-define-line-motion eve-j
  (setq this-command 'next-line)
  (line-move val))

(eve-define-line-motion eve-k
  (setq this-command 'previous-line)
  (line-move (- val)))

(eve-define-exclusive-motion eve-h
  (backward-char val))

(eve-define-exclusive-motion eve-l
  (forward-char val))

(eve-define-exclusive-motion eve-w
  (forward-word val)
  (skip-chars-forward " \t\n"))

(eve-define-exclusive-motion eve-W
  (forward-sexp val)
  (skip-chars-forward " \t\n"))

(eve-define-inclusive-motion eve-U
  (backward-up-list val))

(eve-define-exclusive-motion eve-b
  (backward-word val))

(eve-define-exclusive-motion eve-B
  (backward-sexp val))

(eve-define-inclusive-motion eve-e
  (forward-char)
  (forward-word val)
  (backward-char))

(eve-define-inclusive-motion eve-E
  (forward-char)
  (forward-sexp val)
  (backward-char))

(eve-define-exclusive-motion eve-0
  (beginning-of-line))

(eve-define-exclusive-motion eve-^
  (back-to-indentation))

(eve-define-exclusive-motion eve-$
  (end-of-line))

(eve-define-line-motion eve-gg
  (goto-char (point-min)))

(eve-define-line-motion eve-G
  (goto-char (point-max)))

(eve-define-exclusive-motion eve-\`
  (jump-to-register (read-char)))

(eve-define-line-motion eve-\'
  (jump-to-register (read-char)))



;; f, F, ;, \,, /, ?, n, N

(defvar eve-f-t nil)

(defvar eve-f-forward nil)

(defvar eve-f-char nil)

(defun eve-find-char (val)
  "Find `eve-f-char' by `eve-f-forward'."
  (unless eve-in-repeat
    (setq eve-f-char (read-char)))
  (let ((point (point)))
    (condition-case nil
        (if eve-f-forward
            (progn
              (forward-char)
              (when eve-f-t
                (forward-char))
              (search-forward (string eve-f-char) nil nil val)
              (backward-char))
          (backward-char)
          (search-backward (string eve-f-char) nil nil val))
      (search-failed
       (goto-char point)
       (error "Search failed.")))))

(eve-define-inclusive-motion eve-f
  (setq eve-f-t nil
        eve-f-forward t)
  (eve-find-char val))

(eve-define-inclusive-motion eve-t
  (setq eve-f-t t
        eve-f-forward t)
  (eve-find-char val)
  (backward-char))

(eve-define-exclusive-motion eve-F
  (setq eve-f-t nil
        eve-f-forward nil)
  (eve-find-char val))

(eve-define-exclusive-motion eve-T
  (setq eve-f-t t
        eve-f-forward nil)
  (eve-find-char val)
  (forward-char))

(defun eve-\; (arg)
  "Repeat `eve-find-char'."
  (interactive "P")
  (let ((eve-in-repeat t))
    (if eve-f-t
        (if eve-f-forward (eve-t arg) (eve-T arg))
      (if eve-f-forward (eve-f arg) (eve-F arg)))))

(defun eve-\, (arg)
  "Repeat `eve-find-char' reverse."
  (interactive "P")
  (let ((eve-f-forward (not eve-f-forward)))
    (eve-\; arg)))

(defvar eve-s-forward nil)

(defvar eve-s-string nil)

(defun eve-search-string (val)
  "Search `eve-s-string' by `eve-s-forward'."
  (unless eve-in-repeat
    (setq eve-s-string
          (read-string (if eve-s-forward "/" "?"))))
  (let ((point (point)))
    (condition-case nil
        (if eve-s-forward
            (progn
              (forward-char)
              (re-search-forward eve-s-string nil nil val)
              (re-search-backward eve-s-string))
          (re-search-backward eve-s-string nil nil val))
      (search-failed
       (goto-char point)
       (error "Search failed")))))

(eve-define-exclusive-motion eve-/
  (setq eve-s-forward t)
  (eve-search-string val))

(eve-define-exclusive-motion eve-?
  (setq eve-s-forward nil)
  (eve-search-string val))

(defun eve-n (arg)
  "Repeat `eve-search-string'."
  (interactive "P")
  (let ((eve-in-repeat t))
    (if eve-s-forward
        (eve-/ arg)
      (eve-? arg))))

(defun eve-N (arg)
  "Repeat `eve-search-string' reverse."
  (interactive "P")
  (let ((eve-s-forward (not eve-s-forward)))
    (eve-n arg)))



(defvar eve-surround-last nil)

(defun eve-read-surround ()
  "Read `eve-surround-last' and return corresponding pair."
  (unless eve-in-repeat
    (setq eve-surround-last (read-char)))
  (or (cdr (assq eve-surround-last '((?b . (?\( . ?\)))
                                     (?B . (?\{ . ?\}))
                                     (?r . (?\[ . ?\]))
                                     (?< . (?\< . ?\>))
                                     (?\` . (?\` . ?\')))))
      `(,eve-surround-last . ,eve-surround-last)) )

(defun eve-surround-region (beg end)
  "Surround region, used by `eve-exec-com' when com is ?s."
  (let ((pair (eve-read-surround)))
    (goto-char end)
    (insert (cdr pair))
    (goto-char beg)
    (insert (car pair))
    (indent-region beg end)))

(defun eve-s (arg)
  "Like vim-surround but target determined by `backward-up-list'."
  (interactive "P")
  (let ((val (eve-getval arg))
        (com (eve-getcom arg)))
    (cond ((eq com ?y)
           (eve-command-arg nil))
          ((eq com ?s)
           (eve-command-arg arg))
          ((eq com ?d)
           (setq eve-d-com '(eve-s nil ?d))
           (save-excursion
             (backward-up-list)
             (delete-pair)))
          ((eq com ?c)
           (setq eve-d-com '(eve-s nil ?c))
           (let ((pair (eve-read-surround)))
             (save-excursion
               (backward-up-list)
               (insert-pair 1 (car pair) (cdr pair))
               (delete-pair))))
          (t
           (delete-char val)
           (eve-change-mode-to-insert)))))



(defun eve-end-with-newline-p (string)
  (or (string-empty-p string)
      (eq (aref string (1- (length string))) ?\n)))

(defun eve-p (arg)
  "Yank after."
  (interactive "P")
  (save-excursion
    (let ((val (eve-getval arg)))
      (setq eve-d-com `(eve-p ,val))
      (if (eve-end-with-newline-p (current-kill 0))
          (eve-loop val
            (end-of-line)
            (if (eobp)
                (newline 1)
              (forward-char))
            (yank))
        (forward-char)
        (eve-loop val
          (yank))))))

(defun eve-P (arg)
  "Yank before."
  (interactive "P")
  (save-excursion
    (let ((val (eve-getval arg)))
      (setq eve-d-com `(eve-P ,val))
      (if (eve-end-with-newline-p (current-kill 0))
          (eve-loop val
            (beginning-of-line)
            (yank))
        (eve-loop val
          (yank))))))

(defun eve-x (arg)
  "Kill after char."
  (interactive "P")
  (let ((val (eve-getval arg)))
    (setq eve-d-com `(eve-x ,val))
    (kill-region (point) (+ (point) val))))

(defun eve-X (arg)
  "Kill before char."
  (interactive "P")
  (let ((val (eve-getval arg)))
    (setq eve-d-com `(eve-X ,val))
    (kill-region (- (point) val) (point))))

(defvar eve-r-last nil)

(defun eve-r (arg)
  "Replace after char."
  (interactive "P")
  (let ((val (eve-getval arg)))
    (setq eve-d-com `(eve-r ,val))
    (unless eve-in-repeat
      (setq eve-r-last (read-char)))
    (delete-char val)
    (eve-loop val
      (insert eve-r-last))))

(provide 'eve)
;;; eve.el ends here
