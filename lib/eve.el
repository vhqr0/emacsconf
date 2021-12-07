;;; eve.el --- Emacs Vi Emu. -*- lexical-binding: t -*-

;;; Commentary:
;; Add this code to your init file:
;; (global-set-key (kbd "C-z") 'eve-change-mode-to-vi)

;;; Code:
(require 'subr-x)



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

    (define-key map "1" 'eve-digit)
    (define-key map "2" 'eve-digit)
    (define-key map "3" 'eve-digit)
    (define-key map "4" 'eve-digit)
    (define-key map "5" 'eve-digit)
    (define-key map "6" 'eve-digit)
    (define-key map "7" 'eve-digit)
    (define-key map "8" 'eve-digit)
    (define-key map "9" 'eve-digit)

    (define-key map "C" "c$")
    (define-key map "Y" "y$")
    (define-key map "D" "d$")
    (define-key map "J" "j\M-^")
    (define-key map "K" 'kill-sexp)
    (define-key map "H" 'backward-kill-sexp)
    (define-key map "Q" 'indent-pp-sexp)
    (define-key map "S" 'delete-pair)
    (define-key map "R" 'raise-sexp)
    
    (define-key map "u" 'undo)
    (define-key map "m" 'point-to-register)
    (define-key map ":" 'execute-extended-command)
    (define-key map "v" 'set-mark-command)
    (define-key map "V" "0vj")

    (define-key map "gt" 'tab-next)
    (define-key map "gT" 'tab-previous)
    (define-key map "gn" "\C-c\C-n")
    (define-key map "gp" "\C-c\C-p")
    
    map)
  "Eve vi mode map.")

(define-minor-mode eve-insert-mode
  "Eve insert mode."
  :keymap eve-insert-mode-map
  :lighter " <I>")

(define-minor-mode eve-vi-mode
  "Eve vi mode."
  :keymap eve-vi-mode-map
  :lighter " <V>")

(defvar-local eve-exec-last nil
  "Last (move val ope) used by `eve-.'.")

(defvar-local eve-exec-save-point nil
  "Save point before `eve-exec'.")

(defvar-local eve-exec-beg 0
  "`eve-exec' operator region begin.")

(defvar-local eve-exec-end 0
  "`eve-exec' operator region end.")

(defvar eve-repeat-flag nil
  "Non-nil means in `eve-.', use last record.")

(defvar eve-operator-alist nil
  "Alist of operator char and associated func.")

(defvar eve-operator-translate-alist nil
  "Alist of translated operator and associated operator char.")

(defvar eve-operator-inline-alist nil
  "Alist of operator char which operator operate on inner range.")

(defvar eve-find-last nil
  "Last search char of `eve-f'.")

(defvar eve-find-forward nil
  "Last search direction of `eve-f'.")

(defvar eve-find-to nil
  "Last search type of `eve-f'.")

(defvar eve-search-last nil
  "Last search string of `eve-/'.")

(defvar eve-search-forward nil
  "Last search direction of `eve-/'.")

(defvar eve-replace-last nil
  "Last replace char of `eve-r'.")

(defvar eve-tobj-alist
  '((?w . word)
    (?W . sexp)
    (?f . defun)
    (?p . paragraph)
    (?P . page)
    (?h . buffer)
    (?b . pair))
  "Alist of text object.")

(defvar eve-tobj-last nil
  "Last text object.")

(defvar eve-surround-alist
  '((?b . (?\( . ?\)))
    (?B . (?\{ . ?\}))
    (?r . (?\[ . ?\]))
    (?< . (?\< . ?\>))
    (?\` . (?\` . ?\')))
  "Alist of surrounding used by `eve-s'.")

(defvar eve-surround-last nil
  "Last surrounding of `eve-s'.")

(defvar eve-surround-last-tag nil
  "Last tag surrounding of `eve-s'.")

(defvar eve-shell-last nil
  "Last shell command of `eve-!'.")

(defvar eve-eval-alist
  '((emacs-lisp-mode . eval-region)
    (lisp-interaction-mode . eval-region)
    (python-mode . python-shell-send-region))
  "Alist of (major-mode . eval-region-function), used by `eve-eval-region'.")



(defun eve-change-mode (&optional new-mode)
  "Change mode to NEW-MODE.
NEW-MODE is Vi, Insert or Emacs by default."
  (interactive)
  (eve-vi-mode (if (eq new-mode 'vi) 1 -1))
  (eve-insert-mode (if (eq new-mode 'insert) 1 -1)))

;;;###autoload
(defun eve-change-mode-to-vi ()
  "Change mode to Vi."
  (interactive)
  (eve-change-mode 'vi))

(defun eve-change-mode-to-insert ()
  "Change mode to Insert."
  (interactive)
  (eve-change-mode 'insert))

(defalias 'eve-change-mode-to-emacs 'eve-change-mode)

(defun eve-jk ()
  ":imap jk <esc>."
  (interactive)
  (if (and (not executing-kbd-macro)
           (not defining-kbd-macro)
           (not (sit-for 0.1 'no-redisplay)))
      (let ((next-char (read-event)))
        (if (eq next-char ?k)
            (progn
              (eve-change-mode-to-vi)
              (unless (bolp) (left-char)))
          (insert ?j)
          (push next-char unread-command-events)))
    (insert ?j)))



(defun eve-digit (arg)
  "Digit entry, setup ARG."
  (interactive "P")
  (let ((char last-command-event)
        (val (or (car-safe arg) 0))
        (ope (cdr-safe arg)))
    (while (<= ?0 char ?9)
      (setq val (+ (* 10 val) (- char ?0))
            char (read-char)))
    (setq prefix-arg `(,val . ,ope))
    (push char unread-command-events)))

(defun eve-operator (arg)
  "Operator entry, setup ARG."
  (interactive "P")
  (let ((char last-command-event)
        (val (car-safe arg))
        (ope (cdr-safe arg)))
    (cond ((region-active-p)
           (eve-exec nil nil char))
          (ope
           (if (or (eq char ope)
                   (eq char (cdr (assq ope eve-operator-translate-alist))))
               (eve-line `(,val . ,ope))
             (error "Wrong operation")))
          (t
           (setq prefix-arg `(,val . ,char))))))

(defun eve-exec (move val ope)
  "Call function by OPE and set `eve-exec-last' (MOVE VAL OPE) for `eve-.'."
  (let ((func (cdr (assq ope eve-operator-alist))))
    (when func
      (when (region-active-p)
        (setq eve-exec-beg (mark)
              eve-exec-end (point)))
      (unless (<= eve-exec-beg eve-exec-end)
        (let (tmp)
          (setq tmp eve-exec-beg
                eve-exec-beg eve-exec-end
                eve-exec-end tmp)))
      (funcall func eve-exec-beg eve-exec-end)
      (deactivate-mark)
      (when (eq ope ?c)
        (eve-change-mode-to-insert))
      (when move
        (setq eve-exec-last `(,move ,val ,ope))))))

(defun eve-define-operator (key func &optional inline nobind)
  "Regist an operator KEY associated to FUNC.
INLINE means operator in inner line motion.
NOBIND means dont bind key."
  (unless nobind
    (define-key eve-vi-mode-map (char-to-string key) 'eve-operator))
  (add-to-list 'eve-operator-alist `(,key . ,func))
  (when inline
    (add-to-list 'eve-operator-inline-alist key)))

(defmacro eve-define-translate (before after)
  "Regist an operator translate AFTER associated to BEFORE.
E.G.Translate gc to # so gcc can comment a line."
  (let* ((key (format "g%c" after))
         (func (intern (concat "eve-" key))))
    `(progn
       (add-to-list 'eve-operator-translate-alist '(,before . ,after))
       (define-key eve-vi-mode-map ,key ',func)
       (defun ,func (arg)
         (interactive "P")
         (let ((last-command-event ,before))
           (eve-operator arg))))))

(defmacro eve-define-exclusive-motion (key &rest body)
  "Regist a exclusive motion KEY with BODY."
  (declare (indent 1))
  (let ((func (intern (concat "eve-" key))))
    `(progn
       (define-key eve-vi-mode-map ,key ',func)
       (defun ,func (arg)
         "Generated by `eve-define-exclusive-motion'."
         (interactive "P")
         (let ((val (or (car-safe arg) 1))
               (ope (cdr-safe arg)))
           (when ope
             (unless eve-exec-save-point
               (setq eve-exec-save-point (make-marker)))
             (move-marker eve-exec-save-point (point))
             (setq eve-exec-beg (point)))
           ,@body
           (when ope
             (setq eve-exec-end (point))
             (eve-exec ',func val ope)
             (goto-char eve-exec-save-point)))))))

(defmacro eve-define-inclusive-motion (key &rest body)
  "Regist an inclusive motion KEY with BODY."
  (declare (indent 1))
  (let ((func (intern (concat "eve-" key))))
    `(progn
       (define-key eve-vi-mode-map ,key ',func)
       (defun ,func (arg)
         "Generated by `eve-define-inclusive-motion'."
         (interactive "P")
         (let ((val (or (car-safe arg) 1))
               (ope (cdr-safe arg)))
           (when ope
             (unless eve-exec-save-point
               (setq eve-exec-save-point (make-marker)))
             (move-marker eve-exec-save-point (point))
             (setq eve-exec-beg (point)))
           ,@body
           (when ope
             (setq eve-exec-end (point))
             (unless (eq eve-exec-end (line-end-position))
               (setq eve-exec-end (1+ eve-exec-end)))
             (eve-exec ',func val ope)
             (goto-char eve-exec-save-point)))))))

(defmacro eve-define-line-motion (key &rest body)
  "Regist a line motion KEY with BODY."
  (declare (indent 1))
  (let ((func (intern (concat "eve-" key))))
    `(progn
       (define-key eve-vi-mode-map ,key ',func)
       (defun ,func (arg)
         "Generated by `eve-define-line-motion'."
         (interactive "P")
         (let* ((val (or (car-safe arg) 1))
                (ope (cdr-safe arg))
                (inline (and ope (memq ope eve-operator-inline-alist))))
           (when ope
             (unless eve-exec-save-point
               (setq eve-exec-save-point (make-marker)))
             (move-marker eve-exec-save-point (point))
             (setq eve-exec-beg (if inline
                                    (save-excursion
                                      (back-to-indentation)
                                      (point))
                                  (line-beginning-position))))
           ,@body
           (when ope
             (setq eve-exec-end (if inline
                                    (line-end-position)
                                  (min (1+ (line-end-position)) (point-max))))
             (eve-exec ',func val ope)
             (goto-char eve-exec-save-point)))))))

(defmacro eve-define-command (key docstring &rest body)
  "Define a command KEY with DOCSTRING and BODY."
  (declare (indent 1))
  (let ((func (intern (concat "eve-" key))))
    `(progn
       (define-key eve-vi-mode-map ,key ',func)
       (defun ,func (arg)
         ,docstring
         (interactive "P")
         ,@body))))

(defmacro eve-define-insert (key &rest body)
  "KEY: Eval BODY and change to Insert mode.
Dispatch to `eve-tobj' when there is a ope."
  (declare (indent 1))
  (let ((func (intern (concat "eve-" key))))
    `(progn
       (define-key eve-vi-mode-map ,key ',func)
       (defun ,func (arg)
         "Generated by `eve-define-insert'."
         (interactive "P")
         (let ((ope (cdr-safe arg)))
           (cond ((region-active-p)
                  (eve-tobj nil))
                 (ope (eve-tobj arg))
                 (t
                  ,@body
                  (eve-change-mode-to-insert))))))))



(defun eve-eval-region (beg end)
  "Eval region (BEG . END) by `major-mode'."
  (let ((func (cdr (assq major-mode eve-eval-alist))))
    (when func
      (funcall func beg end))))

(defun eve-shell-region (beg end)
  "Run shell command on region (BEG . END) and replace."
  (unless eve-repeat-flag
    (setq eve-shell-last (read-shell-command "shell command: ")))
  (shell-command-on-region beg end eve-shell-last nil t))

(defun eve-read-surround ()
  "Read `eve-surround-last' and return corresponding pair."
  (unless eve-repeat-flag
    (setq eve-surround-last (read-char)))
  (or (cdr (assq eve-surround-last eve-surround-alist))
      (when (eq eve-surround-last ?t)
        (unless eve-repeat-flag
          (setq eve-surround-last-tag (read-string "<")))
        `(,(concat "<" eve-surround-last-tag ">") .
          ,(concat "</" eve-surround-last-tag ">")))
      `(,eve-surround-last . ,eve-surround-last)))

(defun eve-surround-region (beg end)
  "Surround region (BEG . END), used by `eve-exec' when ope is ?s."
  (let ((pair (eve-read-surround)))
    (goto-char end)
    (insert (cdr pair))
    (goto-char beg)
    (insert (car pair))
    (indent-region beg end)))

(eve-define-operator ?c 'kill-region t)
(eve-define-operator ?d 'kill-region)
(eve-define-operator ?y 'copy-region-as-kill)
(eve-define-operator ?= 'indent-region)
(eve-define-operator ?- 'narrow-to-region)
(eve-define-operator ?! 'eve-shell-region)
(eve-define-operator ?@ 'eve-eval-region)
(eve-define-operator ?# 'comment-or-uncomment-region)
(eve-define-operator ?s 'eve-surround-region t t)

(eve-define-translate ?# ?c)
(eve-define-translate ?@ ?y)



(eve-define-line-motion "_"
  (line-move (1- val) t))

(defalias 'eve-line 'eve-_)

(eve-define-line-motion "j"
  (setq this-command 'next-line)
  (line-move val t))

(eve-define-line-motion "k"
  (setq this-command 'previous-line)
  (line-move (- val) t))

(eve-define-exclusive-motion "h"
  (backward-char val))

(eve-define-exclusive-motion "l"
  (forward-char val))

(eve-define-exclusive-motion "w"
  (forward-word val)
  (skip-chars-forward " \t\n"))

(eve-define-exclusive-motion "W"
  (forward-sexp val)
  (skip-chars-forward " \t\n"))

(eve-define-inclusive-motion "U"
  (backward-up-list val))

(eve-define-exclusive-motion "b"
  (backward-word val))

(eve-define-exclusive-motion "B"
  (backward-sexp val))

(eve-define-exclusive-motion "e"
  (unless ope
    (forward-char))
  (forward-word val)
  (unless ope
    (backward-char)))

(eve-define-exclusive-motion "E"
  (forward-sexp val))

(eve-define-exclusive-motion "0"
  (beginning-of-line))

(eve-define-exclusive-motion "^"
  (back-to-indentation))

(eve-define-exclusive-motion "$"
  (end-of-line))

(eve-define-line-motion "gg"
  (goto-char (point-min)))

(eve-define-line-motion "G"
  (goto-char (point-max)))

(eve-define-line-motion "{"
  (backward-paragraph val))

(eve-define-line-motion "}"
  (forward-paragraph val))

(eve-define-line-motion "["
  (backward-page val))

(eve-define-line-motion "]"
  (forward-page val))

(eve-define-line-motion "("
  (beginning-of-defun val))

(eve-define-line-motion ")"
  (end-of-defun val))

(eve-define-exclusive-motion "`"
  (jump-to-register (read-char)))

(eve-define-line-motion "'"
  (jump-to-register (read-char)))



(defun eve-tobj (arg)
  "Like vim's inner text object with ARG (nil . ope)."
  (interactive "P")
  (let ((ope (cdr-safe arg)))
    (unless eve-repeat-flag
      (setq eve-tobj-last (read-char)))
    (let (range)
      (setq range (cdr (assq eve-tobj-last
                             eve-tobj-alist)))
      (cond ((eq range 'pair)
             (setq range
                   (save-excursion
                     (backward-up-list)
                     (bounds-of-thing-at-point 'sexp))))
            (range
             (setq range (bounds-of-thing-at-point range))))
      (when range
        (if (region-active-p)
            (progn
              (set-mark (car range))
              (goto-char (cdr range)))
          (unless eve-exec-save-point
            (setq eve-exec-save-point (make-marker)))
          (move-marker eve-exec-save-point (point))
          (setq eve-exec-beg (car range)
                eve-exec-end (cdr range))
          (eve-exec 'eve-tobj nil ope)
          (goto-char eve-exec-save-point))))))

(eve-define-insert "i")

(eve-define-insert "a"
  (unless (eolp) (forward-char)))

(eve-define-insert "I"
  (back-to-indentation))

(eve-define-insert "A"
  (end-of-line))

(eve-define-insert "o"
  (end-of-line)
  (newline 1)
  (indent-according-to-mode))

(eve-define-insert "O"
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))



(defun eve-find-char (val)
  "Find VAL `eve-find-last' by `eve-find-forward'."
  (unless eve-repeat-flag
    (setq eve-find-last (read-char)))
  (let ((point (point)))
    (condition-case nil
        (if eve-find-forward
            (progn
              (forward-char (if eve-find-to 2 1))
              (search-forward (string eve-find-last) nil nil val)
              (backward-char))
          (when eve-find-to
            (backward-char))
          (search-backward (string eve-find-last) nil nil val))
      (search-failed
       (goto-char point)
       (error "Search failed")))))

(eve-define-inclusive-motion "f"
  (setq eve-find-to nil
        eve-find-forward t)
  (eve-find-char val))

(eve-define-inclusive-motion "t"
  (setq eve-find-to t
        eve-find-forward t)
  (eve-find-char val)
  (backward-char))

(eve-define-exclusive-motion "F"
  (setq eve-find-to nil
        eve-find-forward nil)
  (eve-find-char val))

(eve-define-exclusive-motion "T"
  (setq eve-find-to t
        eve-find-forward nil)
  (eve-find-char val)
  (forward-char))

(eve-define-command ";"
  "Repeat `eve-find-char' with ARG."
  (let ((eve-repeat-flag t))
    (if eve-find-to
        (if eve-find-forward (eve-t arg) (eve-T arg))
      (if eve-find-forward (eve-f arg) (eve-F arg)))))

(eve-define-command ","
  "Repeat `eve-find-char' reverse with ARG."
  (let ((eve-find-forward (not eve-find-forward)))
    (eve-\; arg)))

(defun eve-search-string (val)
  "Search VAL `eve-search-last' by `eve-search-forward'."
  (unless eve-repeat-flag
    (setq eve-search-last
          (read-string (if eve-search-forward "/" "?"))))
  (let ((point (point)))
    (condition-case nil
        (if eve-search-forward
            (progn
              (forward-char)
              (re-search-forward eve-search-last nil nil val)
              (re-search-backward eve-search-last))
          (re-search-backward eve-search-last nil nil val))
      (search-failed
       (goto-char point)
       (error "Search failed")))))

(eve-define-exclusive-motion "/"
  (setq eve-search-forward t)
  (eve-search-string val))

(eve-define-exclusive-motion "?"
  (setq eve-search-forward nil)
  (eve-search-string val))

(eve-define-command "n"
  "Repeat `eve-search-string' with ARG."
  (let ((eve-repeat-flag t))
    (if eve-search-forward
        (eve-/ arg)
      (eve-? arg))))

(eve-define-command "N"
  "Repeat `eve-search-string' reverse with ARG."
  (let ((eve-search-forward (not eve-search-forward)))
    (eve-n arg)))



(eve-define-command "."
  "Repeat by `eve-exec-last' ARG times."
  (dotimes (_ (or (car-safe arg) 1))
    (let ((eve-repeat-flag t)
          (move (nth 0 eve-exec-last))
          (val (nth 1 eve-exec-last))
          (ope (nth 2 eve-exec-last)))
      (if move
          (funcall move `(,val . ,ope))
        (error "No previous commad to repeat")))))

(eve-define-command "s"
  "Like vim-surround but target determined by `backward-up-list'.
ARG: (val . ope), dispatched by ope."
  (let ((val (or (car-safe arg) 1))
        (ope (cdr-safe arg)))
    (cond ((or (region-active-p)
               (eq ope ?y))
           (eve-operator nil))
          ((eq ope ?s)
           (eve-operator arg))
          ((eq ope ?d)
           (setq eve-exec-last '(eve-s nil ?d))
           (save-excursion
             (backward-up-list)
             (delete-pair)))
          ((eq ope ?c)
           (setq eve-exec-last '(eve-s nil ?c))
           (let ((pair (eve-read-surround)))
             (save-excursion
               (backward-up-list)
               (insert-pair 1 (car pair) (cdr pair))
               (delete-pair))))
          (t
           (delete-char val)
           (eve-change-mode-to-insert)))))

(defun eve-end-with-newline-p (string)
  "Whether STRING end with a ?\n."
  (or (string-empty-p string)
      (eq (aref string (1- (length string))) ?\n)))

(eve-define-command "p"
  "Yank after ARG char."
  (when (region-active-p)
    (delete-region (mark) (point)))
  (save-excursion
    (let ((val (or (car-safe arg) 1)))
      (setq eve-exec-last `(eve-p ,val))
      (if (eve-end-with-newline-p (current-kill 0))
          (dotimes (_ val)
            (end-of-line)
            (if (eobp)
                (newline 1)
              (forward-char))
            (yank))
        (forward-char)
        (dotimes (_ val)
          (yank))))))

(eve-define-command "P"
  "Yank before ARG char."
  (when (region-active-p)
    (delete-region (mark) (point)))
  (save-excursion
    (let ((val (or (car-safe arg) 1)))
      (setq eve-exec-last `(eve-P ,val))
      (if (eve-end-with-newline-p (current-kill 0))
          (dotimes (_ val)
            (beginning-of-line)
            (yank))
        (dotimes (_ val)
          (yank))))))

(eve-define-command "x"
  "Kill after ARG char."
  (if (region-active-p)
      (delete-region (mark) (point))
    (let ((val (or (car-safe arg) 1)))
      (setq eve-exec-last `(eve-x ,val))
      (kill-region (point) (+ (point) val)))))

(eve-define-command "X"
  "Kill before ARG char."
  (if (region-active-p)
      (delete-region (mark) (point))
    (let ((val (or (car-safe arg) 1)))
      (setq eve-exec-last `(eve-X ,val))
      (kill-region (- (point) val) (point)))))

(eve-define-command "~"
  "Inverse case after ARG char."
  (if (region-active-p)
      (progn
        (unless (<= (point) (mark))
          (exchange-point-and-mark))
        (deactivate-mark)
        (eve-~ `(,(- (mark) (point)))))
    (let ((val (or (car-safe arg) 1)))
      (setq eve-exec-last `(eve-~ ,val))
      (dotimes (_ val)
        (let ((char (following-char)))
          (delete-char 1)
          (insert (if (<= ?A char ?Z)
                      (downcase char)
                    (upcase char))))))))

(eve-define-command "r"
  "Replace after ARG char."
  (if (region-active-p)
      (progn
        (unless (<= (point) (mark))
          (exchange-point-and-mark))
        (deactivate-mark)
        (eve-r `(,(- (mark) (point)))))
    (let ((val (or (car-safe arg) 1)))
      (setq eve-exec-last `(eve-r ,val))
      (unless eve-repeat-flag
        (setq eve-replace-last (read-char)))
      (delete-char val)
      (dotimes (_ val)
        (insert eve-replace-last)))))

(provide 'eve)
;;; eve.el ends here
