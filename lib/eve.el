;;; eve.el --- Emacs Vi Emu. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Add this code to your init file:
;; (require 'eve)
;;
;; Or start manually:
;; (setq eve-setup nil)
;; (setq eve-setup-view nil)
;; (global-set-key "\C-z" 'eve-change-mode-to-vi)
;;
;; It's recommended that:
;; (setq view-read-only t)
;; (defalias 'w 'save-buffer)

;;; Code:
(require 'subr-x)

(eval-when-compile
  (require 'view))



(defvar eve-jk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j" "n")
    (define-key map "k" "p")
    (define-key map ":" 'execute-extended-command)
    map))

(defvar eve-insert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j"    'eve-jk)
    (define-key map "\C-r" 'insert-register)
    (define-key map "\M-z" 'eve-change-mode-to-vi)
    (define-key map "\C-z" 'eve-change-mode-to-emacs)
    map)
  "Eve insert mode map.")

(defvar eve-vi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'undefined)
    (define-key map "\C-z" 'eve-change-mode-to-emacs)
    (define-key map "\s"   'scroll-up-command)
    (define-key map "\d"   'scroll-down-command)
    (define-key map (kbd "S-SPC") 'scroll-down-command)

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
    (define-key map "D" "d$")
    (define-key map "Y" "y$")
    (define-key map "J" "j\M-^")
    (define-key map "K" 'kill-sexp)
    (define-key map "H" 'mark-sexp)
    (define-key map "Q" 'indent-pp-sexp)
    (define-key map "R" 'raise-sexp)
    (define-key map "S" 'delete-pair)
    
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

(define-minor-mode eve-jk-mode
  "Eve JK mode."
  :keymap eve-jk-mode-map
  :lighter " <JK>")

(define-minor-mode eve-insert-mode
  "Eve Insert mode."
  :keymap eve-insert-mode-map
  :lighter " <I>")

(define-minor-mode eve-vi-mode
  "Eve Vi mode."
  :keymap eve-vi-mode-map
  :lighter " <V>")

(defvar eve-setup t
  "Wether setup eve.")

(defvar eve-setup-view t
  "Wether setup eve for view mode.")

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

(defvar eve-find-last ?$
  "Last search char of `eve-f'.")

(defvar eve-find-last-2 ?$
  "Last search char of `eve-z'.")

(defvar eve-find-forward nil
  "Last search direction of `eve-f'.")

(defvar eve-find-to nil
  "Last search type of `eve-f'.")

(defvar eve-find-2 nil
  "Last search 2 char or not.")

(defvar eve-jump-forward-chars
  (append '(?f ?j ?g ?h ?a)
          '(?r ?u ?t ?y ?q)
          '(?v ?m ?b ?n ?z)
          '(?4 ?7 ?5 ?6 ?1)
          '(?F ?J ?G ?H ?A)
          '(?R ?U ?T ?Y ?Q)
          '(?V ?M ?B ?N ?Z)
          '(?$ ?& ?% ?^ ?!)
          '(?' ?` ?\[ ?\] ?\\ ?- ?=))
  "Char used by `eve-gj', `eve-gw', `eve-gf'.")

(defvar eve-jump-backward-chars
  (append '(?d ?k ?s ?l ?\;)
          '(?e ?i ?w ?o ?p)
          '(?c ?, ?x ?. ?/)
          '(?3 ?8 ?2 ?9 ?0)
          '(?D ?K ?S ?L ?:)
          '(?E ?I ?W ?O ?P)
          '(?C ?< ?X ?> ??)
          '(?# ?* ?@ ?\( ?\))
          '(?\" ?~ ?\{ ?\} ?| ?_ ?+))
  "Char used by `eve-gj', `eve-gw', `eve-gf'.")

(defvar-local eve-jump-forward-overlays nil
  "Eve jump current overlays.")

(defvar-local eve-jump-backward-overlays nil
  "Eve jump current overlays.")

(defvar eve-replace-last nil
  "Last replace char of `eve-r'.")

(defvar eve-jump-last ?$
  "Last search char of `eve-gf'.")

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

(defvar eve-god-char-alist
  '((?\s . "SPC")
    (tab . "TAB")
    (backspace . "DEL")
    (return . "RET")))

(defvar eve-god-translation-default "C-")

(defvar eve-god-translation-alist
  '(("SPC" . "") ("g" . "M-") ("h" . "C-M-")))



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
             (setq eve-exec-end (if (eolp) (point) (1+ (point))))
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



(defun eve-copy-to-register (beg end)
  "Copy region (BEG . END) to register."
  (copy-to-register (register-read-with-preview "copy to register: ") beg end))

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

(eve-define-operator ?c  'kill-region t)
(eve-define-operator ?d  'kill-region)
(eve-define-operator ?y  'copy-region-as-kill)
(eve-define-operator ?\" 'eve-copy-to-register)
(eve-define-operator ?=  'indent-region)
(eve-define-operator ?-  'narrow-to-region)
(eve-define-operator ?!  'eve-shell-region)
(eve-define-operator ?@  'eve-eval-region)
(eve-define-operator ?#  'comment-or-uncomment-region)
(eve-define-operator ?s  'eve-surround-region t t)

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
  (let ((point (point)))
    (condition-case nil
        (progn
          (forward-char)
          (re-search-forward "\\b\\w" nil nil val)
          (backward-char)
          (skip-chars-forward " \t\n"))
      (search-failed
       (goto-char point)
       (error "Search failed")))))

(eve-define-exclusive-motion "W"
  (let ((point (point)))
    (condition-case nil
        (progn
          (forward-char)
          (re-search-forward "\\_<\\sw" nil nil val)
          (backward-char)
          (skip-chars-forward " \t\n"))
      (search-failed
       (goto-char point)
       (error "Search failed")))))

(eve-define-exclusive-motion "b"
  (backward-word val))

(eve-define-exclusive-motion "B"
  (backward-sexp val))

(eve-define-exclusive-motion "e"
  (unless ope
    (forward-char))
  (forward-word val)
  (unless (or ope (use-region-p))
    (backward-char)))

(eve-define-exclusive-motion "E"
  (forward-sexp val))

(eve-define-inclusive-motion "U"
  (backward-up-list val))

(eve-define-exclusive-motion "L"
  (down-list val))

(eve-define-exclusive-motion "0"
  (beginning-of-line))

(eve-define-exclusive-motion "^"
  (back-to-indentation))

(eve-define-exclusive-motion "$"
  (end-of-line))

(eve-define-line-motion "gg"
  (push-mark)
  (goto-char (point-min)))

(eve-define-line-motion "G"
  (push-mark)
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
  (push-mark)
  (jump-to-register (read-char)))

(eve-define-line-motion "'"
  (push-mark)
  (jump-to-register (read-char)))



(defun eve-tobj (arg)
  "Like vim's inner text object with ARG (nil . ope)."
  (interactive "P")
  (let ((ope (cdr-safe arg)))
    (unless eve-repeat-flag
      (setq eve-tobj-last (read-char)))
    (let ((range (bounds-of-thing-at-point
                  (cdr (assq eve-tobj-last eve-tobj-alist)))))
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
  (setq eve-find-2 nil)
  (let ((point (point)))
    (condition-case nil
        (if eve-find-forward
            (progn
              (forward-char (if eve-find-to 2 1))
              (search-forward (char-to-string eve-find-last) nil nil val)
              (backward-char))
          (when eve-find-to
            (backward-char))
          (search-backward (char-to-string eve-find-last) nil nil val))
      (search-failed
       (goto-char point)
       (error "Search failed")))))

(defun eve-find-char-2 (val)
  "Find VAL `eve-find-last-2' and `eve-find-last-1' by `eve-find-forward'."
  (unless eve-repeat-flag
    (setq eve-find-last-2 (read-char)
          eve-find-last (read-char)))
  (setq eve-find-2 t)
  (let ((point (point)))
    (condition-case nil
        (if eve-find-forward
            (progn
              (forward-char 1)
              (search-forward (format "%c%c" eve-find-last-2 eve-find-last) nil nil val)
              (backward-char 2))
          (search-backward (format "%c%c" eve-find-last-2 eve-find-last) nil nil val))
      (search-failed
       (goto-char point)
       (error "Search failed")))))

(eve-define-inclusive-motion "f"
  (setq eve-find-to nil
        eve-find-forward t)
  (eve-find-char val))

(eve-define-exclusive-motion "F"
  (setq eve-find-to nil
        eve-find-forward nil)
  (eve-find-char val))

(eve-define-inclusive-motion "t"
  (setq eve-find-to t
        eve-find-forward t)
  (eve-find-char val)
  (backward-char))

(eve-define-exclusive-motion "T"
  (setq eve-find-to t
        eve-find-forward nil)
  (eve-find-char val)
  (forward-char))

(eve-define-exclusive-motion "z"
  (setq eve-find-forward t)
  (eve-find-char-2 val))

(eve-define-exclusive-motion "Z"
  (setq eve-find-forward nil)
  (eve-find-char-2 val))

(eve-define-command ";"
  "Repeat `eve-find-char' with ARG."
  (let ((eve-repeat-flag t))
    (cond (eve-find-2
           (if eve-find-forward (eve-z arg) (eve-Z arg)))
          (eve-find-to
           (if eve-find-forward (eve-t arg) (eve-T arg)))
          (t
           (if eve-find-forward (eve-f arg) (eve-F arg))))))

(eve-define-command ","
  "Repeat `eve-find-char' reverse with ARG."
  (let ((eve-find-forward (not eve-find-forward)))
    (eve-\; arg)))

(eve-define-exclusive-motion "/"
  (isearch-forward-regexp)
  (let ((point (point)))
    (condition-case nil
        (re-search-backward isearch-string)
      (search-failed
       (goto-char point)))))

(eve-define-exclusive-motion "?"
  (isearch-backward-regexp))

(defun eve-search-repeat (val)
  (let ((point (point)))
    (condition-case nil
        (if isearch-forward
            (progn
              (forward-char)
              (re-search-forward isearch-string nil nil val)
              (re-search-backward isearch-string))
          (re-search-backward isearch-string nil nil val))
      (search-failed
       (goto-char point)
       (error "Search failed")))))

(eve-define-exclusive-motion "n"
  (eve-search-repeat val))

(eve-define-exclusive-motion "N"
  (let ((isearch-forward (not isearch-forward)))
    (eve-search-repeat val)))



(defun eve-jump-make-overlay (pos)
  "Make an overlay at POS limited in current window."
  (let ((overlay (make-overlay pos (1+ pos))))
    (overlay-put overlay 'window (selected-window))
    overlay))

(defun eve-jump-put-char (chars overlays)
  "Put CHARS on overlay in OVERLAYS."
  (let ((fin (car chars))
        (chars (cdr chars))
        overlay char)
    (while overlays
      (setq overlay (car overlays)
            overlays (cdr overlays))
      (if chars
          (setq char (car chars)
                chars (cdr chars))
        (setq char fin))
      (overlay-put overlay
                   (if (string-equal
                        (buffer-substring
                         (overlay-start overlay)
                         (overlay-end overlay))
                        "\n")
                       'before-string
                     'display)
                   (propertize (char-to-string char) 'face 'match))
      (overlay-put overlay 'char char))))

(defun eve-jump-narrow (char overlays)
  "Narrow OVERLAYS with CHAR."
  (let (overlay noverlays)
    (while overlays
      (setq overlay (car overlays)
            overlays (cdr overlays))
      (if (eq (overlay-get overlay 'char) char)
          (setq noverlays (cons overlay noverlays))
        (delete-overlay overlay)))
    (nreverse noverlays)))

(defun eve-jump-do-jump ()
  "Repeat do jump until find single pos or error."
  (cond ((not (or eve-jump-forward-overlays
                  eve-jump-backward-overlays))
         (error "No such char"))
        ((and (not eve-jump-backward-overlays)
              (car eve-jump-forward-overlays)
              (not (cdr eve-jump-forward-overlays)))
         (goto-char (overlay-start (car eve-jump-forward-overlays)))
         (delete-overlay (car eve-jump-forward-overlays))
         (setq eve-jump-forward-overlays nil))
        ((and (not eve-jump-forward-overlays)
              (car eve-jump-backward-overlays)
              (not (cdr eve-jump-backward-overlays)))
         (push-mark)
         (goto-char (overlay-start (car eve-jump-backward-overlays)))
         (delete-overlay (car eve-jump-backward-overlays))
         (setq eve-jump-backward-overlays nil))
        (t
         (let ((char (read-char)))
           (setq eve-jump-forward-overlays
                 (eve-jump-narrow char eve-jump-forward-overlays)
                 eve-jump-backward-overlays
                 (eve-jump-narrow char eve-jump-backward-overlays)))
         (eve-jump-put-char eve-jump-forward-chars eve-jump-forward-overlays)
         (eve-jump-put-char eve-jump-backward-chars eve-jump-backward-overlays)
         (eve-jump-do-jump))))

(defun eve-jump-goto-regexp (regexp)
  "Setup overlays by REGEXP and then do jump."
  (let ((beg (window-start))
        (end (window-end)))
    (save-excursion
      (condition-case nil
          (let ((ctn (not (eobp))))
            (while ctn
              (forward-char)
              (re-search-forward regexp)
              (re-search-backward regexp)
              (if (< (point) end)
                  (setq eve-jump-forward-overlays
                        (cons (eve-jump-make-overlay (point))
                              eve-jump-forward-overlays))
                (setq ctn nil))))
        (search-failed nil)))
    (save-excursion
      (condition-case nil
          (let ((ctn t))
            (while ctn
              (re-search-backward regexp)
              (if (>= (point) beg)
                  (setq eve-jump-backward-overlays
                        (cons (eve-jump-make-overlay (point))
                              eve-jump-backward-overlays))
                (setq ctn nil))))
        (search-failed nil)))
    (setq eve-jump-forward-overlays (nreverse eve-jump-forward-overlays)
          eve-jump-backward-overlays (nreverse eve-jump-backward-overlays))
    (eve-jump-put-char eve-jump-forward-chars eve-jump-forward-overlays)
    (eve-jump-put-char eve-jump-backward-chars eve-jump-backward-overlays)
    (when (or eve-jump-forward-overlays
              eve-jump-backward-overlays)
      (unwind-protect
          (eve-jump-do-jump)
        (mapc 'delete-overlay eve-jump-forward-overlays)
        (mapc 'delete-overlay eve-jump-backward-overlays)
        (setq eve-jump-forward-overlays nil
              eve-jump-backward-overlays nil)))))

(eve-define-exclusive-motion "gf"
  (unless eve-repeat-flag
    (setq eve-jump-last (read-char)))
  (eve-jump-goto-regexp (regexp-quote (char-to-string eve-jump-last))))

(eve-define-exclusive-motion "gw"
  (eve-jump-goto-regexp "\\_<\\sw"))

(eve-define-inclusive-motion "ge"
  (eve-jump-goto-regexp "\\sw\\_>"))

(eve-define-line-motion "gj"
  (eve-jump-goto-regexp "^.\\|^\n"))

(eve-define-exclusive-motion "g/"
  (eve-jump-goto-regexp isearch-string))



(eve-define-command "."
  "Repeat by `eve-exec-last' ARG times."
  (dotimes (_ (or (car-safe arg) 1))
    (let ((eve-repeat-flag t)
          (move (nth 0 eve-exec-last))
          (val (nth 1 eve-exec-last))
          (ope (nth 2 eve-exec-last)))
      (if move
          (funcall move `(,val . ,ope))
        (error "No previous command to repeat")))))

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
  "Yank after ARG times."
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
  "Yank before ARG times."
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



(defun eve-god-char-to-string (char)
  (or (cdr (assq char eve-god-char-alist))
      (char-to-string char)))

(defun eve-god-lookup-key (&optional key prev-key)
  (let* ((key (eve-god-char-to-string
               (or key (read-event prev-key))))
         (trans (cdr (assoc key eve-god-translation-alist)))
         (keys (if trans
                   (let* ((prev-key (concat prev-key " " trans))
                          (key (eve-god-char-to-string (read-event prev-key))))
                     (concat prev-key key))
                 (concat prev-key " " eve-god-translation-default key)))
         (seq (read-kbd-macro keys t))
         (binding (key-binding seq)))
    (cond ((commandp binding)
           (setq last-command-event (aref seq (1- (length seq))))
           binding)
          ((keymapp binding)
           (eve-god-lookup-key (read-event keys) keys))
          (t
           (error "God: unknown key binding for `%s'" keys)))))

(eve-define-command "go"
  "God mode."
  (let ((binding (eve-god-lookup-key)))
    (setq prefix-arg `(,arg)
          this-command binding
          real-this-command binding)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))



(defun eve-setup ()
  "Eve setup."
  (cond ((derived-mode-p 'special-mode 'compilation-mode 'dired-mode)
         (eve-jk-mode 1))
        ((derived-mode-p 'prog-mode 'text-mode 'fundamental-mode)
         (eve-change-mode-to-vi))))

(defun eve-setup-view ()
  "Eve setup for view mode."
  (if view-mode
      (when (or eve-vi-mode eve-insert-mode)
        (eve-change-mode-to-emacs))
    (eve-setup)))

(when eve-setup
  (global-set-key "\C-z" 'eve-change-mode-to-vi)
  (define-key special-mode-map "n" 'next-line)
  (define-key special-mode-map "p" 'previous-line)
  (add-hook 'after-change-major-mode-hook 'eve-setup))

(when eve-setup-view
  (with-eval-after-load 'view
    (define-key view-mode-map "g" nil)
    (dolist (key '("_" "j" "k" "h" "l" "w" "W" "b" "B" "e" "E" "U" "L"
                   "0" "^" "$" "gg" "G" "{" "}" "[" "]" "(" ")" "`" "'"
                   "f" "F" "t" "T" "z" "Z" ";" "," "/" "?" "n" "N"
                   "gf" "gw" "ge" "gj" "g/" "go"))
      (define-key view-mode-map key (intern (concat "eve-" key))))
    (define-key view-mode-map "y"  'eve-operator)
    (define-key view-mode-map "\"" 'eve-operator)
    (define-key view-mode-map "-"  'eve-operator)
    (define-key view-mode-map "i"  'eve-tobj)
    (define-key view-mode-map "a"  'eve-tobj)
    (define-key view-mode-map "."  'repeat)
    (define-key view-mode-map "m"  'point-to-register)
    (define-key view-mode-map ":"  'execute-extended-command)
    (define-key view-mode-map "v"  'set-mark-command)
    (define-key view-mode-map "V"  "0vj")
    (define-key view-mode-map "gt" 'tab-next)
    (define-key view-mode-map "gT" 'tab-previous)
    (define-key view-mode-map "gn" "\C-c\C-n")
    (define-key view-mode-map "gp" "\C-c\C-p")
    (add-hook 'view-mode-hook 'eve-setup-view)))

(provide 'eve)
;;; eve.el ends here
