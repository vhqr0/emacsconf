(require 'evil)
(require 'cl-lib)



;;; args

(defvar evil-args-openers '("(" "{" "["))
(defvar evil-args-closers '(")" "}" "]"))
(defvar evil-args-delimiters '("," ";"))

(defun evil-args--backward-delimiter (&optional count)
  (let ((openers-regexp (regexp-opt evil-args-openers))
        (closers-regexp (regexp-opt evil-args-closers))
        (delimiters-regexp (regexp-opt evil-args-delimiters))
        (all-regexp (regexp-opt (append evil-args-openers
                                        evil-args-closers
                                        evil-args-delimiters)))
        (begin -1)
        (count (or count 1)))
    (save-excursion
      (while (and (< begin 0)
                  (> count 0))
        ;; search backwards for delimiter, opener, or closer
        (if (not (re-search-backward all-regexp nil t))
            ;; not found
            (setq begin (- (point-at-bol) 1))
          ;; found:
          ;; skip over any matching pairs if necessary
          (while (looking-at-p closers-regexp)
            (evil-jump-item)
            (backward-char))
          ;; if looking at an opener, stop
          (if (looking-at-p openers-regexp)
              (setq begin (point))
            ;; looking at a delimiter: decrement count and check if done
            (when (and (looking-at-p delimiters-regexp)
                       (<= (setq count (- count 1)) 0))
              (setq begin (point)))))))
    (if begin (goto-char begin))))

(defun evil-args--forward-delimiter (&optional count)
  (let ((openers-regexp (regexp-opt evil-args-openers))
        (closers-regexp (regexp-opt evil-args-closers))
        (delimiters-regexp (regexp-opt evil-args-delimiters))
        (all-regexp (regexp-opt (append evil-args-openers
                                        evil-args-closers
                                        evil-args-delimiters)))
        (end -1)
        (count (or count 1)))
    (save-excursion
      (while (and (< end 0)
                  (> count 0))
        ;; search forward for a delimiter, opener, or closer
        (if (not (re-search-forward all-regexp nil t))
            ;; not found
            (setq end (point-at-eol))
          ;; found:
          ;; skip over any matching pairs if necessary
          (backward-char)
          (while (looking-at-p openers-regexp)
            (evil-jump-item)
            (forward-char))
          ;; if looking at a closer, stop
          (if (looking-at-p closers-regexp)
              (setq end (point))
            ;; looking at a delimiter: decrement count and check if done
            (when (looking-at-p delimiters-regexp)
              (if (<= (setq count (- count 1)) 0)
                  (setq end (point))
                (forward-char)))))))
    (if end (goto-char end))))


(defun evil-args--backward-arg-no-skip (count)
  (evil-args--backward-delimiter (or count 1))
  (forward-char)
  (when (looking-at " ")
    (forward-char))
  (when (looking-at "\n")
    (evil-next-line)
    (evil-first-non-blank)))

(defun evil-backward-arg (count)
  "Move the cursor backward COUNT arguments."
  (interactive "p")
  (let ((delimiters-regexp (regexp-opt evil-args-delimiters)))
    (evil-args--backward-arg-no-skip
     (+ (if (looking-back (concat delimiters-regexp
                                  "[\t\n ]*")
                          nil)
            1
          0)
        (or count 1)))))

(defun evil-forward-arg (count)
  "Move the cursor forward COUNT arguments."
  (interactive "p")
  (let ((closers-regexp (regexp-opt evil-args-closers)))
    (evil-args--forward-delimiter (or count 1))
    (when (not (looking-at-p closers-regexp))
      (forward-char)
      (when (looking-at " ")
        (forward-char))
      (when (looking-at "\n")
        (evil-next-line)
        (evil-first-non-blank)))))

(evil-define-text-object evil-inner-arg (count &optional beg end type)
  "Select inner delimited argument."
  (let ((begin (save-excursion (evil-args--backward-arg-no-skip 1) (point)))
        (end (save-excursion (evil-args--forward-delimiter count) (point))))
    (evil-range begin end)))

(evil-define-text-object evil-outer-arg (count &optional beg end type)
  "Select a delimited argument."
  (let ((openers-regexp (regexp-opt evil-args-openers))
        (begin nil)
        (end-on-closer nil)
        (end nil))
    (save-excursion
      (evil-args--forward-delimiter count)
      (if (member (string (char-after)) evil-args-delimiters)
          (evil-forward-arg 1)
        (setq end-on-closer t))
      (setq end (point)))
    (save-excursion
      (evil-args--backward-arg-no-skip 1)
      (if (and end-on-closer
               (not (looking-back (concat openers-regexp
                                          "[\t\n ]*")
                                  nil)))
          (progn (evil-args--backward-delimiter)
                 (setq begin (point)))
        (setq begin (point))))
    (evil-range begin end)))

(defun evil-jump-out-args (count)
  "Move the cursor out of the nearest enclosing matching pairs."
  (interactive "p")
  (setq count (or count 1))
  (let ((openers-regexp (regexp-opt evil-args-openers))
        (closers-regexp (regexp-opt evil-args-closers))
        (delimiters-regexp (regexp-opt evil-args-delimiters))
        (all-regexp (regexp-opt (append evil-args-openers
                                        evil-args-closers
                                        evil-args-delimiters))))
    (while (> count 0)
      (let ((begin -1)
            (success nil))
        (save-excursion
          (while (< begin 0)
            (if (not (re-search-backward all-regexp nil t))
                (setq begin (- (point-at-bol) 1))
              (while (looking-at-p closers-regexp)
                (evil-jump-item)
                (backward-char))
              (when (looking-at-p openers-regexp)
                (setq begin (point))
                (setq success t))))
          (when success
            (evil-backward-word-end)
            (if (not (looking-at closers-regexp))
                (evil-backward-word-begin)
              (evil-jump-item)
              (forward-char))
            (setq begin (point))))
        (if begin (goto-char begin)))
      (setq count (- count 1)))))

;; declare evil motions
(evil-declare-motion 'evil-forward-arg)
(evil-declare-motion 'evil-backward-arg)
(evil-declare-motion 'evil-jump-out-args)



;;; indent-plus

(defvar evil-indent-plus--base)

(defun evil-indent-plus--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun evil-indent-plus--empty-line-p ()
  (string= "" (evil-indent-plus--chomp (thing-at-point 'line))))

(defun evil-indent-plus--not-empty-line-p ()
  (not (evil-indent-plus--empty-line-p)))

(defun evil-indent-plus--geq-p ()
  (>= (current-indentation) evil-indent-plus--base))

(defun evil-indent-plus--geq-or-empty-p ()
  (or (evil-indent-plus--empty-line-p) (evil-indent-plus--geq-p)))

(defun evil-indent-plus--g-p ()
  (> (current-indentation) evil-indent-plus--base))

(defun evil-indent-plus--g-or-empty-p ()
  (or (evil-indent-plus--empty-line-p) (evil-indent-plus--g-p)))

(defun evil-indent-plus--seek (start direction before skip predicate)
  "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
  (save-excursion
    (goto-char start)
    (goto-char (point-at-bol))
    (let ((bnd (if (> 0 direction)
                   (point-min)
                 (point-max)))
          (pt (point)))
      (when skip (forward-line direction))
      (cl-loop while (and (/= (point) bnd) (funcall predicate))
               do (progn
                    (when before (setq pt (point-at-bol)))
                    (forward-line direction)
                    (unless before (setq pt (point-at-bol)))))
      pt)))

(defun evil-indent-plus--same-indent-range (&optional point)
  "Return the point at the begin and end of the text block with the same (or greater) indentation.
If `point' is supplied and non-nil it will return the begin and end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((evil-indent-plus--base (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (evil-indent-plus--seek begin -1 t nil 'evil-indent-plus--geq-or-empty-p))
      (setq begin (evil-indent-plus--seek begin 1 nil nil 'evil-indent-plus--g-or-empty-p))
      (setq end (evil-indent-plus--seek end 1 t nil 'evil-indent-plus--geq-or-empty-p))
      (setq end (evil-indent-plus--seek end -1 nil nil 'evil-indent-plus--empty-line-p))
      (list begin end evil-indent-plus--base))))

(defun evil-indent-plus--up-indent-range (&optional point)
  (let* ((range (evil-indent-plus--same-indent-range point))
         (evil-indent-plus--base (cl-third range))
         (begin (evil-indent-plus--seek (cl-first range)
                                        -1 nil nil
                                        'evil-indent-plus--geq-or-empty-p)))
    (list begin (cl-second range) evil-indent-plus--base)))

(defun evil-indent-plus--up-down-indent-range (&optional point)
  (let* ((range (evil-indent-plus--same-indent-range point))
         (evil-indent-plus--base (cl-third range))
         (begin (evil-indent-plus--seek (cl-first range)
                                        -1 nil nil
                                        'evil-indent-plus--geq-or-empty-p))
         (end (evil-indent-plus--seek (cl-second range)
                                      1 nil nil
                                      'evil-indent-plus--geq-or-empty-p)))
    (list begin end evil-indent-plus--base)))

(defun evil-indent-plus--linify (range)
  (let ((nbeg (save-excursion (goto-char (cl-first range)) (point-at-bol)))
        (nend (save-excursion (goto-char (cl-second range)) (point-at-eol))))
    (evil-range nbeg nend 'line)))

(defun evil-indent-plus--extend (range)
  (let ((begin (cl-first range))
        (end (cl-second range))
        nend)
    (setq nend (evil-indent-plus--seek end 1 t t 'evil-indent-plus--empty-line-p))
    (when (= nend end)
      (setq begin (evil-indent-plus--seek begin -1 t t 'evil-indent-plus--empty-line-p)))
    (list begin nend)))

(evil-define-text-object evil-indent-plus-i-indent (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--same-indent-range)))

(evil-define-text-object evil-indent-plus-a-indent (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--extend (evil-indent-plus--same-indent-range))))

(evil-define-text-object evil-indent-plus-i-indent-up (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above, skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--up-indent-range)))

(evil-define-text-object evil-indent-plus-a-indent-up (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above, skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--extend (evil-indent-plus--up-indent-range))))

(evil-define-text-object evil-indent-plus-i-indent-up-down (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above and below, skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--up-down-indent-range)))

(evil-define-text-object evil-indent-plus-a-indent-up-down (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above and below, skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--extend (evil-indent-plus--up-down-indent-range))))



;;; syntax-plus

(defun evil-syntax-plus--what-face (&optional pos)
  "Shows all faces at point"
  (let* ((pos (or pos (point)))
         (face (get-text-property pos 'face)))
    (unless (keywordp (car-safe face)) (list face))))

(defun evil-syntax-plus--whitespacep (c)
  "This function returns t if c is white spaces, nil otherwise."
  (= 32 (char-syntax c)))

(defun evil-syntax-plus--create-range (&optional inclusive)
  (let ((point-face (evil-syntax-plus--what-face))
        (backward-point (point)) ; last char when stop, including white space
        (backward-none-space-point (point)) ; last none white space char
        (forward-point (point)) ; last char when stop, including white space
        (forward-none-space-point (point)) ; last none white space char
        (start (point))
        (end (point)))

    ;; check chars backward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (>= (- (point) 1) (point-min)))
          (backward-char)
          (let ((backward-point-face (evil-syntax-plus--what-face)))
            (if (evil-syntax-plus--whitespacep (char-after))
                (setq backward-point (point))
              (if (equal point-face backward-point-face)
                  (progn (setq backward-point (point))
                         (setq backward-none-space-point (point)))
                (setq continue nil)))))))

    ;; check chars forward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (< (+ (point) 1) (point-max)))
          (forward-char)
          (let ((forward-point-face (evil-syntax-plus--what-face)))
            (if (evil-syntax-plus--whitespacep (char-after))
                (setq forward-point (point))
              (if (equal point-face forward-point-face)
                  (progn (setq forward-point (point))
                         (setq forward-none-space-point (point)))
                (setq continue nil)))))))

    (if inclusive
        ;; for outer object,
        ;; if both leading and trailing white spaces exist,
        ;; only trailing whitespaces are included.
        ;; otherwise, leading/trailing/none white spaces are included.
        (progn
          (if (and (/= backward-none-space-point backward-point)
                   (/= forward-none-space-point forward-point))
              (setq start backward-none-space-point)
            (setq start backward-point))
          (setq end forward-point))
      ;; for inner object,
      ;; no leading and trailing white spaces are included
      (setq start backward-none-space-point)
      (setq end forward-none-space-point))

    (evil-range start (+ end 1))))

(evil-define-text-object evil-inner-syntax (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (evil-syntax-plus--create-range))

(evil-define-text-object evil-outer-syntax (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (evil-syntax-plus--create-range t))




;;; bindings

;;;###autoload
(defun evil-tobj-plus-default-keybindings ()
  (define-key evil-inner-text-objects-map "," 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "," 'evil-outer-arg)
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down)
  (define-key evil-inner-text-objects-map "S" 'evil-inner-syntax)
  (define-key evil-outer-text-objects-map "S" 'evil-outer-syntax))

(provide 'evil-tobj-plus)
