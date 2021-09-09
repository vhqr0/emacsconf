;;; vip.el --- a VI Package for GNU Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 1986-1988, 1992-1993, 1998, 2001-2021 Free Software
;; Foundation, Inc.

;; Author: Masahiko Sato <ms@sail.stanford.edu>
;; Keywords: emulations

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A full-featured vi(1) emulator.
;;
;; In Japan, the author's address is: masahiko@sato.riec.tohoku.junet
;;
;; Send suggestions and bug reports to one of the above addresses.
;; When you report a bug, be sure to include the version number of VIP and
;; Emacs you are using.

;; Execute info command by typing "M-x info" to get information on VIP.

;;; Code:

;; external variables

(defvar-local vip-insert-point (make-marker)
  "Remember insert point as a marker.")

(defvar-local vip-com-point (make-marker)
  "Remember com point as a marker.")

(defvar-local vip-current-mode 'emacs-mode
  "Current mode.  One of `emacs-mode', `vi-mode', `insert-mode'.")

(defvar vip-last-shell-com nil
  "Last shell command executed by ! command.")

(defvar vip-use-register nil
  "Name of register to store deleted or yanked strings.")

(defvar vip-d-com nil
  "How to reexecute last destructive command.  Value is list (M-COM VAL COM).")

(defvar vip-d-char nil
  "The character remembered by the vi \"r\" command.")

(defvar vip-f-char nil
  "For use by \";\" command.")

(defvar vip-F-char nil
  "For use by \".\" command.")

(defvar vip-f-forward nil
  "For use by \";\" command.")

(defvar vip-f-offset nil
  "For use by \";\" command.")

(defvar vip-s-string nil
  "Last vip search string.")

(defvar vip-s-forward nil
  "If t, search is forward.")

(defvar vip-read-tobj nil
  "For used by \"a\" and \"i\" command to read text object.")

(defvar vip-last-tobj nil
  "Last used text object.")

(defvar vip-read-surround nil)

(defvar vip-last-surround nil)

(defvar vip-eval-functions
  '((emacs-lisp-mode . eval-region)
    (lisp-interaction-mode . eval-region)
    (python-mode . python-shell-send-region)))


;; key bindings

(defvar vip-insert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j"    #'vip-jk)
    (define-key map "\M-z" #'vip-change-mode-to-vi)
    (define-key map "\C-z" #'vip-change-mode-to-emacs)
    map))

(defvar vip-vi-mode-map
  (let ((map (make-keymap)))
    (define-key map [remap self-insert-command] #'undefined)
    (define-key map (kbd "RET") #'undefined)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd "DEL") #'scroll-down-command)

    (define-key map "\C-g" #'vip-keyboard-quit)
    (define-key map "\C-z" #'vip-change-mode-to-emacs)

    (define-key map "0" #'vip-beginning-of-line)
    (define-key map "1" #'vip-digit-argument)
    (define-key map "2" #'vip-digit-argument)
    (define-key map "3" #'vip-digit-argument)
    (define-key map "4" #'vip-digit-argument)
    (define-key map "5" #'vip-digit-argument)
    (define-key map "6" #'vip-digit-argument)
    (define-key map "7" #'vip-digit-argument)
    (define-key map "8" #'vip-digit-argument)
    (define-key map "9" #'vip-digit-argument)

    (define-key map "c"  #'vip-command-argument)
    (define-key map "d"  #'vip-command-argument)
    (define-key map "y"  #'vip-command-argument)
    (define-key map "="  #'vip-command-argument)
    (define-key map "!"  #'vip-command-argument)
    (define-key map "#"  #'vip-command-argument)
    (define-key map "&"  #'vip-command-argument)
    (define-key map "@"  #'vip-command-argument)
    (define-key map "\"" #'vip-command-argument)
    (define-key map "gc" "#")
    (define-key map "gn" "&")
    (define-key map "gr" "@")
    (define-key map "C"  "c$")
    (define-key map "D"  "d$")
    (define-key map "Y"  "y$")

    (define-key map "h"  #'vip-backward-char)
    (define-key map "j"  #'vip-next-line)
    (define-key map "k"  #'vip-previous-line)
    (define-key map "l"  #'vip-forward-char)
    (define-key map "w"  #'vip-forward-word)
    (define-key map "W"  #'vip-forward-Word)
    (define-key map "b"  #'vip-backward-word)
    (define-key map "B"  #'vip-backward-Word)
    (define-key map "e"  #'vip-end-of-word)
    (define-key map "E"  #'vip-end-of-Word)
    (define-key map "f"  #'vip-find-char-forward)
    (define-key map "F"  #'vip-find-char-backward)
    (define-key map "t"  #'vip-goto-char-forward)
    (define-key map "T"  #'vip-goto-char-backward)
    (define-key map ";"  #'vip-repeat-find)
    (define-key map ","  #'vip-repeat-find-opposite)
    (define-key map "/"  #'vip-search-forward)
    (define-key map "?"  #'vip-search-backward)
    (define-key map "n"  #'vip-search-next)
    (define-key map "N"  #'vip-search-Next)
    (define-key map "^"  #'vip-bol-and-skip-white)
    (define-key map "$"  #'vip-goto-eol)
    (define-key map "{"  #'vip-backward-paragraph)
    (define-key map "}"  #'vip-forward-paragraph)
    (define-key map "G"  #'vip-goto-last-line)
    (define-key map "gg" #'vip-goto-first-line)

    (define-key map "." #'vip-repeat)
    (define-key map "u" #'undo)
    (define-key map ":" #'execute-extended-command)
    (define-key map "v" #'set-mark-command)
    (define-key map "m" #'point-to-register)
    (define-key map "`" #'vip-goto-mark)
    (define-key map "'" #'vip-goto-mark-and-skip-white)

    (define-key map "i" #'vip-insert)
    (define-key map "I" #'vip-Insert)
    (define-key map "a" #'vip-append)
    (define-key map "A" #'vip-Append)
    (define-key map "o" #'vip-open-line)
    (define-key map "O" #'vip-Open-line)

    (define-key map "J" "j\M-^")
    (define-key map "p" #'vip-put-back)
    (define-key map "P" #'vip-Put-back)
    (define-key map "r" #'vip-replace-char)
    (define-key map "x" #'vip-delete-char)
    (define-key map "X" #'vip-delete-backward-char)
    (define-key map "s" #'vip-s)

    (define-key map "ZQ" #'kill-emacs)
    (define-key map "ZZ" #'save-buffers-kill-emacs)

    map))

(defalias 'w 'save-buffer)


;; basic set up

(defmacro vip-loop (count &rest body)
  "(COUNT BODY) Execute BODY COUNT times."
  (declare (indent 1))
  `(let ((count ,count))
     (while (> count 0)
       ,@body
       (setq count (1- count)))))

(defun vip-copy-keymap (map)
  (if map (copy-keymap map) (make-sparse-keymap)))

(defun vip-push-mark-silent (&optional location)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
No message."
  (when (mark t)
    (setq mark-ring `(,(copy-marker (mark-marker)) . ,mark-ring))
    (if (> (length mark-ring) mark-ring-max)
        (progn
          (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
          (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil))))
  (set-mark (or location (point))))


;; changing mode

(define-minor-mode vip-insert-mode
  "Vip insert mode."
  :keymap vip-insert-mode-map)

(define-minor-mode vip-vi-mode
  "Vip vi mode."
  :keymap vip-vi-mode-map)

(defun vip-change-mode (new-mode)
  (unless (eq new-mode vip-current-mode)
    (cond ((eq new-mode 'vi-mode)
           (when (eq vip-current-mode 'insert-mode)
             (copy-region-as-kill (point) vip-insert-point)
             (vip-repeat-insert-command)
             (vip-insert-mode -1))
           (vip-vi-mode 1)
           (vip-change-mode-line "[Vi]:"))
          ((eq new-mode 'insert-mode)
           (when (eq vip-current-mode 'vi-mode)
             (move-marker vip-insert-point (point))
             (vip-vi-mode -1))
           (vip-insert-mode 1)
           (vip-change-mode-line "[Insert]:"))
          (t
           (vip-vi-mode -1)
           (vip-insert-mode -1)
           (vip-change-mode-line "[Emacs]:")))
    (setq vip-current-mode new-mode)
    (force-mode-line-update)))

(defun vip-change-mode-line (string)
  "Changing the mode line format."
  (setq mode-line-buffer-identification
        `(,(concat string " %17b"))))

;;;###autoload
(defun vip-change-mode-to-vi ()
  "Change mode to vi mode."
  (interactive)
  (vip-change-mode 'vi-mode))

(defun vip-change-mode-to-insert ()
  "Change mode to insert mode."
  (interactive)
  (vip-change-mode 'insert-mode))

(defun vip-change-mode-to-emacs ()
  "Change mode to Emacs mode."
  (interactive)
  (vip-change-mode 'emacs-mode))

(defun vip-jk ()
  (interactive)
  (if (and (eq vip-current-mode 'insert-mode)
           (not executing-kbd-macro)
           (not defining-kbd-macro)
           (not (sit-for 0.1 'no-redisplay)))
      (let ((next-char (read-event)))
        (if (eq next-char ?k)
            (progn
              (vip-change-mode-to-vi)
              (unless (bolp) (left-char)))
          (insert ?j)
          (push next-char unread-command-events)))
    (insert ?j)))


;; prefix argument for vi mode

;; In vi mode, prefix argument is a dotted pair (NUM . COM) where NUM
;; represents the numeric value of the prefix argument and COM represents
;; command prefix such as "c", "d", "m" and "y".

(defun vip-prefix-arg-value (char value com)
  "Compute numeric prefix arg value.  Invoked by CHAR.  VALUE is the value
obtained so far, and COM is the command part obtained so far."
  (while (<= ?0 char ?9)
    (setq value (+ (* (if (numberp value) value 0) 10) (- char ?0)))
    (setq char (read-char)))
  (setq prefix-arg value)
  (when com
    (setq prefix-arg `(,prefix-arg . ,com)))
  (push char unread-command-events))

(defun vip-prefix-arg-com (char value com)
  "Vi operator as prefix argument."
  (let ((cont t))
    (while (and cont (memq char '(?c ?d ?y ?! ?= ?# ?& ?@ ?s ?r ?R ?\")))
      (if com
          ;; this means that we already have a command character, so we
          ;; construct a com list and exit while.  however, if char is "
          ;; it is an error.
          (progn
            ;; new com is (CHAR . OLDCOM)
            (when (= char ?\")
              (error ""))
            (unless (= char ?s)
              (setq com `(,char . ,com)))
            (setq cont nil))
        ;; if com is nil we set com as char, and read more.  again, if char
        ;; is ", we read the name of register and store it in vip-use-register.
        ;; if char is !, or =, a complete com is formed so we exit while.
        (cond ((= char ?\")
               (let ((reg (read-char)))
                 (setq vip-use-register reg)
                 (setq char (read-char))))
              (t
               (setq com char)
               (setq char (read-char)))))))
  (if (atom com)
      ;; com is a single char, so we construct prefix-arg
      ;; and if char is ?, describe prefix arg, otherwise exit by
      ;; pushing the char back
      (progn
        (setq prefix-arg `(,value . ,com))
        (push char unread-command-events))
    ;; as com is non-nil, this means that we have a command to execute
    (if (memq (car com) '(?r ?R))
        ;; execute appropriate region command.
        (let ((char (car com))
              (com (cdr com)))
          (setq prefix-arg `(,value . ,com))
          (if (= char ?r)
              (vip-region prefix-arg)
            (vip-Region prefix-arg))
          ;; reset prefix-arg
          (setq prefix-arg nil))
      ;; otherwise, reset prefix arg and call appropriate command
      (setq value (or value 1))
      (setq prefix-arg nil)
      (cond ((equal com '(?c . ?c)) (vip-line (cons value ?C)))
            ((equal com '(?d . ?d)) (vip-line (cons value ?D)))
            ((equal com '(?y . ?y)) (vip-line (cons value ?Y)))
            ((equal com '(?! . ?!)) (vip-line (cons value ?!)))
            ((equal com '(?= . ?=)) (vip-line (cons value ?=)))
            ((equal com '(?# . ?#)) (vip-line (cons value ?#)))
            ((equal com '(?c . ?#)) (vip-line (cons value ?#)))
            ((equal com '(?& . ?&)) (vip-line (cons value ?&)))
            ((equal com '(?@ . ?@)) (vip-line (cons value ?@)))
            (t (error ""))))))

(defun vip-digit-argument (arg)
  "Begin numeric argument for the next command."
  (interactive "P")
  (vip-prefix-arg-value
   last-command-event nil (when (consp arg) (cdr arg))))

(defun vip-command-argument (arg)
  "Accept a motion command as an argument."
  (interactive "P")
  (condition-case nil
      (vip-prefix-arg-com
       last-command-event
       (cond ((null arg) nil)
             ((consp arg) (car arg))
             ((numberp arg) arg)
             (t (error "strange arg")))
       (cond ((null arg) nil)
             ((consp arg) (cdr arg))
             ((numberp arg) nil)
             (t (error "strange arg"))))
    (quit
     (setq vip-use-register nil)
     (signal 'quit nil))))

(defun vip-p-val (arg)
  "Get value part of prefix-argument ARG."
  (cond ((null arg) 1)
        ((consp arg) (or (car arg) 1))
        (t arg)))

(defun vip-P-val (arg)
  "Get value part of prefix-argument ARG."
  (cond ((consp arg) (car arg))
        (t arg)))

(defun vip-getcom (arg)
  "Get com part of prefix-argument ARG."
  (cond ((null arg) nil)
        ((consp arg) (cdr arg))
        (t nil)))

(defun vip-getCom (arg)
  "Get com part of prefix-argument ARG and modify it."
  (let ((com (vip-getcom arg)))
    (or (cdr-safe
         (assq com '((?c . ?C) (?d . ?D) (?y . ?Y) (?s . ?S))))
        com)))


;; repeat last destructive command

(defun vip-eval-region (beg end)
  (let ((func (assq major-mode vip-eval-functions)))
    (when func
      (funcall (cdr func) beg end))))

(defun vip-execute-com (m-com val com)
  "(M-COM VAL COM)  Execute command COM. The list (M-COM VAL COM) is set
to vip-d-com for later use by vip-repeat"
  (let ((reg vip-use-register) func)
    (if com
        (cond ((= com ?c) (vip-change vip-com-point (point)))
              ((= com (- ?c)) (vip-change-subr vip-com-point (point)))
              ((memq com `(?C ,(- ?C)))
               (save-excursion
                 (set-mark vip-com-point)
                 (vip-enlarge-region (mark) (point))
                 (when vip-use-register
                   (copy-to-register vip-use-register (mark) (point))
                   (setq vip-use-register nil))
                 (delete-region (mark) (point)))
               (open-line 1)
               (if (= com ?C)
                   (vip-change-mode-to-insert)
                 (yank)))
              ((= com ?d)
               (when vip-use-register
                 (copy-to-register vip-use-register vip-com-point (point))
                 (setq vip-use-register nil))
               (setq last-command
                     (when (eq last-command 'd-command)
                       'kill-region))
               (kill-region vip-com-point (point))
               (setq this-command 'd-command))
              ((= com ?D)
               (save-excursion
                 (set-mark vip-com-point)
                 (vip-enlarge-region (mark) (point))
                 (when vip-use-register
                   (copy-to-register vip-use-register (mark) (point))
                   (setq vip-use-register nil))
                 (setq last-command
                       (when (eq last-command 'D-command)
                         'kill-region))
                 (kill-region (mark) (point))
                 (when (eq m-com 'vip-line)
                   (setq this-command 'D-command)))
               (back-to-indentation))
              ((= com ?y)
               (when vip-use-register
                 (copy-to-register vip-use-register vip-com-point (point))
                 (setq vip-use-register nil))
               (setq last-command nil)
               (copy-region-as-kill vip-com-point (point))
               (goto-char vip-com-point))
              ((= com ?Y)
               (save-excursion
                 (set-mark vip-com-point)
                 (vip-enlarge-region (mark) (point))
                 (when vip-use-register
                   (copy-to-register vip-use-register (mark) (point))
                   (setq vip-use-register nil))
                 (setq last-command nil)
                 (copy-region-as-kill (mark) (point)))
               (goto-char vip-com-point))
              ((memq com `(?s ,(- ?s)))
               (save-excursion
                 (let* ((vip-read-surround (= com ?s))
                        (pair (vip-read-surround)))
                   (insert (cdr pair))
                   (goto-char vip-com-point)
                   (insert (car pair)))))
              ((memq com `(?S ,(- ?S)))
               (save-excursion
                 (let* ((vip-read-surround (= com ?S))
                        (pair (vip-read-surround)))
                   (set-mark vip-com-point)
                   (vip-enlarge-region (mark) (point))
                   (move-marker vip-com-point (mark))
                   (open-line 1)
                   (insert (cdr pair))
                   (goto-char vip-com-point)
                   (open-line 1)
                   (insert (car pair))
                   (indent-region (mark) (point)))))
              ((memq com `(?! ,(- ?!)))
               (save-excursion
                 (set-mark vip-com-point)
                 (vip-enlarge-region (mark) (point))
                 (shell-command-on-region
                  (mark) (point)
                  (if (= com ?!)
                      (setq vip-last-shell-com (read-string "!"))
                    vip-last-shell-com)
                  t t)))
              ((setq func (assq com '((?= . indent-region)
                                      (?# . comment-or-uncomment-region)
                                      (?& . narrow-to-region)
                                      (?@ . vip-eval-region))))
               (save-excursion
                 (set-mark vip-com-point)
                 (vip-enlarge-region (mark) (point))
                 (when (> (mark) (point))
                   (exchange-point-and-mark))
                 (funcall (cdr func) (mark) (point))
                 (deactivate-mark)))))
    (setq vip-d-com (list m-com val
                          (if (memq com '(?c ?C ?s ?S ?!))
                              (- com)
                            com)
                          reg))))

(defun vip-repeat (arg)
  "(ARG)  Re-execute last destructive command.  vip-d-com has the form
\(COM ARG CH REG), where COM is the command to be re-executed, ARG is the
argument for COM, CH is a flag for repeat, and REG is optional and if exists
is the name of the register for COM."
  (interactive "P")
  (let ((m-com (car vip-d-com))
        (val (or (vip-P-val arg)
                 (cadr vip-d-com)))
        (com (nth 2 vip-d-com))
        (reg (nth 3 vip-d-com)))
    (unless m-com
      (error "No previous command to repeat"))
    (setq vip-use-register reg)
    (funcall m-com `(,val . ,com))))


;; utilities

(defun vip-enlarge-region (beg end)
  "Enlarge region between BEG and END."
  (if (< beg end)
      (progn
        (goto-char beg)
        (set-mark end))
    (goto-char end)
    (set-mark beg))
  (beginning-of-line)
  (exchange-point-and-mark)
  (unless (and (eobp) (bolp))
    (with-no-warnings
      (next-line 1)))
  (beginning-of-line)
  (when (> beg end)
    (exchange-point-and-mark)))

(defun vip-end-with-a-newline-p (string)
  "Check if the string ends with a newline."
  (or (string-empty-p string)
      (= (aref string (1- (length string))) ?\n)))


;; insertion commands

(defun vip-tobj (arg)
  "Com on text object."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (setq vip-last-tobj
          (if vip-read-tobj
              (read-char)
            vip-last-tobj))
    (let (range)
      (setq range (cdr (assq vip-last-tobj
                             '((?w . word)
                               (?o . sexp)
                               (?f . defun)
                               (?p . paragraph)
                               (?P . page)
                               (?h . buffer)
                               (?b . pair)))))
      (cond ((eq range 'pair)
             (setq range (save-excursion
                           (backward-up-list)
                           (bounds-of-thing-at-point 'sexp))))
            (range
             (setq range (bounds-of-thing-at-point range))))
      (when range
        (if (region-active-p)
            (progn
              (set-mark (car range))
              (goto-char (cdr range)))
          (move-marker vip-com-point (car range))
          (goto-char (cdr range))
          (vip-execute-com 'vip-tobj val com))))))

(defun vip-repeat-insert-command ()
  "This function is called when mode changes from insertion mode to
vi command mode.  It will repeat the insertion command if original insertion
command was invoked with argument > 1."
  (let ((i-com (car vip-d-com))
        (val (cadr vip-d-com)))
    (if (and val (> val 1)) ;; first check that val is non-nil
        (progn
          (setq vip-d-com `(,i-com ,(1- val) ?r))
          (vip-repeat nil)
          (setq vip-d-com `(,i-com ,val ?r))))))

(defun vip-insert (arg)
  "Insert at point."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (or com (region-active-p))
        (if (eq com ?r)
            (vip-loop val (yank))
          (let ((vip-read-tobj t))
            (vip-tobj arg)))
      (setq vip-d-com `(vip-insert ,val ?r))
      (vip-change-mode-to-insert))))

(defun vip-append (arg)
  "Append after point."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (or com (region-active-p))
        (if (eq com ?r)
            (vip-loop val (yank))
          (let ((vip-read-tobj t))
            (vip-tobj arg)))
      (setq vip-d-com `(vip-append ,val ?r))
      (unless (eolp)
        (forward-char))
      (vip-change-mode-to-insert))))

(defun vip-Insert (arg)
  "Insert before first non-white."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (eq com ?r)
        (vip-loop val (yank))
      (setq vip-d-com `(vip-Insert ,val ?r))
      (unless (region-active-p)
        (back-to-indentation))
      (vip-change-mode-to-insert))))

(defun vip-Append (arg)
  "Append at end of line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (eq com ?r)
        (vip-loop val (yank))
      (setq vip-d-com `(vip-Append ,val ?r))
      (unless (region-active-p)
        (end-of-line))
      (vip-change-mode-to-insert))))

(defun vip-open-line (arg)
  "Open line below."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (eq com ?r)
        (vip-loop val
          (end-of-line)
          (newline 1)
          (indent-for-tab-command)
          (yank))
      (if (region-active-p)
          (exchange-point-and-mark)
        (setq vip-d-com `(vip-open-line ,val ?r))
        (end-of-line)
        (newline 1)
        (indent-for-tab-command)
        (vip-change-mode-to-insert)))))

(defun vip-Open-line (arg)
  "Open line above."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (eq com ?r)
        (vip-loop val
          (beginning-of-line)
          (open-line 1)
          (indent-for-tab-command)
          (yank))
      (if (region-active-p)
          (exchange-point-and-mark)
        (setq vip-d-com `(vip-Open-line ,val ?r))
        (beginning-of-line)
        (open-line 1)
        (indent-for-tab-command)
        (vip-change-mode-to-insert)))))


;; line command

(defun vip-line (arg)
  (let ((val (car arg))
        (com (cdr arg)))
    (move-marker vip-com-point (point))
    (with-no-warnings
      (next-line (1- val)))
    (vip-execute-com 'vip-line val com)))


;; region command

(defun vip-region (arg)
  (interactive "P")
  (let ((val (vip-P-val arg))
        (com (vip-getcom arg)))
    (move-marker vip-com-point (point))
    (exchange-point-and-mark)
    (vip-execute-com 'vip-region val com)))

(defun vip-Region (arg)
  (interactive "P")
  (let ((val (vip-P-val arg))
        (com (vip-getCom arg)))
    (move-marker vip-com-point (point))
    (exchange-point-and-mark)
    (vip-execute-com 'vip-Region val com)))

(defun vip-replace-char (arg)
  "Replace the following ARG chars by the character read."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (setq vip-d-com `(vip-replace-char ,val ?r))
    (vip-replace-char-subr
     (if (equal com ?r)
         vip-d-char
       (read-char))
     val)))

(defun vip-replace-char-subr (char arg)
  (delete-char arg t)
  (setq vip-d-char char)
  (vip-loop (abs arg) (insert char))
  (backward-char arg))


;; basic cursor movement.  j, k, l, m commands.

(defun vip-forward-char (arg)
  "Move point right ARG characters (left if ARG negative).On reaching end
of buffer, stop and signal error."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (forward-char val)
    (when com
      (vip-execute-com 'vip-forward-char val com))))

(defun vip-backward-char (arg)
  "Move point left ARG characters (right if ARG negative).  On reaching
beginning of buffer, stop and signal error."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (backward-char val)
    (when com
      (vip-execute-com 'vip-backward-char val com))))


;; word command

(defun vip-forward-word (arg)
  "Forward word."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (forward-word val)
    (skip-chars-forward " \t\n")
    (when com
      (when (memq com `(?c ,(- ?c)))
        (backward-word 1)
        (forward-word 1))
      (when (memq com '(?d ?y))
        (backward-word 1)
        (forward-word 1)
        (skip-chars-forward " \t"))
      (vip-execute-com 'vip-forward-word val com))))

(defun vip-end-of-word (arg)
  "Move point to end of current word."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (forward-char)
    (forward-word val)
    (backward-char)
    (when com
      (forward-char)
      (vip-execute-com 'vip-end-of-word val com))))

(defun vip-backward-word (arg)
  "Backward word."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (backward-word val)
    (when com
      (vip-execute-com 'vip-backward-word val com))))

(defun vip-forward-Word (arg)
  "Forward word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (re-search-forward "[^ \t\n]*[ \t\n]+" nil t val)
    (when com
      (when (memq com `(?c ,(- ?c)))
        (backward-word 1)
        (forward-word 1))
      (when (memq com '(?d ?y))
        (backward-word 1)
        (forward-word 1)
        (skip-chars-forward " \t"))
      (vip-execute-com 'vip-forward-Word val com))))

(defun vip-end-of-Word (arg)
  "Move forward to end of word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (forward-char)
    (when (re-search-forward "[^ \t\n]+" nil t val)
      (backward-char))
    (when com
      (forward-char)
      (vip-execute-com 'vip-end-of-Word val com))))

(defun vip-backward-Word (arg)
  "Backward word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (when (re-search-backward "[ \t\n]+[^ \t\n]+" nil t val)
      (forward-char)
      (goto-char (point-min)))
    (when com
      (vip-execute-com 'vip-backward-Word val com))))

(defun vip-beginning-of-line (arg)
  "Go to beginning of line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (beginning-of-line val)
    (when com
      (vip-execute-com 'vip-beginning-of-line val com))))

(defun vip-bol-and-skip-white (arg)
  "Beginning of line at first non-white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (back-to-indentation)
    (when com
      (vip-execute-com 'vip-bol-and-skip-white val com))))

(defun vip-goto-eol (arg)
  "Go to end of line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (end-of-line val)
    (when com
      (vip-execute-com 'vip-goto-eol val com))))

(defun vip-goto-first-line (arg)
  "Go to first line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (beginning-of-buffer)
    (when com
      (vip-execute-com 'vip-goto-first-line val com))))

(defun vip-goto-last-line (arg)
  "Go to last line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (end-of-buffer)
    (when com
      (vip-execute-com 'vip-goto-last-line val com))))

(defun vip-next-line (arg)
  "Go to next line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getCom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (line-move val)
    (setq this-command 'next-line)
    (when com
      (vip-execute-com 'vip-next-line val com))))

(defun vip-previous-line (arg)
  "Go to previous line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getCom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (with-no-warnings (next-line (- val)))
    (setq this-command 'previous-line)
    (when com
      (vip-execute-com 'vip-previous-line val com))))


;; moving around

(defun vip-find-char (arg char forward offset)
  "Find ARG's occurrence of CHAR on the current line.  If FORWARD then
search is forward, otherwise backward.  OFFSET is used to adjust point
after search."
  (let ((arg (if forward arg (- arg))) point)
    (save-excursion
      (save-restriction
        (if (> arg 0)
            (narrow-to-region
             ;; forward search begins here
             (if (eolp)
                 (error "")
               (point))
             ;; forward search ends here
             (progn
               (with-no-warnings
                 (next-line 1))
               (beginning-of-line)
               (point)))
          (narrow-to-region
           ;; backward search begins from here
           (if (bolp)
               (error "")
             (point))
           ;; backward search ends here
           (progn
             (beginning-of-line)
             (point))))
        ;; if arg > 0, point is forwarded before search.
        (if (> arg 0)
            (goto-char (1+ (point-min)))
          (goto-char (point-max)))
        (let ((case-fold-search nil))
          (search-forward (char-to-string char) nil 0 arg))
        (setq point (point))
        (when (or (and (> arg 0) (= point (point-max)))
                  (and (< arg 0) (= point (point-min))))
          (error ""))))
    (goto-char (+ point
                  (if (> arg 0)
                      (if offset -2 -1)
                    (if offset 1 0))))))

(defun vip-find-char-forward (arg)
  "Find char on the line.  If called interactively read the char to find
from the terminal, and if called from vip-repeat, the char last used is
used.  This behavior is controlled by the sign of prefix numeric value."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (> val 0)
        ;; this means that the function was called interactively
        (setq vip-f-char (read-char)
              vip-f-forward t
              vip-f-offset nil)
      (setq val (- val)))
    (when com
      (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) t nil)
    (setq val (- val))
    (when com
      (setq vip-F-char vip-f-char) ;; set new vip-F-char
      (forward-char)
      (vip-execute-com 'vip-find-char-forward val com))))

(defun vip-goto-char-forward (arg)
  "Go up to char ARG forward on line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (> val 0)
        ;; this means that the function was called interactively
        (setq vip-f-char (read-char)
              vip-f-forward t
              vip-f-offset t)
      (setq val (- val)))
    (when com
      (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) t t)
    (setq val (- val))
    (when com
      (setq vip-F-char vip-f-char) ;; set new vip-F-char
      (forward-char)
      (vip-execute-com 'vip-goto-char-forward val com))))

(defun vip-find-char-backward (arg)
  "Find char ARG on line backward."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (> val 0)
        ;; this means that the function was called interactively
        (setq vip-f-char (read-char)
              vip-f-forward nil
              vip-f-offset nil)
      (setq val (- val)))
    (when com
      (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) nil nil)
    (setq val (- val))
    (when com
      (setq vip-F-char vip-f-char) ;; set new vip-F-char
      (vip-execute-com 'vip-find-char-backward val com))))

(defun vip-goto-char-backward (arg)
  "Go up to char ARG backward on line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (if (> val 0)
        ;; this means that the function was called interactively
        (setq vip-f-char (read-char)
              vip-f-forward nil
              vip-f-offset t)
      (setq val (- val)))
    (when com
      (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) nil t)
    (setq val (- val))
    (when com
      (setq vip-F-char vip-f-char) ;; set new vip-F-char
      (vip-execute-com 'vip-goto-char-backward val com))))

(defun vip-repeat-find (arg)
  "Repeat previous find command."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (vip-find-char val vip-f-char vip-f-forward vip-f-offset)
    (when com
      (if vip-f-forward (forward-char))
      (vip-execute-com 'vip-repeat-find val com))))

(defun vip-repeat-find-opposite (arg)
  "Repeat previous find command in the opposite direction."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (vip-find-char val vip-f-char (not vip-f-forward) vip-f-offset)
    (when com
      (if vip-f-forward (forward-char))
      (vip-execute-com 'vip-repeat-find-opposite val com))))


;; paragraph

(defun vip-forward-paragraph (arg)
  "Forward paragraph."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getCom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (forward-paragraph val)
    (when com
      (vip-execute-com 'vip-forward-paragraph nil com))))

(defun vip-backward-paragraph (arg)
  "Backward paragraph."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getCom arg)))
    (when com
      (move-marker vip-com-point (point)))
    (backward-paragraph val)
    (when com
      (vip-execute-com 'vip-backward-paragraph nil com))))


;; searching

(defun vip-search-forward (arg)
  "Search a string forward.  ARG is used to find the ARG's occurrence
of the string.  Default is vanilla search.  Search mode can be toggled by
giving null search string."
  (interactive "P")
  (let ((val (vip-P-val arg))
        (com (vip-getcom arg)))
    (setq vip-s-forward t
          vip-s-string (read-string "/"))
    (vip-search vip-s-string t val)
    (when com
      (move-marker vip-com-point (mark))
      (vip-execute-com 'vip-search-next val com))))

(defun vip-search-backward (arg)
  "Search a string backward.  ARG is used to find the ARG's occurrence
of the string.  Default is vanilla search.  Search mode can be toggled by
giving null search string."
  (interactive "P")
  (let ((val (vip-P-val arg))
        (com (vip-getcom arg)))
    (setq vip-s-forward nil
          vip-s-string (read-string "?"))
    (vip-search vip-s-string nil val)
    (when com
      (move-marker vip-com-point (mark))
      (vip-execute-com 'vip-search-next val com))))

(defun vip-search (string forward arg &optional no-offset init-point)
  "(STRING FORWARD COUNT &optional NO-OFFSET) Search COUNT's occurrence of
STRING.  Search will be forward if FORWARD, otherwise backward."
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg))
        (null-arg (null (vip-P-val arg)))
        (offset (not no-offset))
        (start-point (or init-point (point))))
    (if forward
        (condition-case conditions
            (progn
              (when (and offset (not (eobp)))
                (forward-char))
              (re-search-forward string nil nil val)
              (re-search-backward string)
              (push-mark start-point))
          (search-failed
           (if null-arg
               (progn
                 (goto-char (point-min))
                 (vip-search string forward `(1 . ,com) t start-point))
             (goto-char start-point)
             (signal 'search-failed (cdr conditions)))))
      (condition-case conditions
          (progn
            (re-search-backward string nil nil val)
            (push-mark start-point))
        (search-failed
         (if null-arg
             (progn
               (goto-char (point-max))
               (vip-search string forward `(1 . ,com) t start-point))
           (goto-char start-point)
           (signal 'search-failed (cdr conditions))))))))

(defun vip-search-next (arg)
  "Repeat previous search."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (unless vip-s-string
        (error "No previous search string"))
    (vip-search vip-s-string vip-s-forward arg)
    (when com
      (vip-execute-com 'vip-search-next val com))))

(defun vip-search-Next (arg)
  "Repeat previous search in the reverse direction."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (unless vip-s-string
      (error "No previous search string"))
    (vip-search vip-s-string (not vip-s-forward) arg)
    (when com
      (vip-execute-com 'vip-search-Next val com))))


;;; surround

(defun vip-read-surround ()
  (setq vip-last-surround
        (if vip-read-surround
            (read-char)
          vip-last-surround))
  (or (cdr (assq vip-last-surround '((?b . (?\( . ?\)))
                                     (?B . (?\{ . ?\}))
                                     (?r . (?\[ . ?\])))))
      `(,vip-last-surround . ,vip-last-surround)))

(defun vip-surround (arg)
  "Surround."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (com (vip-getcom arg)))
    (cond ((eq com ?y)
           (let ((last-command-event ?s))
             (vip-command-argument nil)))
          ((eq com ?s)
           (setq vip-d-com '(vip-surround nil ?s))
           (let ((pair (vip-read-surround)))
             (save-excursion
               (end-of-line)
               (insert (cdr pair))
               (vip-bol-and-skip-white nil)
               (insert (car pair)))))
          ((eq com ?d)
           (setq vip-d-com '(vip-surround nil ?d))
           (save-excursion
             (backward-up-list)
             (delete-pair)))
          ((eq com ?c)
           (setq vip-d-com `(vip-surround nil ?c))
           (let ((pair (vip-read-surround)))
             (save-excursion
               (backward-up-list)
               (insert-pair 1 (car pair) (cdr pair))
               (delete-pair))))
          ((region-active-p)
           (let ((pair (vip-read-surround)))
             (deactivate-mark)
             (insert (cdr pair))
             (goto-char (mark))
             (insert (car pair))))
          (t
           (vip-delete-char arg)
           (vip-change-mode-to-insert)))))

(defun vip-s (arg)
  "Surround."
  (interactive "P")
  (let ((vip-read-surround t))
    (vip-surround arg)))


;; yank and pop

(defun vip-yank (text)
  "yank TEXT silently."
  (save-excursion
    (vip-push-mark-silent (point))
    (insert text)
    (exchange-point-and-mark))
  (skip-chars-forward " \t"))

(defun vip-put-back (arg)
  "Put back after point/below line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (text (if vip-use-register
                  (if (<= ?1 vip-use-register ?9)
                      (current-kill (- vip-use-register ?1) 'do-not-rotate)
                    (get-register vip-use-register))
                (current-kill 0))))
    (unless text
      (if vip-use-register
          (let ((reg vip-use-register))
            (setq vip-use-register nil)
            (error "Nothing in register %c" reg))
        (error "")))
    (setq vip-use-register nil)
    (if (vip-end-with-a-newline-p text)
        (progn
          (with-no-warnings (next-line 1))
          (beginning-of-line))
      (unless (or (eolp) (eobp))
        (forward-char)))
    (setq vip-d-com `(vip-put-back ,val nil ,vip-use-register))
    (vip-loop val (vip-yank text))))

(defun vip-Put-back (arg)
  "Put back at point/above line."
  (interactive "P")
  (let ((val (vip-p-val arg))
        (text (if vip-use-register
                  (if (<= ?1 vip-use-register ?9)
                      (current-kill (- vip-use-register ?1) 'do-not-rotate)
                    (get-register vip-use-register))
                (current-kill 0))))
    (unless text
      (if vip-use-register
          (let ((reg vip-use-register))
            (setq vip-use-register nil)
            (error "Nothing in register %c" reg))
        (error "")))
    (setq vip-use-register nil)
    (when (vip-end-with-a-newline-p text)
      (beginning-of-line))
    (setq vip-d-com `(vip-Put-back ,val nil ,vip-use-register))
    (vip-loop val (vip-yank text))))

(defun vip-delete-char (arg)
  "Delete character."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (setq vip-d-com `(vip-delete-char ,val nil))
    (when vip-use-register
      (copy-to-register vip-use-register (point) (- (point) val))
      (setq vip-use-register nil))
    (delete-char val t)))

(defun vip-delete-backward-char (arg)
  "Delete previous character."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (setq vip-d-com `(vip-delete-backward-char ,val nil))
    (when vip-use-register
      (copy-to-register vip-use-register (point) (+ (point) val))
      (setq vip-use-register nil))
    (with-no-warnings
      (delete-backward-char val t))))


;; making small changes

(defvar vip-c-string)

(defun vip-change (beg end)
  (setq vip-c-string
        (read-string (format "%s => " (buffer-substring beg end))))
  (vip-change-subr beg end))

(defun vip-change-subr (beg end)
  (when vip-use-register
    (copy-to-register vip-use-register beg end)
    (setq vip-use-register nil))
  (kill-region beg end)
  (setq this-command 'vip-change)
  (insert vip-c-string))


;; marking

(defun vip-goto-mark (arg)
  "Go to mark."
  (interactive "P")
  (let ((char (read-char))
        (com (vip-getcom arg)))
    (vip-goto-mark-subr char com nil)))

(defun vip-goto-mark-and-skip-white (arg)
  "Go to mark and skip to first non-white on line."
  (interactive "P")
  (let ((char (read-char))
        (com (vip-getCom arg)))
    (vip-goto-mark-subr char com t)))

(defun vip-goto-mark-subr (char com skip-white)
  (let ((buff (current-buffer)))
    (when com
      (move-marker vip-com-point (point)))
    (goto-char (register-to-point char))
    (when skip-white
      (back-to-indentation))
    (vip-change-mode-to-vi)
    (when com
      (if (equal buff (current-buffer))
          (vip-execute-com (if skip-white
                               'vip-goto-mark-and-skip-white
                             'vip-goto-mark)
                           nil com)
        (switch-to-buffer buff)
        (goto-char vip-com-point)
        (vip-change-mode-to-vi)
        (error "")))))

(defun vip-keyboard-quit ()
  "Abort partially formed or running command."
  (interactive)
  (setq vip-use-register nil)
  (keyboard-quit))

(provide 'vip)

;;; vip.el ends here
