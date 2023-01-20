(defvar god-default-modifier "C-")

(defvar god-modifier-alist
  '(("\s" . "") ("g" . "M-") ("h" . "C-")))

(defvar god-special-char-alist
  '((tab . "TAB")
    (backspace . "DEL")
    (return . "RET")))

(defun god-char-to-string (char)
  (cond ((symbolp char)
         (or (cdr (assq char god-special-char-alist)) ""))
        ((stringp char)
         char)
        ((integerp char)
         (char-to-string char))
        (t
         (user-error "god-char-to-string: invalid char"))))

(defun god-read-event (prev-keys &optional modifiers event)
  (let* ((event (or event (read-key (concat prev-keys " " modifiers))))
         (key (god-char-to-string event))
         (modifier (cdr (assoc key god-modifier-alist))))
    (cond ((memq event '(?\C-h f1 help))
           (execute-kbd-macro (kbd (concat prev-keys " C-h")))
           (god-read-event prev-keys modifiers))
          (modifier
           (god-read-event prev-keys (concat modifiers modifier)))
          (t
           (cons (or modifiers god-default-modifier) key)))))

(defun god-lookup-key-1 (keys)
  (let* ((keyseq (read-kbd-macro keys t))
         (binding (key-binding keyseq)))
    (when binding
      (list binding keys keyseq))))

(defun god-lookup-key (prev-keys &optional event)
  (let* ((event (god-read-event prev-keys nil event))
         (binding (or (god-lookup-key-1 (concat prev-keys " " (car event) (cdr event)))
                      (god-lookup-key-1 (concat prev-keys " " (cdr event))))))
    (cond ((and binding (keymapp (car binding)))
           (god-lookup-key (cadr binding)))
          ((and binding (commandp (car binding)))
           binding)
          (t
           (user-error "god-lookup-key: lookup failed")))))

;;;###autoload
(defun god-execute (&optional prev-keys)
  (interactive)
  (let* ((binding (god-lookup-key prev-keys))
         (keyseq (caddr binding))
         (binding (car binding)))
    (if (not (commandp binding t))
        (execute-kbd-macro binding)
      (setq last-command-event (aref keyseq (1- (length keyseq)))
            this-command binding
            real-this-command binding)
      (call-interactively binding))))

;;;###autoload
(defun god-C-x () (interactive) (god-execute "C-x"))

;;;###autoload
(defun god-C-c () (interactive) (god-execute "C-c"))

(provide 'god)
