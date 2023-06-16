;;; evil-eval.el --- Eval operator of evil. -*- lexical-binding: t *-

(require 'evil)

(eval-when-compile
  (require 'evil))

(defvar evil-eval-alist
  '((emacs-lisp-mode . eval-region)
    (lisp-interaction-mode . eval-region)
    (python-mode . python-shell-send-region)))

(evil-define-operator evil-operator-eval (start end)
  :move-point nil
  (interactive "<r>")
  (let ((func (cdr (assq major-mode evil-eval-alist))))
    (if func
        (funcall func start end)
      (user-error "Major mode doesn't support"))))

(provide 'evil-eval)
