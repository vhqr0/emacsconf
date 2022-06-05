(setq ivy-use-virtual-buffers t
      counsel-describe-symbol-function   'helpful-symbol
      counsel-describe-variable-function 'helpful-variable
      counsel-describe-function-function 'helpful-callable
      counsel-descbinds-function         'helpful-callable)

(global-set-key (kbd "<f5>") 'ivy-resume)

(global-set-key "\M-x" 'counsel-M-x)

(defvar comint-mode-map)
(defvar eshell-hist-mode-map)
(define-key minibuffer-local-map "\M-R" 'counsel-minibuffer-history)
(with-eval-after-load 'comint
  (define-key comint-mode-map "\M-R" 'counsel-shell-history))
(with-eval-after-load 'em-hist
  (define-key eshell-hist-mode-map "\M-R" 'counsel-esh-history))

(define-key search-map "s" 'swiper)
(define-key search-map "S" 'counsel-rg)
(define-key isearch-mode-map [remap swiper] 'swiper-from-isearch)

(define-key ctl-x-r-map "v" 'ivy-push-view)
(define-key ctl-x-r-map "V" 'ivy-pop-view)
(define-key ctl-x-r-map "p" 'counsel-yank-pop)
(define-key ctl-x-r-map "i" 'counsel-register)
(define-key ctl-x-r-map "I" 'counsel-evil-registers)

(define-key goto-map "." 'avy-resume)
(define-key goto-map "j" 'avy-goto-line)
(define-key goto-map "f" 'avy-goto-char)
(define-key goto-map "w" 'avy-goto-symbol-1)
(define-key goto-map "/" 'avy-goto-char-timer)
(define-key goto-map "m" 'counsel-mark-ring)
(define-key goto-map "M" 'counsel-evil-marks)

(define-key help-map "o" 'counsel-describe-symbol)
(define-key help-map "f" 'counsel-describe-function)
(define-key help-map "v" 'counsel-describe-variable)
(define-key help-map "b" 'counsel-descbinds)
(define-key help-map "a" 'counsel-apropos)
(define-key help-map "F" 'counsel-describe-face)
(define-key help-map "S" 'counsel-info-lookup-symbol)
(define-key help-map "t" 'counsel-load-library)
(define-key help-map "T" 'counsel-load-theme)

(defun ivy-tab-completion (arg &optional command)
  "Tab completion with `ivy-read'."
  (interactive "P")
  (require 'ivy)
  (let* ((completion-in-region-function 'ivy-completion-in-region)
         (command (or command
                      (lookup-key `(,(current-local-map) ,(current-global-map))
                                  (if arg
                                      (read-key-sequence-vector "command: ")
                                    (kbd "TAB")))))
         (command (if (memq command '(indent-for-tab-command c-indent-line-or-region))
                      'completion-at-point
                    command)))
    (call-interactively command)))

(defun ivy-dabbrev-completion ()
  (interactive)
  (ivy-tab-completion nil 'dabbrev-completion))

(global-set-key (kbd "<f2>") 'ivy-tab-completion)
(global-set-key (kbd "C-M-/") 'ivy-dabbrev-completion)
(global-set-key (kbd "C-M-_") 'ivy-dabbrev-completion)

(provide 'ivy-setup)
