(setq ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      ivy-read-action-function 'ivy-hydra-read-action)

(require 'ivy)
(require 'swiper)
(require 'counsel)

(ivy-mode 1)
(counsel-mode 1)

(dolist (mode '(ivy-mode counsel-mode))
  (setcdr (assq mode minor-mode-alist) '("")))



(define-key ivy-minibuffer-map (kbd "<f2>") 'ivy-occur)
(define-key ivy-minibuffer-map "\M-g" 'ivy-avy)
(define-key ivy-minibuffer-map "\M-." 'minibuffer-yank-symbol)

(define-key counsel-mode-map [remap comint-history-isearch-backward-regexp] 'counsel-shell-history)
(define-key counsel-mode-map [remap eshell-previous-matching-input] 'counsel-esh-history)

(global-set-key (kbd "<f5>") 'ivy-resume)



(define-key search-map "s" 'swiper)
(define-key search-map "S" 'counsel-rg)
(define-key isearch-mode-map [remap swiper] 'swiper-from-isearch)

(define-key ctl-x-r-map "v" 'ivy-push-view)
(define-key ctl-x-r-map "V" 'ivy-pop-view)
(define-key ctl-x-r-map "i" 'counsel-register)
(define-key ctl-x-r-map "I" 'counsel-evil-registers)

(define-key goto-map "m" 'counsel-mark-ring)
(define-key goto-map "M" 'counsel-evil-marks)

(define-key help-map "V" 'counsel-set-variable)



(defun ivy-tab-completion (arg &optional command)
  "Tab completion with `ivy-read'."
  (interactive "P")
  (let* ((completion-in-region-function 'ivy-completion-in-region)
         (command (or command
                      (lookup-key `(,(current-local-map) ,(current-global-map))
                                  (if arg
                                      (read-key-sequence-vector "command: ")
                                    (kbd "TAB")))))
         (command (if (memq command '(indent-for-tab-command c-indent-line-or-region))
                      'completion-at-point
                    command)))
    (completion-in-region-mode -1)
    (call-interactively command)))

(global-set-key (kbd "<f2>") 'ivy-tab-completion)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<f2>") 'counsel-company))



(provide 'ivy-setup)
