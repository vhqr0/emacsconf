;;; -*- lexical-binding: t -*-



;;* vertico
(setq marginalia-align 'right)

(vertico-mode 1)
(marginalia-mode 1)

(defun +vertico-setup-completion-styles ()
  (setq-local completion-styles '(orderless basic)))
(advice-add 'vertico--setup :after '+vertico-setup-completion-styles)

(global-set-key "\M-o" 'embark-act)

(define-key vertico-map (kbd "<f2>") 'embark-export)

(with-eval-after-load 'evil-collection-vertico
  (when evil-collection-setup-minibuffer
    (evil-collection-define-key 'normal 'vertico-map
      "gg" 'vertico-first
      "G"  'vertico-last)))

;;* consult

(setq completion-in-region-function 'consult-completion-in-region)

;;** search
(defalias 'consult-line-at-point 'consult-line)

(define-key search-map "s" 'consult-line)
(define-key search-map "S" 'consult-ripgrep)
(global-set-key "\C-s" 'consult-line-at-point)

;;** buffer
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
(global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)

;;** nav
(global-set-key [remap goto-line] 'consult-goto-line)
(global-set-key [remap imenu] 'consult-imenu)

;;** ring
(global-set-key [remap yank-pop] 'consult-yank-pop)
(global-set-key [remap next-matching-history-element] 'consult-history)
(global-set-key [remap previous-matching-history-element] 'consult-history)
(global-set-key [remap comint-history-isearch-backward-regexp] 'consult-history)
(global-set-key [remap eshell-next-matching-input] 'consult-history)
(global-set-key [remap eshell-previous-matching-input] 'consult-history)

;;** misc
(global-set-key [remap load-theme] 'consult-theme)

;;** preview

(with-eval-after-load 'consult
  (consult-customize
   consult-line-at-point
   :initial (thing-at-point 'symbol)
   consult-buffer consult-buffer-other-window consult-buffer-other-frame
   :preview-key nil
   consult-grep consult-git-grep consult-ripgrep consult-theme
   :preview-key '(:debounce 0.5 any)))
