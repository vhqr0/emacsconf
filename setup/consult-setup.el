;;; -*- lexical-binding: t -*-



;;* vertico

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
      "\C-u" 'vertico-scroll-down
      "\C-d" 'vertico-scroll-up
      "gg"   'vertico-first
      "G"    'vertico-last)))

;;* consult

(setq completion-in-region-function 'consult-completion-in-region)

;;** search
(define-key search-map "s" 'consult-line)
(define-key search-map "S" 'consult-ripgrep)

;;** switch
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
(global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
(global-set-key [remap bookmark-jump] 'consult-bookmark)

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
