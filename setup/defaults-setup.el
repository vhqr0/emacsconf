;;; -*- lexical-binding: t -*-



;;* ui

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq use-dialog-box nil
      use-file-dialog nil)

(setq text-quoting-style 'grave)

(let ((modes
       '(blink-cursor-mode
         tooltip-mode
         tool-bar-mode
         menu-bar-mode
         scroll-bar-mode)))
  (dolist (mode modes)
    (when (fboundp mode)
      (funcall mode -1))))

(setq-default cursor-in-non-selected-windows nil)

(setq frame-title-format nil
      default-frame-alist
      '((left-fringe . 0)
        (right-fringe . 0)
        (internal-border-width . 24)))

(setq window-divider-default-right-width 24)
(window-divider-mode 1)

(defun +nanolize (&rest args)
  (interactive)
  (let* ((background
          (plist-get (custom-face-attributes-get 'default nil)
                     :background))
         (mode-line
          (plist-get (custom-face-attributes-get 'mode-line nil)
                     :background))
         (mode-line-active
          (or (plist-get (custom-face-attributes-get 'mode-line-active nil)
                         :background)
              mode-line))
         (mode-line-inactive
          (or (plist-get (custom-face-attributes-get 'mode-line-inactive nil)
                         :background)
              mode-line)))
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
      (set-face-attribute face nil :foreground background))
    (set-face-attribute 'mode-line-active nil
                        :box `(:line-width 6 :color ,mode-line-active :style nil))
    (set-face-attribute 'mode-line-inactive nil
                        :box `(:line-width 6 :color ,mode-line-inactive :style nil))))

(+nanolize)
(advice-add 'enable-theme :after '+nanolize)



;;* files

(setq auto-revert-check-vc-info t
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 10
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t
      delete-by-moving-to-trash t
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-delete-by-moving-to-trash t
      remote-file-name-inhibit-auto-save-visited t)

(setq backup-directory-alist            `((".*" . ,(expand-file-name "backup/"    user-emacs-directory)  ))
      auto-save-file-name-transforms    `((".*"   ,(expand-file-name "auto-save/" user-emacs-directory) t))
      lock-file-name-transforms         `((".*"   ,(expand-file-name "lock/"      user-emacs-directory) t))
      trash-directory                              (expand-file-name "trash/"     user-emacs-directory)
      undo-tree-history-directory-alist `((".*" . ,(expand-file-name "undo-tree/" user-emacs-directory)  )))

(defvar +auto-save-visited-predicate-hook nil)
(defun +auto-save-visited-predicate ()
  (not (run-hook-with-args-until-success '+auto-save-visited-predicate-hook)))
(setq auto-save-visited-interval 1
      auto-save-visited-predicate '+auto-save-visited-predicate)
(auto-save-visited-mode 1)
(add-to-list 'minor-mode-alist '(auto-save-visited-mode " ASV"))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200)
  :config
  (recentf-mode 1))

(use-package undo-tree
  :init
  (setq undo-tree-mode-lighter nil)
  :config
  (global-undo-tree-mode 1)
  (defun +auto-save-visited-predicate-undo-tree ()
    (and undo-tree-mode
         (let ((buffer (current-buffer)))
           (with-current-buffer (window-buffer)
             (and (eq major-mode 'undo-tree-visualizer-mode)
                  (eq buffer undo-tree-visualizer-parent-buffer))))))
  (add-hook '+auto-save-visited-predicate-hook
            '+auto-save-visited-predicate-undo-tree))



;;* indent and pair

(setq-default indent-tabs-mode nil)

(setq show-paren-context-when-offscreen t)
(show-paren-mode 1)
(electric-pair-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package display-line-numbers
  :ensure nil
  :hook ((text-mode prog-mode) . display-line-numbers-mode)
  :init
  (setq display-line-numbers-type 'relative))

(use-package hl-line
  :ensure nil
  :defer t
  :init
  (setq global-hl-line-sticky-flag t))



;;* wrap

(setq-default truncate-lines t)

(setq word-wrap-by-category t)          ; for cjk wrap

(defun +fixup-whitespace-override ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context.

Override: fix join lines leave space between CJK chars."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (unless (or (looking-at "^\\|$\\|\\s)")
                (save-excursion
                  (forward-char -1)
                  (looking-at "$\\|\\s(\\|\\s'"))
                (and (looking-at "[[:multibyte:]]")
                     (save-excursion
                       (forward-char -1)
                       (looking-at "[[:multibyte:]]"))))
      (insert ?\s))))
(advice-add 'fixup-whitespace :override '+fixup-whitespace-override)



;;* repeat

(setq disabled-command-function nil)

(use-package repeat
  :ensure nil
  :config
  (repeat-mode 1)
  (with-eval-after-load 'dired
    (put 'dired-jump 'repeat-map nil)))




;;* layout

(use-package tab-bar
  :ensure nil
  :init
  (setq tab-bar-tab-hints t
        tab-bar-select-tab-modifiers '(meta))
  :config
  (bind-key "`" 'toggle-frame-tab-bar tab-prefix-map))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))




;;* completion

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(use-package orderless
  :init
  (setq orderless-component-separator 'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless partial-completion basic)))

(bind-key "C-M-_" 'dabbrev-completion)  ; for terminal

(bind-keys ("C-@"   . toggle-input-method)
           ("C-SPC" . toggle-input-method))



;;* match

(use-package isearch
  :ensure nil
  :init
  (setq isearch-lazy-count t
        isearch-allow-scroll t
        isearch-allow-motion t
        isearch-yank-on-move t
        isearch-motion-changes-direction t
        isearch-repeat-on-direction-change t)
  :config
  (bind-keys :map isearch-mode-map
             ("<f2>" . isearch-occur)
             ("M-."  . isearch-forward-symbol-at-point)))

;;* misc

(use-package simple-x
  :ensure nil
  :config
  (simple-x-default-keybindings))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package embark
  :bind ("M-o" . embark-act)
  :init
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  :config
  (advice-add 'embark-completing-read-prompter
              :around 'embark-hide-which-key-indicator))
