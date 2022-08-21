;;; simple-x.el --- Simple Extra. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some simple commands collection.

;;; Code:



;; some linux tools wrap

(defvar xclip-program "xclip -selection clip")

(defun xclip (beg end)
  "Xclip wrap for copy regin (BEG . END)."
  (interactive "r")
  (call-shell-region beg end xclip-program)
  (deactivate-mark))

(defvar xdg-open-program "xdg-open")

(defun xdg-open (&optional file)
  "Xdg wrap for open FILE or current file if called interactively."
  (interactive `(,(or buffer-file-name default-directory)))
  (when (and file (not (file-remote-p file)))
    (call-process-shell-command (concat xdg-open-program " " file))))

(declare-function dired-get-marked-files "dired")

(defun dired-do-xdg-open ()
  "`xdg-open' files in Dired."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (xdg-open file)))

(defvar sdcv-program "sdcv")

(defun sdcv (&optional word)
  (interactive (list (if current-prefix-arg
                         (read-string "word: ")
                       (thing-at-point 'word))))
  (when word
    (shell-command (concat sdcv-program " " word))))

(defalias 'sd 'sdcv)

(declare-function grep--save-buffers "grep")

(defvar rg-program "rg")

(defun rg ()
  "Rg wrap for `grep-mode'."
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: "
                       (concat rg-program " --no-heading ")
                       'grep-history)
   'grep-mode))

(defvar external-format-program-alist
  '((c-mode . "clang-format")
    (c++-mode . "clang-format")
    (python-mode . "yapf")))

(defun external-format (beg end)
  (interactive (list (if (use-region-p) (region-beginning) (point-min))
                     (if (use-region-p) (region-end) (point-max))))
  (let ((program (cdr (assq major-mode external-format-program-alist))))
    (save-restriction
      (narrow-to-region beg end)
      (if program
          (let ((row (line-number-at-pos)))
            (shell-command-on-region (point-min) (point-max) program nil t)
            (goto-char (point-min))
            (forward-line (1- row))
            (narrow-to-region (line-beginning-position) (line-end-position))
            (back-to-indentation))
        (delete-trailing-whitespace (point-min) (point-max))
        (indent-region (point-min) (point-max))))))



;; the missing commands

(defun minibuffer-yank-symbol ()
  "Yank current symbol to minibuffer."
  (interactive)
  (when (window-minibuffer-p)
    (let ((symbol (with-selected-window (minibuffer-selected-window)
                    (thing-at-point 'symbol))))
      (when symbol
        (insert symbol)))))

(defun rotate-window (arg)
  "Rotate current window or swap it if called with prefix ARG."
  (interactive "P")
  (if arg
      (let* ((window (selected-window))
             (next-window (next-window))
             (buffer (window-buffer window))
             (next-buffer (window-buffer next-window)))
        (unless (eq window next-window)
          (set-window-buffer window next-buffer)
          (set-window-buffer next-window buffer)
          (select-window next-window)))
    (let* ((window (selected-window))
           (parent (window-parent window))
           (top-child (window-top-child parent))
           (next-window (window-next-sibling window))
           (prev-window (window-prev-sibling window)))
      (cond ((not parent))
            (next-window
             (let ((next-window-buffer (window-buffer next-window)))
               (delete-window next-window)
               (set-window-buffer (if top-child
                                      (split-window-horizontally)
                                    (split-window-vertically))
                                  next-window-buffer)))
            (prev-window
             (let ((prev-window-buffer (window-buffer prev-window)))
               (delete-window prev-window)
               (select-window (if top-child
                                  (split-window-horizontally)
                                (split-window-vertically)))
               (set-window-buffer window prev-window-buffer)))))))

(define-minor-mode fixup-whitespace-nospace-mode
  "Advice fixup-whitespace leave nospace."
  :lighter " FWN")

(advice-add 'fixup-whitespace
            :around (lambda (func &rest args)
                      (if fixup-whitespace-nospace-mode
                          (delete-horizontal-space)
                        (apply func args))))

(defvar eshell-buffer-name)
(declare-function eshell-save-some-history "em-hist")
(declare-function eshell-save-some-last-dir "em-dirs")

(defun eshell-dwim (arg)
  "Eshell in new window or other window if called with prefix ARG."
  (interactive "P")
  (require 'eshell)
  (let ((flag t)
        (directory default-directory)
        (buffer-list (buffer-list))
        (window-list (mapcar 'window-buffer (window-list)))
        buffer)
    (while (and flag buffer-list)
      (setq buffer (car buffer-list)
            buffer-list (cdr buffer-list))
      (when (and (eq (with-current-buffer buffer
                       major-mode)
                     'eshell-mode)
                 (string-prefix-p eshell-buffer-name (buffer-name buffer))
                 (not (get-buffer-process buffer))
                 (not (member buffer window-list)))
        (setq flag nil)))
    (if flag
        (progn
          (setq buffer (generate-new-buffer eshell-buffer-name))
          (with-current-buffer buffer
            (eshell-mode)))
      (with-current-buffer buffer
        (eshell/cd directory)
        (eshell-reset)))
    (cond ((and (consp arg) (> (prefix-numeric-value arg) 4))
           (switch-to-buffer buffer))
          (arg
           (switch-to-buffer-other-window buffer))
          (t
           (let ((parent (window-parent (selected-window))))
             (cond ((window-left-child parent)
                    (select-window (split-window-vertically))
                    (switch-to-buffer buffer))
                   ((window-top-child parent)
                    (select-window (split-window-horizontally))
                    (switch-to-buffer buffer))
                   (t
                    (switch-to-buffer-other-window buffer))))))))



(defvar dired-mode-map)

;;;###autoload
(defun simple-x-default-keybindings ()
  (define-key ctl-x-x-map "o" 'xdg-open)
  (with-eval-after-load 'dired
    (define-key dired-mode-map [remap xdg-open] 'dired-do-xdg-open))
  (define-key ctl-x-x-map "=" 'external-format)
  (define-key ctl-x-x-map "^" 'fixup-whitespace-nospace-mode)
  (define-key minibuffer-local-map "\M-." 'minibuffer-yank-symbol)
  (global-set-key (kbd "C-x 9") 'rotate-window))

(provide 'simple-x)
;;; simple-x.el ends here
