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
  (interactive)
  (let ((word (or word (thing-at-point 'word))))
    (when word
      (shell-command (concat sdcv-program " " word)))))

(declare-function grep--save-buffers "grep")

(defvar rg-program "rg")

(defun rg ()
  "Ripgrep wrap for `grep-mode'."
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: "
                       (concat rg-program " --no-heading ")
                       'grep-history)
   'grep-mode))

(defvar formater-program-alist
  '((c-mode . "clang-format")
    (c++-mode . "clang-format")
    (python-mode . "black -q -")))

(defun formater ()
  (interactive)
  (let ((program (cdr (assq major-mode formater-program-alist))))
    (if program
        (let ((row (line-number-at-pos))
              (col (current-column))
              (beg (if (use-region-p) (region-beginning) (point-min)))
              (end (if (use-region-p) (region-end) (point-max))))
          (shell-command-on-region beg end program nil t)
          (goto-char (point-min))
          (forward-line (1- row))
          (forward-char (min col
                             (- (line-end-position)
                                (line-beginning-position)))))
      (delete-trailing-whitespace (if (use-region-p) (region-beginning) (point-min))
                                  (if (use-region-p) (region-end) (point-max)))
      (indent-region (if (use-region-p) (region-beginning) (point-min))
                     (if (use-region-p) (region-end) (point-max))))))



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

(defvar toggle-letter-case-state 0)

(defun toggle-letter-case ()
  (interactive)
  (save-mark-and-excursion
    (deactivate-mark)
    (let ((beg (progn
                 (skip-chars-backward "[:alpha:]")
                 (point)))
          (end (progn
                 (skip-chars-forward "[:alpha:]")
                 (point))))
      (when (not (eq last-command this-command))
        (setq toggle-letter-case-state 0))
      (cond
       ((eq toggle-letter-case-state 0)
        (upcase-initials-region beg end)
        (setq toggle-letter-case-state 1))
       ((eq toggle-letter-case-state 1)
        (upcase-region beg end)
        (setq toggle-letter-case-state 2))
       ((eq toggle-letter-case-state 2)
        (downcase-region beg end)
        (setq toggle-letter-case-state 0))))))

(defvar eshell-buffer-name)

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
    (when flag
      (setq buffer (generate-new-buffer eshell-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory directory)
      (widen)
      (goto-char (point-max))
      (eshell-mode))
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
    (define-key dired-mode-map "V" 'dired-do-xdg-open))
  (define-key ctl-x-x-map "=" 'formater)
  (define-key minibuffer-local-map "\M-." 'minibuffer-yank-symbol)
  (global-set-key (kbd "C-x 9") 'rotate-window)
  (global-set-key "\M-E" 'eshell-dwim)
  (global-set-key "\M-B" 'toggle-letter-case))

(provide 'simple-x)
;;; simple-x.el ends here
