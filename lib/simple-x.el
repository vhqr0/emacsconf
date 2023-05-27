;;; simple-x.el --- Simple Extra. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some simple commands collection.

;;; Code:



;; some linux tools wrap

(defvar xclip-command
  (if (eq system-type 'windows-nt)
      "clip.exe"
    "xclip -selection clip"))

(defun xclip (&optional beg end)
  "Xclip or clip.exe on Windows wrap for copy regin (BEG . END)."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (call-shell-region beg end xclip-command)
  (deactivate-mark))

(defvar xdg-open-command-format
  (if (eq system-type 'windows-nt)
      "explorer.exe file://%s"
    "xdg-open %s"))

(defun xdg-open (&optional file)
  "Xdg or explorer.exe wrap for open FILE or current file if called interactively."
  (interactive `(,(or buffer-file-name default-directory)))
  (when (and file (not (file-remote-p file)))
    (call-process-shell-command (format xdg-open-command-format
                                        (expand-file-name file)))))

(declare-function dired-get-marked-files "dired")

(defun dired-do-xdg-open ()
  "`xdg-open' files in Dired."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (xdg-open file)))

(defvar sdcv-command-format "sdcv %s")

(defun sdcv (&optional word)
  (interactive (list (if current-prefix-arg
                         (read-string "word: ")
                       (thing-at-point 'word))))
  (when word
    (shell-command (format sdcv-command-format word))))

(defalias 'sd 'sdcv)

(declare-function grep--save-buffers "grep")

;; -n: display line number, disabled in non-terminal by default
;; -H: display file name, disabled in single-file-search by default
;; --no-heading: display file name inline, disabled by default
(defvar rg-command-prompt "rg -n -H --no-heading ")

(defun rg ()
  "Rg wrap for `grep-mode'."
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: "
                       rg-command-prompt
                       'grep-history)
   'grep-mode))



(defvar clang-format-command "clang-format")
(defvar yapf-command "yapf")
(defvar prettier-command "prettier")

(defun prettier-compute-command ()
  (format "%s --stdin-filepath %s"
          prettier-command
          (or buffer-file-name
              (cond ((derived-mode-p 'js-json-mode)
                     "index.json")
                    ((derived-mode-p 'js-mode)
                     "index.js")
                    ((derived-mode-p 'mhtml-mode)
                     "index.html")
                    ((derived-mode-p 'css-mode)
                     "index.css")
                    (t
                     (user-error "Major mode doesn't support"))))))

(defvar format-dwim-command-alist
  `((c-mode       . ,clang-format-command)
    (c++-mode     . ,clang-format-command)
    (python-mode  . ,yapf-command)
    (js-json-mode . prettier-compute-command)
    (js-mode      . prettier-compute-command)
    (mhtml-mode   . prettier-compute-command)
    (css-mode     . prettier-compute-command)))

(defun format-dwim (beg end)
  (interactive (list (if (use-region-p) (region-beginning) (point-min))
                     (if (use-region-p) (region-end) (point-max))))
  (let ((command (cdr (assq major-mode format-dwim-command-alist))))
    (save-restriction
      (narrow-to-region beg end)
      (if command
          (let ((row (line-number-at-pos)))
            (when (symbolp command)
              (setq command (funcall command)))
            (shell-command-on-region (point-min) (point-max) command nil t)
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

(defun kill-current-buffer-dwim ()
  "Kill current buffer with prompt."
  (interactive)
  (when (y-or-n-p (format "kill current buffer<%s>?" (buffer-name)))
    (kill-buffer)))

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

(defvar eshell-buffer-name)
(declare-function eshell-reset "esh-mode")
(declare-function eshell/cd "em-dirs")

(defun eshell-dwim (arg)
  "Eshell in new window or other window if called with prefix ARG."
  (interactive "P")
  (require 'eshell)
  (let ((directory default-directory)
        (buffer-list (buffer-list))
        (window-list (mapcar 'window-buffer (window-list)))
        found buffer)
    (while (and (not found) buffer-list)
      (setq buffer (car buffer-list)
            buffer-list (cdr buffer-list))
      (when (and (eq (with-current-buffer buffer
                       major-mode)
                     'eshell-mode)
                 (string-prefix-p eshell-buffer-name (buffer-name buffer))
                 (not (get-buffer-process buffer))
                 (not (member buffer window-list)))
        (setq found t)))
    (if found
        (with-current-buffer buffer
          (eshell/cd directory)
          (eshell-reset))
      (setq buffer (generate-new-buffer eshell-buffer-name))
      (with-current-buffer buffer
        (eshell-mode)))
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
  (define-key ctl-x-x-map "c" 'xclip)
  (define-key ctl-x-x-map "o" 'xdg-open)
  (with-eval-after-load 'dired
    (define-key dired-mode-map [remap xdg-open] 'dired-do-xdg-open))
  (define-key minibuffer-local-map "\M-." 'minibuffer-yank-symbol)
  (global-set-key (kbd "C-x 9") 'rotate-window))

(provide 'simple-x)
;;; simple-x.el ends here
