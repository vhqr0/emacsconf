;;; simple-x.el --- Simple Extra. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Some simple commands collection.

;;; Code:



(defvar sdcv-command-format "sdcv %s")

(defun sdcv (&optional word)
  (interactive (list (if current-prefix-arg
                         (read-string "word: ")
                       (thing-at-point 'word))))
  (when word
    (shell-command (format sdcv-command-format word))))

(defalias 'sd 'sdcv)

(defun minibuffer-yank-symbol ()
  "Yank current symbol to minibuffer."
  (interactive)
  (when (window-minibuffer-p)
    (let ((symbol (with-selected-window (minibuffer-selected-window)
                    (thing-at-point 'symbol))))
      (when symbol
        (insert symbol)))))

(defun kill-buffer-dwim (arg)
  "Kill current buffer with prompt."
  (interactive "P")
  (if arg
      (call-interactively 'kill-buffer)
    (let ((c (read-char "Kill current buffer <%s>? [y]" (buffer-name))))
      (if (memq c '(?\r ?y))
          (kill-buffer)
        (message "Aborted")))))

(defun simple-x-default-keybindings ()
  (bind-key "M-." 'minibuffer-yank-symbol minibuffer-local-map))

(provide 'simple-x)
;;; simple-x.el ends here
