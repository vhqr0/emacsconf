;;; evil-format.el --- Format operator of evil. -*- lexical-binding: t *-

(require 'evil)

(eval-when-compile
  (require 'evil))

(defvar clang-format-program "clang-format")
(defvar yapf-program "yapf")
(defvar prettier-program "prettier")

(defun prettier-command ()
  (format "%s --stdin-filepath %s"
          prettier-program
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

(defvar format-command-alist
  `((c-mode       . ,clang-format-program)
    (c++-mode     . ,clang-format-program)
    (python-mode  . ,yapf-program)
    (js-json-mode . prettier-command)
    (js-mode      . prettier-command)
    (mhtml-mode   . prettier-command)
    (css-mode     . prettier-command)))

(defun format-region (start end)
  (let ((command (cdr (assq major-mode format-command-alist))))
    (save-restriction
      (narrow-to-region start end)
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

;;;###autoload
(defun format-buffer ()
  (interactive "*")
  (format-region (point-min) (point-max)))

(evil-define-operator evil-operator-format (start end)
  :move-point nil
  (interactive "<r>")
  (format-region start end))

(provide 'evil-format)
