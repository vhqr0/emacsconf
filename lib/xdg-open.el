(defvar xdg-open-program "xdg-open")

(defvar xdg-open-command-function
  (if (eq system-type 'windows-nt)
      'windows-xdg-open-command
    'linux-xdg-open-command))

(defun linux-xdg-open-command (file)
  (format "%s %s" xdg-open-program (expand-file-name file)))

(defun windows-xdg-open-command (file)
  (format "explorer.exe file://%s" (expand-file-name file)))

;;;###autoload
(defun xdg-open (&optional files)
  (interactive)
  (unless files
    (setq files (cond (buffer-file-name
                       (list buffer-file-name))
                      ((eq major-mode 'dired-mode)
                       (dired-get-marked-files))
                      (default-directory
                       (list default-directory)))))
  (dolist (file files)
    (call-process-shell-command (funcall xdg-open-command-function file))))

(provide 'xdg-open)
