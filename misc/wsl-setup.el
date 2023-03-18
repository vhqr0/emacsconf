(when (getenv "WSLENV")
  (setq recentf-exclude '("^/mnt/.*")
        xclip-command "clip.exe"
        xdg-open-command-format (concat (expand-file-name "wsl-xdg-open.py" +misc-directory) " %s")
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))

(defun +wsl-eshell-cleanup-path ()
  (interactive)
  (let* ((path (getenv "PATH"))
         (path (split-string path ":"))
         (path (seq-filter
                (lambda (x) (not (string-prefix-p "/mnt/" x)))
                path))
         (path (string-join path ":")))
    (eshell/export (concat "PATH=" path))))

(add-hook 'eshell-mode-hook '+wsl-eshell-cleanup-path)
