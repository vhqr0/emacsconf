(when (getenv "WSLENV")
  (setq recentf-exclude '("^/mnt/.*")
        xclip-program "clip.exe"
        xdg-open-program (expand-file-name "misc/wsl-xdg-open.py" user-emacs-directory)
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))
