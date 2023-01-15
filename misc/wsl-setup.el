(when (getenv "WSLENV")
  (setq recentf-exclude '("^/mnt/.*")
        xclip-program "clip.exe"
        xdg-open-program (expand-file-name "wsl-xdg-open.py" +misc-directory)
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))
