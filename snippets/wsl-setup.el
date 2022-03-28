(when (getenv "WSLENV")
  (setq xclip-program "clip.exe"
        xdg-open-program (expand-file-name "snippets/wsl-xdg-open.py" user-emacs-directory)
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))
