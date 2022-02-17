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
    (when program
      (let ((row (line-number-at-pos))
            (col (current-column))
            (beg (if (use-region-p) (region-beginning) (point-min)))
            (end (if (use-region-p) (region-end) (point-max))))
        (shell-command-on-region beg end program nil t)
        (goto-char (point-min))
        (forward-line (1- row))
        (forward-char (min col
                           (- (line-end-position)
                              (line-beginning-position))))))))



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



;; some list commands

(defvar list-misc-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "y" 'list-kill-ring)
    (define-key map "m" 'list-global-mark-ring)
    (define-key map "i" 'list-imenu)
    map))

(defvar list-misc-text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    map))

(defvar list-misc-mark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" 'list-misc-display-mark)
    (define-key map "\r"   'list-misc-goto-mark)
    (define-key map "o"    'list-misc-goto-mark-other-window)
    map))

(define-derived-mode list-misc-text-mode text-mode "List Text"
  "Major mode for list text based elements.")

(define-derived-mode list-misc-mark-mode special-mode "List Mark"
  "Major mode for list mark based elements.")

(defun list-misc-display-mark ()
  "Display current mark."
  (interactive)
  (let ((pos (get-char-property (point) :pos)))
    (when (markerp pos)
      (with-selected-window (display-buffer (marker-buffer pos))
        (goto-char pos)))))

(defun list-misc-goto-mark ()
  "Goto current mark."
  (interactive)
  (let ((pos (get-char-property (point) :pos)))
    (when (markerp pos)
      (switch-to-buffer (marker-buffer pos))
      (goto-char pos))))

(defun list-misc-goto-mark-other-window ()
  "Goto current mark other window."
  (interactive)
  (let ((pos (get-char-property (point) :pos)))
    (when (markerp pos)
      (switch-to-buffer-other-window (marker-buffer pos))
      (goto-char pos))))

(defun list-kill-ring ()
  "List `kill-ring'."
  (interactive)
  (switch-to-buffer-other-window "*list-kill-ring*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (text kill-ring)
      (insert text "\n\n\C-l\n\n")))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (list-misc-text-mode))

(defun list-global-mark-ring (arg)
  "List `global-mark-ring' and `mark-ring' if called with prefix ARG."
  (interactive "P")
  (let ((marks (if arg
                   global-mark-ring
                 (append mark-ring global-mark-ring))))
    (switch-to-buffer-other-window "*list-global-mark-ring*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (mark marks)
        (let ((buffer (marker-buffer mark)))
          (when buffer
            (insert (propertize (concat (buffer-name buffer)
                                        ":"
                                        (number-to-string (marker-position mark))
                                        ":"
                                        (with-current-buffer buffer
                                          (save-excursion
                                            (goto-char mark)
                                            (buffer-substring (line-beginning-position)
                                                              (line-end-position))))
                                        "\n")
                                :pos mark)))))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (list-misc-mark-mode))))

(defvar imenu-auto-rescan)
(declare-function imenu--subalist-p "imenu")
(declare-function imenu--make-index-alist "imenu")

(defun list-imenu-alist (buffer alist &optional prefix)
  "List imenu sub ALIST of BUFFER with PREFIX."
  (dolist (index alist)
    (if (imenu--subalist-p index)
        (list-imenu-alist buffer
                          (cdr index)
                          (if prefix
                              (concat prefix "::" (car index))
                            (car index)))
      (insert (propertize (concat (if prefix
                                      (concat prefix "::" (car index))
                                    (car index))
                                  "\n")
                          :pos
                          (let ((pos (cdr index)))
                            (cond ((integerp pos)
                                   (with-current-buffer buffer
                                     (copy-marker pos)))
                                  ((overlayp pos)
                                   (with-current-buffer buffer
                                     (copy-marker (overlay-start pos))))
                                  (t
                                   pos))))))))

(defun list-imenu (arg)
  "List imenu, force rescan if called with prefix ARG."
  (interactive "P")
  (require 'imenu)
  (let* ((imenu-auto-rescan arg)
         (buffer (current-buffer))
         (alist (imenu--make-index-alist t)))
    (switch-to-buffer "*list-imenu*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (list-imenu-alist buffer (cdr alist)))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (list-misc-mark-mode)))

(defvar dired-mode-map)

;;;###autoload
(defun simple-x-default-keybindings ()
  (define-key ctl-x-x-map "o" 'xdg-open)
  (with-eval-after-load 'dired
    (define-key dired-mode-map "V" 'dired-do-xdg-open))
  (global-set-key (kbd "C-c f") 'formater)
  (define-key minibuffer-local-map "\M-." 'minibuffer-yank-symbol)
  (global-set-key (kbd "C-x 9") 'rotate-window)
  (global-set-key "\M-E" 'eshell-dwim)
  (global-set-key (kbd "C-x l") list-misc-prefix-map))

(provide 'simple-x)
;;; simple-x.el ends here
