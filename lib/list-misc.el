;;;###autoload
(defvar list-misc-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "y" 'list-kill-ring)
    (define-key map "r" 'list-history)
    (define-key map "m" 'list-global-mark-ring)
    (define-key map "i" 'list-imenu)
    map))

(defvar list-misc-text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    map))

(defvar list-misc-mark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'list-misc-goto-mark)
    (define-key map "o"  'list-misc-goto-mark-other-window)
    map))

(define-derived-mode list-misc-text-mode text-mode "List Text"
  "Major mode for list text based elements.")

(define-derived-mode list-misc-mark-mode special-mode "List Mark"
  "Major mode for list mark based elements.")

(defun list-misc-goto-mark ()
  (interactive)
  (let ((pos (get-char-property (point) :pos)))
    (when (markerp pos)
      (switch-to-buffer (marker-buffer pos))
      (goto-char pos))))

(defun list-misc-goto-mark-other-window ()
  (interactive)
  (let ((pos (get-char-property (point) :pos)))
    (when (markerp pos)
      (switch-to-buffer-other-window (marker-buffer pos))
      (goto-char pos))))

;;;###autoload
(defun list-kill-ring ()
  (interactive)
  (switch-to-buffer-other-window "*list-kill-ring*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (text kill-ring)
      (insert text "\n\n\C-l\n\n")))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (list-misc-text-mode))

(defvar comint-input-ring)
(defvar eshell-history-ring)
(declare-function ring-elements "ring")

;;;###autoload
(defun list-history ()
  (interactive)
  (let ((historys (cond ((derived-mode-p 'comint-mode)
                         comint-input-ring)
                        ((eq major-mode 'eshell-mode)
                         eshell-history-ring)
                        (t
                         (error "Unknown history type")))))
    (switch-to-buffer-other-window "*list-history*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (history (ring-elements historys))
        (insert history "\n\n\n\n"))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (list-misc-text-mode))))

;;;###autoload
(defun list-global-mark-ring (arg)
  (interactive "P")
  (let ((marks (if arg
                   global-mark-ring
                 (append mark-ring global-mark-ring))))
    (switch-to-buffer-other-window "*list-global-mark-ring*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (mark marks)
        (insert (propertize (concat (buffer-name (marker-buffer mark))
                                    ":"
                                    (number-to-string (marker-position mark))
                                    ":"
                                    (with-current-buffer (marker-buffer mark)
                                      (save-excursion
                                        (goto-char mark)
                                        (buffer-substring (line-beginning-position) (line-end-position))))
                                    "\n")
                            :pos mark)))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (list-misc-mark-mode))))

(defvar imenu-auto-rescan)
(declare-function imenu--subalist-p "imenu")
(declare-function imenu--make-index-alist "imenu")

(defun list-imenu-alist (buffer alist &optional prefix)
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

;;;###autoload
(defun list-imenu (arg)
  (interactive "P")
  (require 'imenu)
  (let* ((buffer (current-buffer))
         (imenu-auto-rescan arg)
         (alist (imenu--make-index-alist t)))
    (switch-to-buffer "*list-imenu*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (list-imenu-alist buffer (cdr alist)))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (list-misc-mark-mode)))

(provide 'list-misc)
