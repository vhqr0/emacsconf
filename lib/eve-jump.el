(require 'eve)

(defvar eve-jump-forward-chars
  (append '(?f ?j ?g ?h ?a)
          '(?r ?u ?t ?y ?q)
          '(?v ?m ?b ?n ?z)
          '(?4 ?7 ?5 ?6 ?1)
          '(?F ?J ?G ?H ?A)
          '(?R ?U ?T ?Y ?Q)
          '(?V ?M ?B ?N ?Z)
          '(?$ ?& ?% ?^ ?!)
          '(?' ?` ?\[ ?\] ?\\ ?- ?=)))

(defvar eve-jump-backward-chars
  (append '(?d ?k ?s ?l ?\;)
          '(?e ?i ?w ?o ?p)
          '(?c ?, ?x ?. ?/)
          '(?3 ?8 ?2 ?9 ?0)
          '(?D ?K ?S ?L ?:)
          '(?E ?I ?W ?O ?P)
          '(?C ?< ?X ?> ??)
          '(?# ?* ?@ ?\( ?\))
          '(?\" ?~ ?\{ ?\} ?| ?_ ?+)))

(defvar-local eve-jump-forward-overlays nil)

(defvar-local eve-jump-backward-overlays nil)

(defun eve-jump-make-overlay (pos)
  (let ((overlay (make-overlay pos (1+ pos))))
    (overlay-put overlay 'window (selected-window))
    overlay))

(defun eve-jump-put-char (chars overlays)
  (let ((fin (car chars))
        (chars (cdr chars))
        overlay char)
    (while overlays
      (setq overlay (car overlays)
            overlays (cdr overlays))
      (if chars
          (setq char (car chars)
                chars (cdr chars))
        (setq char fin))
      (overlay-put overlay
                   (if (string-equal
                        (buffer-substring
                         (overlay-start overlay)
                         (overlay-end overlay))
                        "\n")
                       'before-string
                     'display)
                   (propertize (char-to-string char) 'face 'match))
      (overlay-put overlay 'char char))))

(defun eve-jump-narrow (char overlays)
  (let (overlay noverlays)
    (while overlays
      (setq overlay (car overlays)
            overlays (cdr overlays))
      (if (eq (overlay-get overlay 'char) char)
          (setq noverlays (cons overlay noverlays))
        (delete-overlay overlay)))
    (nreverse noverlays)))

(defun eve-jump-jump ()
  (cond ((not (or eve-jump-forward-overlays
                  eve-jump-backward-overlays))
         (user-error "No such char"))
        ((and (not eve-jump-backward-overlays)
              (car eve-jump-forward-overlays)
              (not (cdr eve-jump-forward-overlays)))
         (goto-char (overlay-start (car eve-jump-forward-overlays)))
         (delete-overlay (car eve-jump-forward-overlays))
         (setq eve-jump-forward-overlays nil))
        ((and (not eve-jump-forward-overlays)
              (car eve-jump-backward-overlays)
              (not (cdr eve-jump-backward-overlays)))
         (goto-char (overlay-start (car eve-jump-backward-overlays)))
         (delete-overlay (car eve-jump-backward-overlays))
         (setq eve-jump-backward-overlays nil))
        (t
         (let ((char (read-char)))
           (setq eve-jump-forward-overlays
                 (eve-jump-narrow char eve-jump-forward-overlays)
                 eve-jump-backward-overlays
                 (eve-jump-narrow char eve-jump-backward-overlays)))
         (eve-jump-put-char eve-jump-forward-chars eve-jump-forward-overlays)
         (eve-jump-put-char eve-jump-backward-chars eve-jump-backward-overlays)
         (eve-jump-jump))))

(defun eve-jump-goto-regexp (regexp)
  (let ((beg (window-start))
        (end (window-end)))
    (save-excursion
      (condition-case nil
          (let ((ctn (not (eobp))))
            (while ctn
              (forward-char)
              (re-search-forward regexp)
              (re-search-backward regexp)
              (if (< (point) end)
                  (setq eve-jump-forward-overlays
                        (cons (eve-jump-make-overlay (point))
                              eve-jump-forward-overlays))
                (setq ctn nil))))
        (search-failed nil)))
    (save-excursion
      (condition-case nil
          (let ((ctn t))
            (while ctn
              (re-search-backward regexp)
              (if (>= (point) beg)
                  (setq eve-jump-backward-overlays
                        (cons (eve-jump-make-overlay (point))
                              eve-jump-backward-overlays))
                (setq ctn nil))))
        (search-failed nil)))
    (setq eve-jump-forward-overlays (nreverse eve-jump-forward-overlays)
          eve-jump-backward-overlays (nreverse eve-jump-backward-overlays))
    (eve-jump-put-char eve-jump-forward-chars eve-jump-forward-overlays)
    (eve-jump-put-char eve-jump-backward-chars eve-jump-backward-overlays)
    (when (or eve-jump-forward-overlays
              eve-jump-backward-overlays)
      (unwind-protect
          (eve-jump-jump)
        (mapc 'delete-overlay eve-jump-forward-overlays)
        (mapc 'delete-overlay eve-jump-backward-overlays)
        (setq eve-jump-forward-overlays nil
              eve-jump-backward-overlays nil)))))

(defvar eve-jump-last ?$)

(eve-define-exclusive-motion "gf"
  (unless eve-repeat-flag
    (setq eve-jump-last (read-char)))
  (eve-jump-goto-regexp (regexp-quote (char-to-string eve-jump-last))))

(eve-define-exclusive-motion "gw"
  (eve-jump-goto-regexp "\\_<\\sw"))

(eve-define-line-motion "gj"
  (eve-jump-goto-regexp "^.\\|^\n"))

(provide 'eve-jump)

;;;###autoload
(with-eval-after-load 'eve
  (require 'eve-jump))
