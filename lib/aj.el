(defvar aj-forward-chars
  (append '(?f ?j ?g ?h ?a)
          '(?r ?u ?t ?y ?q)
          '(?v ?m ?b ?n ?z)
          '(?4 ?7 ?5 ?6 ?1)
          '(?F ?J ?G ?H ?A)
          '(?R ?U ?T ?Y ?Q)
          '(?V ?M ?B ?N ?Z)
          '(?$ ?& ?% ?^ ?!)
          '(?' ?` ?\[ ?\] ?\\ ?- ?=)))

(defvar aj-backward-chars
  (append '(?d ?k ?s ?l ?\;)
          '(?e ?i ?w ?o ?p)
          '(?c ?, ?x ?. ?/)
          '(?3 ?8 ?2 ?9 ?0)
          '(?D ?K ?S ?L ?:)
          '(?E ?I ?W ?O ?P)
          '(?C ?< ?X ?> ??)
          '(?# ?* ?@ ?\( ?\))
          '(?\" ?~ ?\{ ?\} ?| ?_ ?+)))

(defvar-local aj-overlays nil)

(defun aj-clear-overlays ()
  (mapc 'delete-overlay
        aj-overlays)
  (setq aj-overlays nil))

(defun aj-make-overlay (pos char)
  (let ((window (selected-window))
        (overlay (make-overlay pos (1+ pos))))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'before-string (propertize (char-to-string char) 'face 'highlight))
    (overlay-put overlay 'char char)
    (setq aj-overlays (cons overlay aj-overlays))))

(defun aj-goto-overlay (char)
  (let ((ctn t)
        (cur aj-overlays))
    (while (and ctn cur)
      (when (eq (overlay-get (car cur) 'char) char)
        (setq ctn nil)
        (goto-char (overlay-start (car cur))))
      (setq cur (cdr cur)))
    (when ctn
      (user-error "No such char"))))

;;;###autoload
(defun aj-goto-line ()
  (interactive)
  (let ((beg (window-start))
        (end (window-end)))
    (save-excursion
      (let ((ctn t)
            (cur aj-forward-chars))
        (while (and ctn cur)
          (goto-char (line-end-position))
          (forward-char)
          (if (< (point) end)
              (aj-make-overlay (point) (car cur))
            (setq ctn nil))
          (setq cur (cdr cur)))))
    (save-excursion
      (let ((ctn t)
            (cur aj-backward-chars))
        (while (and ctn cur)
          (backward-char)
          (goto-char (line-beginning-position))
          (if (> (point) beg)
              (aj-make-overlay (point) (car cur))
            (setq ctn nil))
          (setq cur (cdr cur))))))
  (when aj-overlays
    (unwind-protect
        (aj-goto-overlay (read-char))
      (aj-clear-overlays))))

;;;###autoload
(defun aj-goto-symbol ()
  (interactive)
  (let ((beg (window-start))
        (end (window-end)))
    (save-excursion
      (let ((ctn t)
            (cur aj-forward-chars))
        (while (and ctn cur)
          (forward-symbol 2)
          (forward-symbol -1)
          (if (< (point) end)
              (aj-make-overlay (point) (car cur))
            (setq ctn nil))
          (setq cur (cdr cur)))))
    (save-excursion
      (let ((ctn t)
            (cur aj-backward-chars))
        (while (and ctn cur)
          (forward-symbol -1)
          (if (> (point) beg)
              (aj-make-overlay (point) (car cur))
            (setq ctn nil))
          (setq cur (cdr cur))))))
  (when aj-overlays
    (unwind-protect
        (aj-goto-overlay (read-char))
      (aj-clear-overlays))))

;;;###autoload
(defun aj-goto-char (&optional char)
  (interactive)
  (let ((regexp (regexp-quote (char-to-string (or char (read-char)))))
        (beg (window-start))
        (end (window-end)))
    (save-excursion
      (let ((ctn t)
            (cur aj-forward-chars))
        (condition-case nil
            (while (and ctn cur)
              (forward-char)
              (re-search-forward regexp)
              (re-search-backward regexp)
              (if (< (point) end)
                  (aj-make-overlay (point) (car cur))
                (setq ctn nil))
              (setq cur (cdr cur)))
          (search-failed nil))))
    (save-excursion
      (let ((ctn t)
            (cur aj-backward-chars))
        (condition-case nil
            (while (and ctn cur)
              (re-search-backward regexp)
              (if (> (point) beg)
                  (aj-make-overlay (point) (car cur))
                (setq ctn nil))
              (setq cur (cdr cur)))
          (search-failed nil)))))
  (when aj-overlays
    (unwind-protect
        (aj-goto-overlay (read-char))
      (aj-clear-overlays))))

(provide 'aj)