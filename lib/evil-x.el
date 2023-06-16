;;; evil-x.el --- Extra operator/textobject for evil. -*- lexical-binding: t *-

(require 'evil)
(require 'thingatpt)

(eval-when-compile
  (require 'evil)
  (require 'cl-macs)
  (require 'bind-key))

(evil-define-operator evil-operator-comment (start end)
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region start end))

(evil-define-operator evil-operator-narrow (start end)
  :move-point nil
  (interactive "<r>")
  (narrow-to-region start end))

(evil-define-text-object evil-textobject-line (count &optional start end type)
  (evil-range (line-beginning-position) (line-end-position) 'exclusive))

(evil-define-text-object evil-textobject-filename (count &optional start end type)
  (cl-destructuring-bind (start . end)
      (bounds-of-thing-at-point 'filename)
    (evil-range start end)))

(evil-define-text-object evil-textobject-defun (count &optional start end type)
  (cl-destructuring-bind (start . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range start end 'line)))

(evil-define-text-object evil-textobject-page (count &optional start end type)
  (cl-destructuring-bind (start . end)
      (bounds-of-thing-at-point 'page)
    (evil-range start end 'line)))

(evil-define-text-object evil-textobject-entire (count &optional start end type)
  (evil-range (point-min) (point-max) 'line))

(bind-keys
 :map evil-normal-state-map
 ("gc" . evil-operator-comment)
 :map evil-motion-state-map
 ("g-" . evil-operator-narrow)
 :map evil-inner-text-objects-map
 ("l" . evil-textobject-line)
 ("F" . evil-textobject-filename)
 ("u" . evil-textobject-filename)
 ("f" . evil-textobject-defun)
 ("d" . evil-textobject-defun)
 ("P" . evil-textobject-page)
 ("g" . evil-textobject-entire)
 ("h" . evil-textobject-entire)
 :map evil-outer-text-objects-map
 ("l" . evil-textobject-line)
 ("F" . evil-textobject-filename)
 ("u" . evil-textobject-filename)
 ("f" . evil-textobject-defun)
 ("d" . evil-textobject-defun)
 ("P" . evil-textobject-page)
 ("g" . evil-textobject-entire)
 ("h" . evil-textobject-entire))

(provide 'evil-x)
