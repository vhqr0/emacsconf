(require 'cmake-mode)

(defvar cmake-capf-lists
  (append
   (cmake-get-list "command")
   (cmake-get-list "variable")
   (cmake-get-list "module")
   (cmake-get-list "property")))


;;;###autoload
(defun cmake-capf ()
  (let* ((symbol (cmake-symbol-at-point))
         (beg (save-excursion
                (search-backward symbol)
                (point)))
         (end (point)))
    `(,beg ,end ,cmake-capf-lists :exclusive 'no)))

;;;###autoload
(with-eval-after-load 'cmake-mode
  (add-hook 'cmake-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cmake-capf nil 'local))))

(provide 'cmake-capf)
