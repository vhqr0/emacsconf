(defun eshell-setup-pager ()
  (setenv "PAGER" (expand-file-name "pager.py" +misc-directory)))

(add-hook 'eshell-mode-hook 'eshell-setup-pager)
