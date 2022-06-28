(setq +package (append +package '(emmet-mode company-web)))

(setq prettier-program "prettier")

(defun prettier-command (default-file-path)
  (format "%s --stdin-filepath %s"
          prettier-program
          (or buffer-file-name default-file-path)))

(defun sgml-mode-setup ()
  (setq-local company-backends
              (append '(company-web-html) company-backends)
              external-format-program-alist
              `((,major-mode
                 . ,(prettier-command "index.html"))))
  (company-mode 1)
  (emmet-mode 1))

(defun js-mode-setup ()
  (setq-local external-format-program-alist
              `((,major-mode
                 . ,(prettier-command "index.jsx"))))
  (company-mode 1)
  (emmet-mode 1))

(add-hook 'sgml-mode-hook 'sgml-mode-setup)
(add-hook 'js-mode-hook 'js-mode-setup)
