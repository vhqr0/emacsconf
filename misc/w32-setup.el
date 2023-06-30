

;;* coding system setup

(prefer-coding-system 'utf-8)

(setq default-process-coding-system '(gbk-dos . gbk-dos)) ; cmd, clip, ...: gbk

(setq process-coding-system-alist
      '(("[rR][gG]" . (utf-8-dos . gbk-dos)))) ; rg: input is gbk and output is utf-8 :-)

(defun +w32-grep-set-coding-system ()   ; `grep' doesn't match `process-coding-system-alist'
  (setq-local default-process-coding-system '(utf-8-dos . gbk-dos)))
(advice-add 'grep-process-setup :after '+w32-grep-set-coding-system)

(defmacro +w32-with-process-coding-system (coding-system &rest body)
  (declare (indent 1))
  `(let ((default-process-coding-system ,coding-system))
     ,@body))

(defun +w32-around-proocess-coding-system-utf-8 (func &rest args)
  (+w32-with-process-coding-system '(utf-8-dos . gbk-dos)
    (apply func args)))

(defun +w32-around-proocess-coding-system-utf-8-both (func &rest args)
  (+w32-with-process-coding-system '(utf-8-dos . utf-8-dos)
    (apply func args)))




;;* program setup

;; for git asking password
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

;; assume using unix tools shipped with git: C:\Program Files\Git\usr\bin
;; fix find resolve to Windows find, rename git find to find2
(setq find-program "find2")

;; formatter: both utf-8
(advice-add 'format-buffer :around '+w32-around-proocess-coding-system-utf-8-both)

;; use wsl sdcv
(setq sdcv-command-format "wsl sdcv %s")
(advice-add 'sdcv :around '+w32-around-proocess-coding-system-utf-8)

;; use wsl ispell
(setq ispell-program-name (expand-file-name "w32-wsl-ispell.bat" +misc-directory))
(advice-add 'ispell-call-process :around '+w32-around-proocess-coding-system-utf-8)
