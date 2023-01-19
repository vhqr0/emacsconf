

;;* coding system setup

(prefer-coding-system 'utf-8)

(setq default-process-coding-system '(gbk-dos . gbk-dos)) ; cmd, clip, ...: gbk

(setq process-coding-system-alist
      '(("[rR][gG]" . (utf-8-dos . gbk-dos)))) ; rg: input is gbk and output is utf-8 :-)

(defun +w32-grep-set-coding-system ()   ; `grep' doesn't match `process-coding-system-alist'
  (setq-local default-process-coding-system '(utf-8-dos . gbk-dos)))
(advice-add 'grep-process-setup :after '+w32-grep-set-coding-system)

(defmacro +w32-with-process-coding-system-utf-8 (&rest body)
  `(let ((default-process-coding-system '(utf-8-dos . gbk-dos)))
     ,@body))

(defun +w32-around-proocess-coding-system-utf-8 (func &rest args)
  (+w32-with-process-coding-system-utf-8
   (apply func args)))



;;* program setup

;; for git asking password
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

;; assume using unix tools shipped with git: C:\Program Files\Git\usr\bin
;; fix find resolve to Windows find, rename git find to find2
(setq find-program "find2")

;; use wsl sdcv
(setq sdcv-program "wsl sdcv")
(advice-add 'sdcv :around '+w32-around-proocess-coding-system-utf-8)
