(prefer-coding-system 'utf-8)

(setq default-process-coding-system '(gbk-dos . gbk-dos)) ; cmd, clip, ...: gbk

(setq process-coding-system-alist
      '(("[rR][gG]" . (utf-8-dos . gbk-dos)))) ; rg: input is gbk and output is utf-8 :-)

(defun +w32-grep-set-coding-system ()   ; `grep' doesn't match `process-coding-system-alist'
  (setq-local default-process-coding-system '(utf-8-dos . gbk-dos)))
(advice-add 'grep-process-setup :after '+w32-grep-set-coding-system)

;; for git asking password
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")
