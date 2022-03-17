(require 'erc-backend)

(define-erc-response-handler (CAP)
  "Client capability framework is used to request SASL auth, need
  to wait for ACK to begin" nil
  (let ((msg (erc-response.contents parsed)))
    (when (string-match " *sasl" msg)
      (erc-server-send "AUTHENTICATE PLAIN"))))

(define-erc-response-handler (AUTHENTICATE)
  "Handling empty server response indicating ready to receive
  authentication." nil
  (if erc-session-password
      (let ((msg (erc-response.contents parsed)))
        (when (equal "+" msg)
          (erc-server-send
           (concat "AUTHENTICATE "
                   (base64-encode-string
                    (concat "\0" (erc-current-nick)
                            "\0" erc-session-password)
                    t)))))
    (progn
      (erc-display-message
       parsed 'error
       (if erc-server-connected 'active proc)
       "You must set a password in order to use SASL authentication.")
      (erc-server-send (erc-server-send "AUTHENTICATE *")))))

(define-erc-response-handler (903)
  "Handling a successful SASL authentication." nil
  (erc-server-send "CAP END"))

(defun erc-sasl-login ()
  "Perform user authentication at the IRC server. (PATCHED)"
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
           (erc-current-nick)
           (user-login-name)
           (or erc-system-name (system-name))
           erc-session-server
           erc-session-user-full-name))
  (if erc-session-password
      (erc-server-send (format "PASS %s" erc-session-password))
    (message "Logging in without password"))
  (erc-server-send "CAP REQ :sasl")
  (erc-server-send (format "NICK %s" (erc-current-nick)))
  (erc-server-send
   (format "USER %s %s %s :%s"
       ;; hacked - S.B.
       (if erc-anonymous-login erc-email-userid (user-login-name))
       "0" "*"
       erc-session-user-full-name))
  (erc-update-mode-line))

(advice-add 'erc-login :override 'erc-sasl-login)

(provide 'erc-sasl)
