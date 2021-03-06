;;; +mu4e.el -- ~/.doom.d/mu4e/+mu4e.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Mu4e related functions and config
;;;
;;; Code:

;; Assuming mu4e built from source at the moment
(when (string= (zwei/which-linux-distro) "Debian")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

;; Config

(set-email-account! "fastmail"
                    '((mu4e-sent-folder       . "/fastmail/Sent")
                      (mu4e-drafts-folder     . "/fastmail/Drafts")
                      (mu4e-trash-folder      . "/fastmail/Trash")
                      (mu4e-refile-folder     . "/fastmail/Archive")
                      (smtpmail-smtp-user     . "zweihander@fastmail.com")
                      (user-mail-address      . "zweihander@fastmail.com")
                      )t)
(setq +mu4e-backend 'mbsync
      mu4e-get-mail-command "mbsync -c '${XDG_CONFIG_HOME}/isync/mbsyncrc' --all"
      mu4e-update-interval 1200
      mu4e-context-policy 'pick-first
      mu4e-compose-context-policy nil)
(require 'org-mu4e)
(setq mu4e-org-link-query-in-headers-mode nil)

;;; +mu4e.el ends here
