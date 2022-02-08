;;; 50_mu4e.el --- mu4e config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Mu4e related functions and config
;;;
;;; Code:

(use-package! mu4e
  :commands (=mu4e)
  :init
  ;; Assuming mu4e built from source at the moment
  (when (string= (zwei/which-linux-distro) "Debian")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))
  (when (string= (zwei/which-linux-distro) "Arch")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))
  :config
  (set-email-account! "fastmail"
                      '((mu4e-sent-folder       . "/fastmail/Sent")
                        (mu4e-drafts-folder     . "/fastmail/Drafts")
                        (mu4e-trash-folder      . "/fastmail/Trash")
                        (mu4e-refile-folder     . "/fastmail/Archive")
                        (smtpmail-smtp-user     . "zweihander@fastmail.com")
                        (user-mail-address      . "zweihander@fastmail.com")
                        )t)
  (setq +mu4e-backend 'mbsync
        mu4e-get-mail-command "mbsync -c \"${XDG_CONFIG_HOME}/isync/mbsyncrc\" --all"
        mu4e-update-interval 1200
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil
        mu4e-org-link-query-in-headers-mode nil))

;;; 50_mu4e.el ends here
