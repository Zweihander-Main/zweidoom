;;; 50_mu4e.el --- mu4e config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Mu4e related functions and config
;;;
;;; Code:

(use-package! mu4e
  :commands (=mu4e mu4e mu4e-compose-new)
  :config
  ;; Assuming mu4e built from source at the moment on Debian
  (when (string= (zwei/which-linux-distro) "Debian")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))
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

;; TEMP Fix mu incompat
(after! mu4e
  (defalias 'mu4e~start 'mu4e--start)
  (defalias 'mu4e~stop 'mu4e--stop)
  (defalias 'mu4e~check-requirements 'mu4e--check-requirements)
  (defalias 'mu4e~proc-sentinel 'mu4e--server-sentinel)
  (defalias 'mu4e~proc-start 'mu4e--server-start)
  (defalias 'mu4e~proc-eat-sexp-from-buf 'mu4e--server-eat-sexp-from-buf)
  (defalias 'mu4e~pong-handler 'mu4e--pong-handler)
  (defalias 'mu4e~main-view 'mu4e--main-view)
  (defalias 'mu4e~main-view-real 'mu4e--main-view-real)
  (defalias 'mu4e~main-view-real-1 'mu4e--main-view-real-1)
  (defalias 'mu4e~main-menu 'mu4e--main-menu)
  (defalias 'mu4e~main-action-str 'mu4e--main-action-str)
  (defalias 'mu4e~main-redraw-buffer 'mu4e--main-redraw-buffer)
  (defalias 'mu4e~proc-move 'mu4e--server-move)
  (defalias 'mu4e~mark-check-target 'mu4e--mark-check-target)
  (defalias 'mu4e~maildirs-with-query 'mu4e--maildirs-with-query)
  (defalias 'mu4e~longest-of-maildirs-and-bookmarks 'mu4e--longest-of-maildirs-and-bookmarks)
  (defalias 'mu4e~update-timer 'mu4e--update-timer)
  (defalias 'mu4e~proc-remove 'mu4e--server-remove))
;;; 50_mu4e.el ends here
