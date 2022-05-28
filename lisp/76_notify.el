;;; 76_notify --- notify for agenda -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Get agenda to give notifications for upcoming items.
;;;
;;; Code:


(use-package! org-wild-notifier
  :after org-agenda
  :config
  (eval-when-compile
    (declare-function org-wild-notifier-mode "org-wild-notifier"))
  (setq alert-default-style 'libnotify
        org-wild-notifier-alert-time '(1)
        org-wild-notifier-notification-title "Org-Agenda"
        org-wild-notifier-notification-icon nil
        org-wild-notifier-keyword-whitelist nil
        org-wild-notifier-keyword-blacklist nil
        org-wild-notifier-tags-whitelist nil
        org-wild-notifier-tags-blacklist nil
        org-wild-notifier-alert-times-property "NOTIFY_BEFORE")
  (org-wild-notifier-mode))

;;; 76_notify ends here
