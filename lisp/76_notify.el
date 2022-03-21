;;; 76_notify --- notify for agenda -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Get agenda to give notifications for upcoming items.
;;;
;;; Code:

(use-package! org-alert
  :after org-agenda
  :config
  (setq alert-default-style 'libnotify
        org-alert-match-string (concat "SCHEDULED>=\"<today>\""
                                       "+SCHEDULED<\"<tomorrow>\""
                                       "|DEADLINE>=\"<today>\""
                                       "+DEADLINE<\"<tomorrow>\""
                                       "TIMESTAMP>=\"<today>\""
                                       "+TIMESTAMP<\"<tomorrow>\""))
  (org-alert-enable))

;;; 76_notify ends here
