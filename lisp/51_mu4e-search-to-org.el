;;; 51_mu4e-search-to-org.el --- mu4e search pkg-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Configures first party package `mu4e-search-to-org'.
;;;
;;; Code:

(use-package! mu4e-search-to-org
  :commands (mu4e-search-to-org-script-mode mu4e-search-to-org-search)
  :after mu4e
  :config
  (setq mu4e-search-to-org-file-for-output zwei/org-agenda-todo-file
        mu4e-search-to-org-context "fastmail"
        mu4e-search-to-org-search-bookmark
        "flag:unread AND NOT flag:trashed AND maildir:/fastmail/memo")
    (mu4e-search-to-org-script-mode t))

;;; 51_mu4e-search-to-org ends here
