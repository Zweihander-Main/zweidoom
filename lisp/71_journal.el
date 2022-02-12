;;; 71_journal --- org-journal config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to org-journal.
;;;
;;; Code:

(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t))

;;; 71_journal ends here
