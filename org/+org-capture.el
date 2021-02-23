;;; +org-capture.el -- ~/.doom.d/org/+org-capture.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Config related to org capture and reviews called from it.
;;;
;;; Code:

(setq org-capture-templates
      `(("i" "inbox"
         entry
         (file ,zwei/org-agenda-todo-file)
         "* TODO %?")
        ("n" "next"
         entry
         (file ,zwei/org-agenda-next-file)
         "* NEXT %? %^g:@work: %^{Effort}p ")
        ("c" "org-protocol-capture"
         entry
         (file ,zwei/org-agenda-todo-file)
         "* TODO [[%:link][%:description]]\n\n %i"
         :immediate-finish t)
        ("r" "Review templates")
        ("rm" "Monthly Review"
         entry
         (function zwei/reviews-position-monthly-template)
         (function zwei/reviews-generate-monthly-template)
         :jump-to-captured t
         :tree-type 'monthly
         :immediate-finish nil)
        ("rw" "Weekly Review"
         entry
         (file+olp+datetree ,zwei/org-agenda-reviews-file "Weekly Reviews")
         (file ,zwei/org-agenda-weekly-review-template-file)
         :jump-to-captured t
         :time-prompt t
         :tree-type 'week
         :immediate-finish nil)
        ("rd" "Daily Review"
         entry
         (function (lambda ()
                     (org-journal-new-entry nil)
                     (insert "Daily Review")))
         (file ,zwei/org-agenda-daily-review-template-file)
         :jump-to-captured t
         :immediate-finish nil)))

;;; +org-capture.el ends here
