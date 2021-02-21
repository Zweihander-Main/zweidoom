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
        ("rw" "Weekly Review"
         entry
         (file+olp+datetree ,zwei/org-agenda-reviews-file)
         (file ,zwei/org-agenda-weekly-review-template-file)
         :jump-to-captured t
         :tree-type 'week
         :immediate-finish nil)
        ("rd" "Daily Review"
         entry
         (function (lambda () (org-journal-new-entry nil)))
         (file ,zwei/org-agenda-daily-review-template-file)
         :jump-to-captured t
         :immediate-finish nil)))

(defun zwei/org-capture-goal-extract (goal)
  "Return GOAL todo in the format:
todo text by yyyy-mm-dd
Will return \"\" if goal is \"OTHER\"."
  (if (string= goal "OTHER") ""
    (let* ((headline
            (plist-get
             (car
              (org-ql-query
                :from zwei/org-agenda-goals-file
                :where `(and (todo "TODO")
                             (parent ,goal))))
             'headline))
           (raw (plist-get headline ':raw-value))
           (scheduled (plist-get headline ':scheduled)))
      (if scheduled
          (concat raw " by " (org-timestamp-format scheduled "%Y-%m-%d"))
        raw))))


;;; +org-capture.el ends here
