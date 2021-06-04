;;; +capture.el -- doom/org/+capture.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to org capture and reviews called from it.
;;;
;;; Code:

(require 'org)
(require 'org-capture)

;; Functions

(defun zwei/org-inbox-capture ()
  "Shortcut to org-capture->inbox."
  (interactive)
  "Capture a an inbox task."
  (org-capture nil "i"))

;; Mappings

(map! :leader
      :prefix "n"
      :desc "Inbox entry" "i" #'zwei/org-inbox-capture)

;; Config
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
         :immediate-finish t)))


(use-package! zweigtd-reviews
  :config
  (setq zweigtd-reviews-file zwei/org-agenda-reviews-file
        zweigtd-reviews-daily-review-template zwei/org-agenda-daily-review-template-file
        zweigtd-reviews-weekly-review-template zwei/org-agenda-weekly-review-template-file
        zweigtd-reviews-monthly-review-template zwei/org-agenda-monthly-review-template-file)
  (zweigtd-reviews-default-bootstrap))


;;; +capture.el ends here
