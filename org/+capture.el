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
  (setq zweigtd-reviews-goals-file zwei/org-agenda-goals-file
        zweigtd-reviews-goals-list (zweigtd-goals-get-goals))
  (zweigtd-reviews-init))

;;; +capture.el ends here
