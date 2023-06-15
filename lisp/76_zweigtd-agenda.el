;;; 76_zweigtd-agenda --- zweigtd-agenda config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to zweigtd-agenda
;;;
;;; Code:


(map! :map org-agenda-mode-map
      :localleader
      :desc "Process inbox items" "i" #'zweigtd-agenda-process-inbox-all-items
      :desc "Process all links" "L" #'zweigtd-agenda-process-inbox-open-and-archive-all-links
      :desc "Process current item" "P" #'zweigtd-agenda-process-inbox-single-item)

(after! org-agenda
  (setq org-agenda-bulk-custom-functions
        `((?c zweigtd-agenda-process-inbox-single-item))))

(use-package! zweigtd-agenda-process-inbox
  :commands (zweigtd-agenda-process-inbox-all-items
             zweigtd-agenda-process-inbox-open-and-archive-all-links
             zweigtd-agenda-process-inbox-single-item
             zweigtd-agenda-heading-functions-break-into-child
             zweigtd-agenda-heading-functions-saved-effort)
  :config
  (setq zweigtd-agenda-process-inbox-category ""
        zweigtd-agenda-process-inbox-next-file zwei/org-agenda-next-file
        zweigtd-agenda-process-inbox-refile-target-info
        '((zwei/org-agenda-projects-file :maxlevel . 2)
          (zwei/org-agenda-tickler-file :maxlevel . 2)
          (zwei/org-agenda-next-file :level . 1 ))))


;;; 76_zweigtd-agenda ends here
