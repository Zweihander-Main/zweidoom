;;; 76_process-inbox --- process-org-agenda-inbox config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to process-org-agenda-inbox
;;;
;;; Code:


(map! :map org-agenda-mode-map
      :localleader
      :desc "Process inbox items" "i" #'process-org-agenda-inbox-all-items
      :desc "Process all links" "L" #'process-org-agenda-inbox-open-and-archive-all-links
      :desc "Process current item" "P" #'process-org-agenda-inbox-single-item)

(after! org-agenda
  (setq org-agenda-bulk-custom-functions
        `((?c process-org-agenda-inbox-single-item))))

(use-package! process-org-agenda-inbox
  :commands (process-org-agenda-inbox-all-items
             process-org-agenda-inbox-open-and-archive-all-links
             process-org-agenda-inbox-single-item)
  :config
  (setq process-org-agenda-inbox-category ""
        process-org-agenda-inbox-next-file zwei/org-agenda-next-file
        process-org-agenda-inbox-refile-target-info
        '((zwei/org-agenda-projects-file :maxlevel . 2)
          (zwei/org-agenda-tickler-file :maxlevel . 2)
          (zwei/org-agenda-next-file :level . 1 ))))

;;; 76_process-inbox ends here
