;;; 75_agenda --- org-agenda config-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains all org-agenda config EXCEPT for custom commands and inbox
;;; processing.
;;;
;;; Code:

(map! :map org-agenda-mode-map
       :localleader
       :desc "Edit headline" "e" #'org-agenda-heading-functions-edit-headline
       :desc "Toggle entry text mode" "E" #'org-agenda-entry-text-mode
       :desc "Break into child tasks" "b" #'org-agenda-heading-functions-break-into-child)

(use-package! org-agenda
  :commands (org-agenda org-agenda-entry-text-mode)
  :config
  (setq org-agenda-files (list zwei/org-agenda-directory)
        org-agenda-start-with-log-mode t
        org-agenda-start-day "-1d"
        org-agenda-span 3
        org-agenda-block-separator nil
        org-agenda-prefix-format
        `((agenda . " %i %-12:c%?-12t% s|%e|")
          (todo . " %i %-12:c|%e|")
          (tags . " %i %-12:c|%e|")
          (search . " %i %-12:c|%e|"))
        org-columns-default-format
        "%40ITEM(Task) %Effort(E Est){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)"))

;;; 75_agenda ends here
