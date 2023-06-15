;;; 75_agenda --- org-agenda config-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains all org-agenda config EXCEPT for custom commands and inbox
;;; processing.
;;;
;;; Code:

(map! :leader
      (:prefix-map ("n" . "notes")
       :desc "consult-org-agenda" "/" #'consult-org-agenda))

(map! :map org-agenda-mode-map
      :localleader
      :desc "Toggle entry text mode" "E" #'org-agenda-entry-text-mode)

(after! org-agenda
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
