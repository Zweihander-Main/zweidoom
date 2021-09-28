;;; +agenda.el -- doom/org/+agenda.el-*-lexical-binding:t-*-

;;;
;;; Commentary:
;;;
;;; Contains all org-agenda config EXCEPT for custom commands and inbox
;;; processing.
;;;
;;; Code:


(require 'org)
(require 'org-agenda)
(require 'org-clock)

(use-package! org-agenda-heading-functions)

;; Hooks

(add-hook! 'org-clock-in-hook :append #'zwei/set-todo-state-next)

;; Mappings -- see custom commands for more

(map! :map org-agenda-mode-map
      :localleader
      :desc "Edit headline" "e" #'org-agenda-heading-functions-edit-headline
      :desc "Toggle entry text mode" "E" #'org-agenda-entry-text-mode
      :desc "Break into child tasks" "b" #'org-agenda-heading-functions-break-into-child)

  ;;; Enable easymotion in agenda
(after! evil-easymotion
  (map! :map evil-org-agenda-mode-map
        :m "gs" evilem-map))

;; Config

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
      "%40ITEM(Task) %Effort(E Est){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")


;; Super agenda
(use-package! org-super-agenda
  :after evil-org-agenda
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-header-map evil-org-agenda-mode-map)) ; fix keymaps

;; Org-clock-convenience for agenda
(use-package! org-clock-convenience
  :defer t
  :after org-clock)

(map! :after org-clock-convenience
      :map org-agenda-mode-map
      "<S-up>" #'org-clock-convenience-timestamp-up
      "<S-down>" #'org-clock-convenience-timestamp-down
      "o" #'org-clock-convenience-fill-gap
      "e" #'org-clock-convenience-fill-gap-both)

(use-package! process-org-agenda-inbox
  :config
  (setq process-org-agenda-inbox-category ""
        process-org-agenda-inbox-next-file zwei/org-agenda-next-file
        process-org-agenda-inbox-refile-target-info
        '((zwei/org-agenda-projects-file :maxlevel . 2)
          (zwei/org-agenda-tickler-file :maxlevel . 2)
          (zwei/org-agenda-next-file :level . 1 )))

  (setq org-agenda-bulk-custom-functions `((?c process-org-agenda-inbox-single-item)))

  ;; Mappings for process-org-agenda-inbox
  (map! :map org-agenda-mode-map
        :localleader
        :desc "Process inbox items" "i" #'process-org-agenda-inbox-all-items
        :desc "Process all links" "L" #'process-org-agenda-inbox-open-and-archive-all-links
        :desc "Process current item" "P" #'process-org-agenda-inbox-single-item))

;; Org-habit (note -- loaded in +org as org-modules)
(after! org-habit
  :config
  (setq org-habit-following-days 1))

;;; +agenda.el ends here
