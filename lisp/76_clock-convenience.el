;;; 76_clock-convenience --- org-clock-convenience config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-clock-convenience' configuration.
;;;
;;; Code:

(map! :map org-agenda-mode-map
      "<S-up>" #'org-clock-convenience-timestamp-up
      "<S-down>" #'org-clock-convenience-timestamp-down
      "o" #'org-clock-convenience-fill-gap
      "e" #'org-clock-convenience-fill-gap-both)

(use-package! org-clock-convenience
  :commands (org-clock-convenience-timestamp-up
             org-clock-convenience-timestamp-down
             org-clock-convenience-fill-gap
             org-clock-convenience-fill-gap-both))

;;; 76_clock-convenience ends here
