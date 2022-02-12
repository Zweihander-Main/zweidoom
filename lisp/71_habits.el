;;; 71_habits --- org-habit config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-habit' configuration
;;;
;;; Code:

(after! org
  (add-to-list 'org-modules 'org-habit))

(use-package! org-habit
  :defer t
  :after org-agenda
  :config
  (eval-when-compile
    (declare-function rxt--re-builder-switch-pcre-mode "pcre2el"))
  (setq org-habit-following-days 1))

;;; 71_habits ends here
