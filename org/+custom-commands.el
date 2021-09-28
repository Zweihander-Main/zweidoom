;;; +custom-commands.el -- doom/org/+custom-commands.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains custom commands. Functions related (ie skip functions) are
;;; included too.
;;;
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-ql)
(require 'org-super-agenda)

(defun zwei/org-agenda-skip-all-siblings-but-first (&optional check-func)
  "Skip all but the first non-done entry.
If CHECK-FUNC is provided, will check using that too."
  (let ((should-skip-entry)
        (all-checks (lambda ()
                      (let ((pass t))
                        (when check-func
                          (save-excursion
                            (when (funcall check-func)
                              (setq pass nil))))
                        (and pass (zwei/org-current-is-todo-esque))))))
    (unless (funcall all-checks)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (funcall all-checks)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (if (funcall all-checks)
          (condition-case nil
              (progn (evil-org-top) (outline-forward-same-level 1) (point))
            (error (goto-char (point-max))))
        (or (outline-next-heading)
            (goto-char (point-max)))))))

;; Custom commands and their mappings
(setq org-agenda-custom-commands nil)

(add-to-list 'org-agenda-custom-commands
             `("1" "Agenda"
               ((agenda ""
                        ((org-agenda-span 1)
                         (org-agenda-start-day "+0d")
                         (org-deadline-warning-days 365)))
                (todo "NEXT"
                      ((org-agenda-overriding-header "=== In Progress")
                       (org-agenda-files '(,zwei/org-agenda-projects-file
                                           ,zwei/org-agenda-goals-file
                                           ,zwei/org-agenda-tickler-file
                                           ,zwei/org-agenda-next-file))))
                (tags-todo "+TODO=\"TODO\"+PRIORITY=\"A\""
                           ((org-agenda-overriding-header "=== Important & Urgent")
                            (org-agenda-files '(,zwei/org-agenda-projects-file))
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if
                               'deadline
                               'scheduled
                               'timestamp))))
                (tags-todo "+TODO=\"TODO\"+PRIORITY=\"A\""
                           ((org-agenda-overriding-header "=== Pointless & Urgent")
                            (org-agenda-files '(,zwei/org-agenda-next-file))
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if
                               'deadline
                               'scheduled
                               'timestamp)))))))

(add-to-list 'org-agenda-custom-commands
             `("2" "Inbox"
               ((alltodo ""
                         ((org-agenda-overriding-header "=== To Refile")
                          (org-agenda-prefix-format " |%e|")
                          (org-agenda-files '(,zwei/org-agenda-todo-file)))))))

(add-to-list 'org-agenda-custom-commands
             `("3" "Work"
               ((tags "+@work+TODO=\"TODO\"|+@work+TODO=\"NEXT\""
                      ((org-agenda-overriding-header "=== Work")
                       (org-agenda-skip-function
                        '(zwei/org-agenda-skip-all-siblings-but-first
                          #'(lambda()
                              (org-agenda-skip-entry-if
                               'deadline
                               'scheduled
                               'timestamp))))
                       (org-agenda-files
                        '(,zwei/org-agenda-projects-file
                          ,zwei/org-agenda-next-file)))) ; no tickler
                (tags "+@work+TODO=\"WAIT\""
                      ((org-agenda-overriding-header "\n=== Waiting")
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'deadline
                          'scheduled 'timestamp))
                       (org-agenda-files '(,zwei/org-agenda-projects-file
                                           ,zwei/org-agenda-next-file)))))))

(add-to-list 'org-agenda-custom-commands
             `("4" "Not sure yet"
               ((todo "NEXT"
                      ((org-agenda-overriding-header "=== In Progress")
                       (org-agenda-files '(,zwei/org-agenda-projects-file
                                           ,zwei/org-agenda-goals-file
                                           ,zwei/org-agenda-tickler-file
                                           ,zwei/org-agenda-next-file))))
                (tags-todo "+TODO=\"TODO\"+PRIORITY=\"A\""
                           ((org-agenda-overriding-header "=== Important & Urgent")
                            (org-agenda-files '(,zwei/org-agenda-projects-file))
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if
                               'deadline
                               'scheduled
                               'timestamp))))
                (tags-todo "+TODO=\"TODO\"+PRIORITY=\"A\""
                           ((org-agenda-overriding-header "=== Pointless & Urgent")
                            (org-agenda-files '(,zwei/org-agenda-next-file))
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if
                               'deadline
                               'scheduled
                               'timestamp))))
                (tags-todo "+TODO=\"TODO\"+PRIORITY=\"B\"|+TODO=\"TODO\"+PRIORITY=\"C\""
                           ((org-agenda-overriding-header "=== Important & Can Wait")
                            (org-agenda-files '(,zwei/org-agenda-projects-file))
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if
                               'deadline
                               'scheduled
                               'timestamp))))
                (tags-todo "+TODO=\"TODO\"+PRIORITY=\"B\"|+TODO=\"TODO\"+PRIORITY=\"C\""
                           ((org-agenda-overriding-header "=== Frivolous")
                            (org-agenda-files '(,zwei/org-agenda-next-file))
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if
                               'deadline
                               'scheduled
                               'timestamp)))))))

(add-to-list 'org-agenda-custom-commands
             `("5" "Super-Agenda"
               ((agenda ""
                        ((org-agenda-span 1)
                         (org-agenda-start-day "+0d")
                         (org-deadline-warning-days 365)
                         (org-super-agenda-groups
                          '((:name ""
                             :time-grid t)
                            (:name ""
                             :anything t)))))
                (todo "NEXT"
                      ((org-agenda-overriding-header "\n === In Progress")
                       (org-agenda-files '(,zwei/org-agenda-projects-file
                                           ,zwei/org-agenda-goals-file
                                           ,zwei/org-agenda-tickler-file
                                           ,zwei/org-agenda-next-file))))
                (todo ""
                      ((org-agenda-overriding-header "")
                       (org-agenda-files '(,zwei/org-agenda-projects-file
                                           ,zwei/org-agenda-next-file))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'deadline
                          'scheduled
                          'timestamp))
                       (org-super-agenda-groups
                        '((:name "=== Important & Urgent"
                           :and (:todo "TODO"
                                 :priority "A"
                                 :file-path ("projects")))
                          (:name "=== Important & Urgent"
                           :and (:todo "TODO"
                                 :priority "A"
                                 :file-path ("next")
                                 ))
                          (:discard (:anything t)))))))))

(add-to-list 'org-agenda-custom-commands
             `("6" "QL-Agenda"
               ((agenda ""
                        ((org-agenda-span 1)
                         (org-agenda-start-day "+0d")
                         (org-deadline-warning-days 365)))
                (org-ql-block '(todo "NEXT")
                              ((org-ql-block-header "=== In Progress")
                               (org-agenda-files '(,zwei/org-agenda-projects-file
                                                   ,zwei/org-agenda-goals-file
                                                   ,zwei/org-agenda-tickler-file
                                                   ,zwei/org-agenda-next-file))))
                (org-ql-block '(and (todo "TODO")
                                    (priority "A")
                                    (not (ts-active)))
                              ((org-ql-block-header "=== Important & Urgent")
                               (org-agenda-files '(,zwei/org-agenda-projects-file))))
                (org-ql-block '(and (todo "TODO")
                                    (priority "A")
                                    (not (ts-active)))
                              ((org-ql-block-header "=== Pointless & Urgent")
                               (org-agenda-files '(,zwei/org-agenda-next-file)))))))

;; +custom-commands.el ends here
