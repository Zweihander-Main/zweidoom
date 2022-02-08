;;; +custom-commands.el- doom/org/+custom-commands.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains custom commands. Functions related (ie skip functions) are
;;; included too.
;;;
;;; Code:


(after! org-agenda
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
                           (org-deadline-warning-days 365)
                           (org-super-agenda-groups
                            '((:name "⤷Goals"
                               :file-path("goals")
                               :order 9)
                              (:name ""
                               :time-grid t)
                              (:name ""
                               :deadline past)
                              (:name ""
                               :deadline today)
                              (:name ""
                               :and (:deadline future
                                     :scheduled nil))
                              (:name ""
                               :scheduled past)
                              (:name ""
                               :scheduled today)
                              (:discard (:anything t))))))
                  (todo "NEXT"
                        ((org-agenda-overriding-header "\n === In Progress")
                         (org-agenda-files '(,zwei/org-agenda-projects-file
                                             ,zwei/org-agenda-goals-file
                                             ,zwei/org-agenda-tickler-file
                                             ,zwei/org-agenda-next-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :and (:not (:tag ("@play" "@down" "@end"))))
                            (:name "⤷@play"
                             :tag "@play")
                            (:name "⤷@down"
                             :tag "@down")
                            (:name "⤷@end"
                             :tag "@end")))))
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
                            (:name "=== Pointless & Urgent"
                             :and (:todo "TODO"
                                   :priority "A"
                                   :file-path ("next")))
                            (:name "=== Important & Can Wait"
                             :and (:todo "TODO"
                                   :priority ("B" "C")
                                   :file-path ("projects")))
                            (:name "=== Pointless & Can Wait"
                             :and (:todo "TODO"
                                   :priority ("B" "C")
                                   :file-path ("next")))
                            (:name "=== To prioritize"
                             :todo "TODO")
                            (:discard (:anything t)))))))))

  (add-to-list 'org-agenda-custom-commands
               `("2" "Inbox"
                 ((alltodo ""
                           ((org-agenda-overriding-header " === To Refile")
                            (org-agenda-prefix-format " |%e|")
                            (org-agenda-files '(,zwei/org-agenda-todo-file))
                            (org-super-agenda-groups
                             '((:name ""
                                :todo "TODO")
                               (:name "=== To file/fix"
                                :anything t))))))))


  (add-to-list 'org-agenda-custom-commands
               `("3" "Work"
                 ((tags "+@work"
                        ((org-agenda-overriding-header " === Work")
                         (org-agenda-skip-function
                          '(zwei/org-agenda-skip-all-siblings-but-first
                            #'(lambda()
                                (org-agenda-skip-entry-if
                                 'deadline
                                 'scheduled
                                 'timestamp))))
                         (org-agenda-files
                          '(,zwei/org-agenda-projects-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "TODO"
                             :todo "NEXT")
                            (:discard (:anything t))))))
                  (tags "+@work"
                        ((org-agenda-overriding-header "\n === Waiting")
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if
                            'deadline
                            'scheduled 'timestamp))
                         (org-agenda-files '(,zwei/org-agenda-projects-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "WAIT")
                            (:name "=== Held"
                             :todo "HOLD")
                            (:discard (:anything t))))))))))

;; +custom-commands.el ends here