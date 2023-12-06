;;; 76_custom-commands --- agenda custom commands -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains custom commands. Functions related (ie skip functions) are
;;; included too.
;;;
;;; Code:

(map! :g "<f1>" (cmd! (zwei/org-agenda-force-load "1"))
      :g "<f2>" (cmd! (zwei/org-agenda-force-load "2"))
      :g "<f3>" (cmd! (zwei/org-agenda-force-load "3"))
      :g "<f4>" (cmd! (zwei/org-agenda-force-load "4"))
      :g "<f5>" (cmd! (zwei/org-agenda-force-load "5"))
      :g "<f6>" (cmd! (zwei/org-agenda-force-load "6")))

(eval-when-compile
  (declare-function org-roam-buffer--visibility "org-roam")
  (declare-function org-roam-buffer-toggle "org-roam")
  (declare-function evil-org-top "evil-org")
  (declare-function org-goto-sibling "org")
  (declare-function outline-forward-same-level "outline")
  (declare-function outline-next-heading "outline"))


;;;###autoload
(defun zwei/org-agenda-force-load (key)
  "Go to agenda KEY and stick to the first line.
Used for global agenda-access keys."
  (when (and (modulep! :lang org +roam2)
             (functionp #'org-roam-buffer--visibility)
             (eq 'visible (org-roam-buffer--visibility)))
    (org-roam-buffer-toggle))
  (org-agenda nil key)
  (evil-goto-first-line))

;;;###autoload
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

(after! org-agenda
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
                                   :not (:tag "@play")
                                   :priority ("B" "C")
                                   :file-path ("next")))
                            (:name "=== To prioritize"
                             :and (:todo "TODO"
                                   :not (:tag ("@play"))))
                            (:discard (:anything t))))))
                  (todo "WAIT"
                        ((org-agenda-overriding-header "\n === Waiting")
                         (org-agenda-files '(,zwei/org-agenda-projects-file
                                             ,zwei/org-agenda-next-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :and (:not (:tag ("@play" "@down" "@end"))))
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
                 ((todo "NEXT"
                        ((org-agenda-overriding-header " === In Progress")
                         (org-agenda-files '(,zwei/org-agenda-projects-file
                                             ,zwei/org-agenda-goals-file
                                             ,zwei/org-agenda-tickler-file
                                             ,zwei/org-agenda-next-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :and (:not (:tag ("@play" "@down" "@end"))))
                            (:discard (:anything t))))))
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
                                   :not (:tag ("@play" "@down" "@end"))
                                   :priority "A"
                                   :file-path ("projects")))
                            (:discard (:anything t)))))))))

  (add-to-list 'org-agenda-custom-commands
               `("4" "Play"
                 ((tags "@play"
                        ((org-agenda-overriding-header " === Current on Play Comp")
                         (org-agenda-files
                          '(,zwei/org-agenda-projects-file
                            ,zwei/org-agenda-tickler-file
                            ,zwei/org-agenda-next-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "NEXT")
                            (:discard (:anything t))))))
                  (tags "@play"
                        ((org-agenda-overriding-header "\n === Todo on Play Comp")
                         (org-agenda-files
                          '(,zwei/org-agenda-projects-file
                            ,zwei/org-agenda-tickler-file
                            ,zwei/org-agenda-next-file))
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if
                            'deadline
                            'scheduled
                            'timestamp))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "TODO")
                            (:discard (:anything t)))))))))

  (add-to-list 'org-agenda-custom-commands
               `("5" "Commit"
                 ((tags "+commit"
                        ((org-agenda-overriding-header " === Commits Working On")
                         (org-agenda-files
                          '(,zwei/org-agenda-projects-file
                            ,zwei/org-agenda-tickler-file
                            ,zwei/org-agenda-next-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "NEXT")
                            (:discard (:anything t))))))
                  (tags "+commit"
                        ((org-agenda-overriding-header " === Commits Needed")
                         (org-agenda-files
                          '(,zwei/org-agenda-projects-file
                            ,zwei/org-agenda-tickler-file
                            ,zwei/org-agenda-next-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "TODO")
                            (:discard (:anything t)))))))))

  (add-to-list 'org-agenda-custom-commands
               `("6" "Review"
                 ((todo ""
                        ((org-agenda-overriding-header "=== Important & Urgent")
                         (org-agenda-files '(,zwei/org-agenda-projects-file
                                             ,zwei/org-agenda-next-file
                                             ,zwei/org-agenda-tickler-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :and (:todo ("WAIT" "HOLD")
                                   :priority "A"
                                   :file-path ("projects")))
                            (:name "=== Pointless & Urgent"
                             :and (:todo ("WAIT" "HOLD")
                                   :priority "A"
                                   :file-path ("next")))
                            (:name "=== Important & Can Wait"
                             :and (:todo ("WAIT" "HOLD")
                                   :priority ("B" "C")
                                   :file-path ("projects")))
                            (:name "=== Pointless & Can Wait"
                             :and (:todo ("WAIT" "HOLD")
                                   :priority ("B" "C")
                                   :file-path ("next")))
                            (:name "=== Tickler"
                             :and (:todo ("WAIT" "HOLD")
                                   :file-path ("tickler")
                                   :priority ("A" "B" "C")))
                            (:name "=== To prioritize"
                             :and (:todo ("WAIT" "HOLD")
                                   :not (:priority ("A" "B" "C"))))
                            (:discard (:anything t))))))
                  )))

  (add-to-list 'org-agenda-custom-commands
               `("9" "OLD Work"
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
                            'scheduled
                            'timestamp))
                         (org-agenda-files '(,zwei/org-agenda-projects-file))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "WAIT")
                            (:name "=== Held"
                             :todo "HOLD")
                            (:discard (:anything t))))))))))


;;; 76_custom-commands ends here
