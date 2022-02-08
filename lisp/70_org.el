;;; org--- basic org config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Basic org configuration.
;;;
;;; Code:

(map! :map org-mode-map
      :m "gMp" #'zwei/org-toggle-properties
      :m "gMe" #'org-toggle-pretty-entities
      :m "gMl" #'org-latex-preview
      :localleader
      (:prefix "r"
       :desc "Archive all done tasks" "a" #'zwei/org-archive-done-tasks))

;;;###autoload
(defun zwei/find-gtd-file ()
  "Find a file in `zwei/org-agenda-directory'."
  (interactive)
  (let ((+vertico-consult-fd-args
         (format "%s --color=never -i -H -E .git -e org --regex"
                 doom-projectile-fd-binary)))
    (doom-project-find-file zwei/org-agenda-directory)))

;;;###autoload
(defun zwei/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/+DONE" 'file)
  (org-map-entries 'org-archive-subtree "/+KILL" 'file))

;;;###autoload
(defun zwei/set-todo-state-next ()
  "Change todo to NEXT."
  (org-todo "NEXT"))

;;;###autoload
(defun zwei/org-current-is-todo-esque ()
  "Returns if current heading is a form of todo"
  (let ((state (org-get-todo-state)))
    (or
     (string= "TODO" state)
     (string= "NEXT" state))))

;;;###autoload
(defun zwei/org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer.
Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

;;;###autoload
(defun zwei/org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

;;;###autoload
(defun zwei/org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (zwei/org-show-properties)
    (zwei/org-hide-properties)))

(use-package! org
  :commands (zwei/find-gtd-file)
  :commands (zwei/deft-gtd-file)
  :config
  ;; General
  (setq org-hide-emphasis-markers t
        org-extend-today-until 4 ;; add some buffer after midnight
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        org-hierarchical-todo-statistics nil
        org-startup-folded 'overview
        org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(n)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A checklist that needs doing
           "[-](N)"   ; Checklist is in progress
           "[?](W)"   ; Checklist is being held up or paused
           "|"
           "[X](D)")) ; Checklist was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("NEXT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)))

  ;; Logging
  (setq org-log-done 'time
        org-log-into-drawer t)

  ;; Tagging -- used for place and goal
  (setq org-tag-persistent-alist `((:startgroup . "place")
                                   ("@work" . ?w)
                                   ("@play" . ?p)
                                   ("@down" . ?d)
                                   ("@end" . ?e)
                                   (:endgroup . "place"))
        org-fast-tag-selection-single-key nil
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance '("crypt" "@work" "@play" "@down" "@end"))



  (use-package! zweigtd-goals
    :config
    (setq zweigtd-goals-file zwei/org-agenda-goals-file)
    (zweigtd-goals-init '((:name "1#PHYSICAL"   :key ?1 :color "#CC2200")
                          (:name "2#MENTAL"     :key ?2 :color "#008F40")
                          (:name "3#CODING"     :key ?3 :color "#42A5F5")
                          (:name "4#AUTOMATION" :key ?4 :color "#00FF33")
                          (:name "5#BUSINESS"   :key ?5 :color "#F5C400")
                          (:name "6#WANKER"     :key ?6 :color "#6A3B9F"))))


  ;; Filing
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((zwei/org-agenda-projects-file :maxlevel . 1)
                             (zwei/org-agenda-tickler-file :maxlevel . 1)
                             (zwei/org-agenda-next-file :level . 0 )))

  ;; Other modules
  (add-to-list 'org-modules 'org-habit)

  ;; Disable fancy-priorities for now
  (after! org-fancy-priorities
    :config
    (setq org-fancy-priorities-mode -1))

  ;; Org-journal
  (after! org-journal
    (setq org-journal-date-prefix "#+TITLE: "
          org-journal-file-format "%Y-%m-%d.org"
          org-journal-date-format "%A, %d %B %Y"
          org-journal-enable-agenda-integration t))

  (use-package! org-statistics-cookie-helpers))

;;; org ends here
