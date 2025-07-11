;;; 70_org --- basic org config -*-lexical-binding:t-*-
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
      (:localleader
       :desc "Edit headline" "E" #'org-edit-headline)
      (:localleader
       (:prefix "r"
        :desc "Archive all done tasks" "a" #'zwei/org-archive-done-tasks))
      (:leader
       (:prefix "n"
        :desc "Find in gtd" :g "g" #'zwei/find-gtd-file)))

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
(defun zwei/org-current-is-todo-esque ()
  "Return if current heading is a form of todo."
  (let ((state (org-get-todo-state)))
    (or
     (string= "TODO" state)
     (string= "NEXT" state))))

;;;###autoload
(defun zwei/org-hide-properties ()
  "Hide all `org-mode' headline property drawers in buffer.
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
  "Show all `org-mode' property drawers hidden by org-hide-properties."
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
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :config
  ;; https://github.com/hlissner/doom-emacs/issues/4832
  ;; Fixes the following flow: agenda -> capture -> broken.
  (defalias '+org--restart-mode-h #'ignore)
  (eval-when-compile
    (declare-function org-map-entries "org")
    (declare-function org-get-todo-state "org"))
  ;; General
  (setq org-hide-emphasis-markers t
        org-extend-today-until 4 ;; add some buffer after midnight
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        org-hierarchical-todo-statistics nil
        org-startup-folded 'content
        org-image-actual-width 600
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
        org-log-into-drawer nil)

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

  ;; Filing
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((zwei/org-agenda-projects-file :maxlevel . 1)
                             (zwei/org-agenda-tickler-file :maxlevel . 1)
                             (zwei/org-agenda-next-file :level . 0 ))))

;;; 70_org ends here
