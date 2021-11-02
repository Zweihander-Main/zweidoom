;;; +org.el -- doom/org/+org.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains org related config that's not listed in other files in /org
;;;
;;; Code:

(eval-when-compile
  (defvar deft-directory))

(require 'org)
(require 'dash)

;;; Functions
(defun zwei/find-gtd-file ()
  "Find a file in `zwei/org-agenda-directory'."
  (interactive)
  (let ((+vertico-consult-fd-args
         (format "%s --color=never -i -H -E .git -e org --regex"
                 doom-projectile-fd-binary)))
    (doom-project-find-file zwei/org-agenda-directory)))

(defun zwei/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/+DONE" 'file)
  (org-map-entries 'org-archive-subtree "/+KILL" 'file))

(defun zwei/set-todo-state-next ()
  "Change todo to NEXT."
  (org-todo "NEXT"))

(defun zwei/org-current-is-todo-esque ()
  "Returns if current heading is a form of todo"
  (let ((state (org-get-todo-state)))
    (or
     (string= "TODO" state)
     (string= "NEXT" state))))

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

(defun zwei/org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun zwei/org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (zwei/org-show-properties)
    (zwei/org-hide-properties)))

(defun zwei/checklist-find ()
  "Open a checklist in `zwei/org-checklists-directory'."
  (interactive)
  (let ((+snippets-dir zwei/org-checklists-directory))
    (+snippets/find-private)))

(defun zwei/checklist-new ()
  "Create a new checklist in `zwei/org-checklists-directory'."
  (interactive)
  (let* ((major-mode 'org-mode)
         (default-directory
           (expand-file-name (symbol-name major-mode)
                             zwei/org-checklists-directory)))
    (+snippet--ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (yas-expand-snippet (concat "# -*- mode: snippet -*-\n"
                                  "# name: $1\n"
                                  "# uuid: $1\n"
                                  "# --\n"
                                  "- [ ] $0"))
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))

;; Mappings
(map! :map org-mode-map
      :m "gSp" #'zwei/org-toggle-properties
      :m "gSe" #'org-toggle-pretty-entities
      :m "gSl" #'org-latex-preview
      :localleader
      (:prefix ("C" . "Checklists")
       :desc "New checklist" "n" #'zwei/checklist-new
       :desc "Find checklist" "f" #'zwei/checklist-find
      (:prefix "r"
       :desc "Archive all done tasks" "a" #'zwei/org-archive-done-tasks)))

;; General config
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

;; Checklists
(use-package! yasnippet
  :commands (zwei/checklist-edit
             zwei/checklist-new)
  :config
  (add-to-list 'yas-snippet-dirs 'zwei/org-checklists-directory)
  (yas-reload-all))

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

(use-package! org-statistics-cookie-helpers)
;;; +org.el ends here
