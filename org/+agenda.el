;;; +agenda.el -- doom/org/+agenda.el-*-lexical-binding:t-*-

;;;
;;; Commentary:
;;;
;;; Contains all org-agenda config EXCEPT for custom commands and inbox
;;; processing.
;;;
;;; Code:


(use-package! org-agenda
  :defer-incrementally mu4e org-roam) ;; get mu4e and roam loading when agenda opened

(require 'org)
(require 'org-agenda)
(require 'org-clock)

;; Variables

(defvar zwei/org-current-effort "1:00"
  "Current effort for agenda items.")

;; Functions

(defun zwei/org-agenda-set-effort (effort)
  "Set the EFFORT property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " zwei/org-current-effort) nil nil zwei/org-current-effort)))
  (setq zwei/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil zwei/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun zwei/org-agenda-redo-all-buffers ()
  "Refresh/redo all org-agenda buffers."
  (interactive)
  (dolist (buffer (doom-visible-buffers))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-redo)))))

(defun zwei/org-agenda-edit-headline ()
  "Perform org-edit-headline on current agenda item."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (call-interactively #'org-edit-headline)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
      (beginning-of-line 1))))

(defun zwei/org-agenda-break-into-child (child)
  "Create CHILD heading under current heading with the same properties and custom effort."
  (interactive
   (list (read-string "Child task: " nil nil nil)))
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         cur-tags cur-line cur-priority cur-stats-cookies)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (setq cur-line (thing-at-point 'line t))
        (if (string-match org-priority-regexp cur-line)
            (setq cur-priority (match-string 2 cur-line)))
        (setq cur-tags (org-get-tags-string))
        (setq cur-stats-cookies (zwei/org-find-statistics-cookies))
        (if (eq cur-stats-cookies 'nil)
            (zwei/org-insert-statistics-cookies))
        (call-interactively #'+org/insert-item-below)
        (call-interactively #'org-demote-subtree)
        (funcall-interactively 'org-edit-headline child)
        (funcall-interactively 'org-set-tags-to cur-tags)
        (if cur-priority
            (funcall-interactively 'org-priority (string-to-char cur-priority)))
        (org-update-parent-todo-statistics)
        (end-of-line 1))
      (beginning-of-line 1)))
  (zwei/org-agenda-redo-all-buffers)
  (let (txt-at-point)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (and (search-forward child nil t)
           (setq txt-at-point
                 (get-text-property (match-beginning 0) 'txt)))
      (if (get-char-property (point) 'invisible)
          (beginning-of-line 2)
        (when (string-match-p child txt-at-point)
          (call-interactively 'zwei/org-agenda-set-effort))))))

;; Hooks

(add-hook! 'org-clock-in-hook :append #'zwei/set-todo-state-next)

;; Mappings -- see custom commands for more

(map! :map org-agenda-mode-map
      :localleader
      :desc "Edit headline" "e" #'zwei/org-agenda-edit-headline
      :desc "Toggle entry text mode" "E" #'org-agenda-entry-text-mode
      :desc "Break into child tasks" "b" #'zwei/org-agenda-break-into-child)

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

  ;; Mappings
  (map! :map org-agenda-mode-map
        :localleader
        :desc "Process inbox items" "i" #'process-org-agenda-inbox-all-items
        :desc "Process all links" "L" #'process-org-agenda-inbox-open-and-archive-all-links
        :desc "Process current item" "P" #'process-org-agenda-inbox-single-item))

;;; +agenda.el ends here
