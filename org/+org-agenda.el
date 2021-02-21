;;; +org-agenda.el -- ~/.doom.d/org/+org-agenda.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Contains all org-agenda config EXCEPT for custom commands and inbox
;;; processing.
;;;
;;; Code:


(use-package! org-agenda
  :defer t
  :after org
  :defer-incrementally mu4e org-roam) ;; get mu4e and roam loading when agenda opened

;; Variables

(defvar zwei/org-current-effort "1:00"
  "Current effort for agenda items.")

;; Functions

(defun zwei/org-agenda-bulk-mark-regexp-category (regexp)
  "Mark entries whose category matches REGEXP for future agenda bulk action."
  (interactive "sMark entries with category matching regexp: ")
  (let ((entries-marked 0) txt-at-point)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (while (and (re-search-forward regexp nil t)
                  (setq category-at-point
                        (get-text-property (match-beginning 0) 'org-category)))
        (if (get-char-property (point) 'invisible)
            (beginning-of-line 2)
          (when (string-match-p regexp category-at-point)
            (setq entries-marked (1+ entries-marked))
            (call-interactively 'org-agenda-bulk-mark)))))
    (unless entries-marked
      (message "No entry matching this regexp."))))

(defun zwei/org-inbox-capture ()
  "Shortcut to org-capture->inbox."
  (interactive)
  "Capture a an inbox task."
  (org-capture nil "i"))

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
         cur-tags cur-line cur-priority cur-stats-cookies txt-at-point)
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
  (save-excursion
    (goto-char (point-min))
    (goto-char (next-single-property-change (point) 'org-hd-marker))
    (and (search-forward child nil t)
         (setq txt-at-point
               (get-text-property (match-beginning 0) 'txt)))
    (if (get-char-property (point) 'invisible)
        (beginning-of-line 2)
      (when (string-match-p child txt-at-point)
        (call-interactively 'zwei/org-agenda-set-effort)))))

(defun zwei/org-agenda-directory-plus-archives ()
  "zwei/org-agenda-directory plus the addition of the archive files."
  (org-agenda-files
   (directory-files
    zwei/org-agenda-directory
    t
    "\\(\.org\\)\\|\\(.org_archive\\)$" t)))

;; Hooks

(add-hook! 'org-clock-in-hook :append #'zwei/set-todo-state-next)

;; Mappings -- see custom commands for more

(map! :leader
      :prefix "n"
      :desc "Inbox entry" "i" #'zwei/org-inbox-capture)

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

(setq org-agenda-files (directory-files zwei/org-agenda-directory t "\.org$" t)
      org-agenda-start-with-log-mode t
      org-agenda-start-day "-1d"
      org-agenda-span 3
      org-agenda-block-separator nil
      org-agenda-bulk-custom-functions `((?c zwei/org-agenda-process-inbox-item))
      org-agenda-prefix-format
      `((agenda . " %i %-12:c%?-12t% s|%e|")
        (todo . " %i %-12:c|%e|")
        (tags . " %i %-12:c|%e|")
        (search . " %i %-12:c|%e|"))
      org-columns-default-format
      "%40ITEM(Task) %Effort(E Est){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

;; Org-clock-convenience for agenda
(use-package! org-clock-convenience
  :after org-clock)

(map! :after org-clock-convenience
      :map org-agenda-mode-map
      "<S-up>" #'org-clock-convenience-timestamp-up
      "<S-down>" #'org-clock-convenience-timestamp-down
      "o" #'org-clock-convenience-fill-gap
      "e" #'org-clock-convenience-fill-gap-both)

;;; +org-agenda.el ends here
