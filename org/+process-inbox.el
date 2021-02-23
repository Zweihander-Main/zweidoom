;;; +process-inbox.el -- ~/.doom.d/org/+process-inbox.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Contains org-agenda code related to inbox processing.
;;;
;;; Code:

(defun zwei/org-agenda-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (zwei/org-agenda-bulk-mark-regexp-category "")
  (zwei/org-agenda-bulk-process-entries))

(defun zwei/org-agenda-process-inbox-item ()
  "Process a single item in the agenda."
  (interactive)
  (org-with-wide-buffer
   (let ((answer nil)
         (continue nil)
         (type "todo"))
     (while (not continue)
       (setq answer
             (read-answer "Item options: [v]iew/[e]dit/[t]odo/[d]one/[a]:note/[l]ink/[k]ill/[n]ext/[i]nfo/[r]ear/[RET]:Continue "
                          '(("view" ?v "View in minibuffer")
                            ("edit" ?e "Edit the headline of the item")
                            ("todo" ?t "Change TODO state of item")
                            ("done" ?d "Mark done and archive")
                            ("note" ?a "Add a note to the item")
                            ("link" ?l "Open link and mark done")
                            ("kill" ?k "Kill current line")
                            ("next" ?n "Put in next file")
                            ("info" ?i "Conver to list item and refile under item")
                            ("rear" ?r "Move to end (rear) of list")
                            ("continue" ?\r "Continue processing"))))
       (cond ((string= answer "continue") (setq continue t))
             ((string= answer "view") (org-agenda-tree-to-indirect-buffer 1)  )
             ((string= answer "link")
              (let ((ret-msg ""))
                (setq ret-msg (org-agenda-open-link))
                (unless (and (stringp ret-msg )(string= ret-msg "No link to open here"))
                  (setq type "link"
                        continue t))))
             ((string= answer "rear") (setq type "rear"
                                            continue t))
             ((string= answer "next") (setq type "next"
                                            continue t))
             ((string= answer "done") (setq type "done"
                                            continue t))
             ((string= answer "info") (setq type "info"
                                            continue t))
             ((string= answer "kill") (setq type "kill"
                                            continue t))
             ((string= answer "edit") (call-interactively #'zwei/org-agenda-edit-headline))
             ((string= answer "todo") (org-agenda-todo))
             ((string= answer "note") (call-interactively #'org-agenda-add-note))))
     (cond ((string= type "todo")
            (progn
              (org-agenda-set-tags)
              (org-agenda-priority)
              (call-interactively 'zwei/org-agenda-set-effort)
              (org-agenda-refile nil nil t)))
           ((string= type "kill")
            (progn
              (org-agenda-todo "KILL")
              (org-agenda-archive)))
           ((string= type "done")
            (progn
              (org-agenda-todo "DONE")
              (org-agenda-archive)))
           ((string= type "rear")
            (org-agenda-drag-line-forward (- (length org-agenda-markers) 1)))
           ((string= type "next")
            (progn
              (org-agenda-todo "NEXT")
              (org-agenda-set-tags)
              (org-agenda-priority)
              (call-interactively 'zwei/org-agenda-set-effort)
              (org-agenda-refile nil
                                 (list (concat (car (last (split-string zwei/org-agenda-next-file "/"))) "/") ;; should be "next.org/"
                                       zwei/org-agenda-next-file nil nil) t)))
           ((string= type "link")
            (progn
              (org-agenda-todo "DONE")
              (org-agenda-archive)))
           ((string= type "info")
            (let ((org-refile-target-verify-function)
                  (org-refile-targets '((zwei/org-agenda-projects-file :maxlevel . 2)
                                        (zwei/org-agenda-tickler-file :maxlevel . 2)
                                        (zwei/org-agenda-next-file :level . 1 ))))
              ;; TODO: add in way to add to ideas, herf, english to add, ect. -- need roam refile
              ;; TODO: add in way to defer to bottom
              ;; TODO: Allow for schedule
              (org-agenda-refile nil nil t)
              (let* ((bookmark (plist-get org-bookmark-names-plist :last-refile))
                     (pos (bookmark-get-position bookmark))
                     (filename (bookmark-get-filename bookmark))
                     (buffer (get-file-buffer filename))
                     (inhibit-read-only t))
                (org-with-remote-undo buffer
                  (with-current-buffer buffer
                    (widen)
                    (goto-char pos)
                    (debug)
                    (org-todo "")
                    (org-toggle-item t))))))))))

(defun zwei/org-agenda-bulk-process-entries ()
  "Bulk process entries in agenda."
  (interactive)
  ;; Set temporary variable lookup -- set hl-line-face from hl-line to hl-line-active
  (when (not (null org-agenda-bulk-marked-entries))
    (let ((entries (reverse org-agenda-bulk-marked-entries))
          (processed 0)
          (skipped 0))
      (dolist (e entries)
        (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
          (if (not pos)
              (progn (message "Skipping removed entry at %s" e)
                     (cl-incf skipped))
            (goto-char pos)
            (hl-line-highlight)
            (highlight-lines-matching-regexp (string-trim (thing-at-point 'line t)) 'highlight)
            (let (org-loop-over-headlines-in-active-region) (funcall 'zwei/org-agenda-process-inbox-item))
            ;; `post-command-hook' is not run yet.  We make sure any
            ;; pending log note is processed.
            (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                      (memq 'org-add-log-note post-command-hook))
              (org-add-log-note))
            (cl-incf processed))))
      (org-agenda-redo t)
      (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
      (message "Acted on %d entries%s%s"
               processed
               (if (= skipped 0)
                   ""
                 (format ", skipped %d (disappeared before their turn)"
                         skipped))
               (if (not org-agenda-persistent-marks) "" " (kept marked)")))))


;; Mappings
(map! :map org-agenda-mode-map
      :localleader
      :desc "Process inbox items" "p" #'zwei/org-agenda-process-inbox
      :desc "Process marked items" "P" #'zwei/org-agenda-bulk-process-entries
      :desc "Process current item" "i" #'zwei/org-agenda-process-inbox-item)

;;; +process-inbox.el ends here
