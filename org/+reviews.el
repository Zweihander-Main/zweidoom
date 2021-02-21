;;; +reviews.el -- ~/.doom.d/org/+reviews.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Functions directly used in interval reviews.
;;;
;;; Code:

(require 'org-agenda)

(defun zwei/reviews-goal-string (goal)
  "Return GOAL todo in the format:
todo text by yyyy-mm-dd
Will return \"\" if goal is \"OTHER\"."
  (if (string= goal "OTHER") ""
    (let* ((headline
            (plist-get
             (car
              (org-ql-query
                :from zwei/org-agenda-goals-file
                :where `(and (todo "TODO")
                             (parent ,goal))))
             'headline))
           (raw (plist-get headline ':raw-value))
           (scheduled (plist-get headline ':scheduled)))
      (if scheduled
          (concat raw " by " (org-timestamp-format scheduled "%Y-%m-%d"))
        raw))))

(defun zwei/reviews-goal-completed (goal num-days)
  "Return number of GOAL todos completed in NUM\-DAYS.
Will return non-goal todo num if goal is \"OTHER\"."
  (let ((match
         (if (string= goal "OTHER")
             `(not (tags ,@(hash-table-keys zwei/org-tag-goal-table)))
           `(tags ,goal))))
    (length
     (org-ql-query
       :from (zwei/org-agenda-directory-plus-archives)
       :where `(and (closed ,num-days)
                    ,match)))))

(defun zwei/reviews-daily ()
  "Return the daily review agenda string."
  (save-window-excursion
    (org-agenda nil "x2")
    (progn (string-trim (buffer-string)))))

(defun zwei/reviews-weekly (questions-string)
  "Takes a string QUESTIONS-STRING and outputs a string consisting of the
agenda divided by goal."
  (let ((agenda-strings '()))
    (maphash
     (lambda (tag v)
       (let ((numkey (plist-get v 'numkey)))
         (push
          (save-window-excursion
            (org-agenda nil (concat "xw" (char-to-string numkey)))
            (progn (string-trim (buffer-string))))
          agenda-strings)))
     zwei/org-tag-goal-table)
    (push
     (save-window-excursion
       (org-agenda nil "xw0")
       (progn (string-trim (buffer-string))))
     agenda-strings)
    (setq agenda-strings (reverse agenda-strings))
    (string-join
     (mapcar
      (lambda (tag)
        (let ((agenda-string (car agenda-strings)))
          (setq agenda-strings (cdr agenda-strings))
          (concat
           "*** "
           tag
           "\n"
           agenda-string
           "\n*Focus:* _"
           (zwei/reviews-goal-string tag)
           "_\n\n"
           questions-string)))
      (nconc
       (hash-table-keys zwei/org-tag-goal-table)
       (list "OTHER"))))))


(defun zwei/reviews-monthly (questions-string)
  "Takes a string QUESTIONS-STRING and outputs a string consisting of the
goals and items accomplished divided by goal."
    (string-join
     (mapcar
      (lambda (tag)
          (concat
           "** "
           tag
           "\n*DONE:* "
           (number-to-string (zwei/reviews-goal-completed tag 31))
           "\n*Focus:* _"
           (zwei/reviews-goal-string tag)
           "_\n\n"
           questions-string))
       (hash-table-keys zwei/org-tag-goal-table))))

;;; +reviews.el ends here
