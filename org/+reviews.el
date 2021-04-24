;;; +reviews.el -- doom/org/+reviews.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Functions directly used in interval reviews.
;;;
;;; Code:

(require 'org-capture)
(require 'org-agenda)

;; Variables

(defvar zwei/reviews-current-working-date '(1 1 1900)
  "Current date being used by org-capture for generating reviews. Usually
generated by calendar-read-date. List in (month day year) format.")

;; Functions

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

(defun zwei/reviews-goal-completed (goal orgql-date-predicate)
  "Return number of GOAL todos completed in ORGQL-DATE-PREDICATE.
Will return non-goal todo num if goal is \"OTHER\"."
  (let ((match
         (if (string= goal "OTHER")
             `(not (tags ,@(hash-table-keys zwei/org-tag-goal-table)))
           `(tags ,goal))))
    (length
     (org-ql-query
       :from (org-agenda-files t t)
       :where `(and (closed ,@orgql-date-predicate)
                    ,match)))))

(defun zwei/reviews-daily ()
  "Return the daily review agenda string."
  (save-window-excursion
    (org-agenda nil "x2")
    (progn (string-trim (buffer-string)))))

(defun zwei/reviews-prompt-week-number ()
  ""
  (interactive)
  (let* ((today (calendar-current-date))
         (day-of-week (calendar-day-of-week today)) ; 0 is sun
         (abs-days (calendar-absolute-from-gregorian today))
         (abs-start (- abs-days day-of-week -1)) ; iso starts on mon
         (iso-date-today (calendar-iso-from-absolute abs-start))
         (abs-end (+ abs-start 7))
         (collection '()))
    (let ((counter 104) ; 52 weeks before, 52 weeks after
          (counter-abs-days (- abs-start (* 52 7))) ; start 52 weeks back
          (counter-iso-date-start))
      (while (> counter 0)
        (setq counter-iso-date-start
              (calendar-iso-from-absolute counter-abs-days))
        (push
         (cons (concat
          (format "W%02d" (nth 0 counter-iso-date-start))
          ", "
          (number-to-string (nth 2 counter-iso-date-start))
          " | "
          (zwei/date-to-time
           (calendar-gregorian-from-absolute counter-abs-days))
          " - "
          (zwei/date-to-time
           (calendar-gregorian-from-absolute (+ counter-abs-days 6)))
          )
         counter-iso-date-start)
         collection)
        (setq counter-abs-days (+ counter-abs-days 7))
        (setq counter (1- counter))))
    (assoc
     (completing-read
     "Select which week: "
     (reverse collection)
     nil
     t
     nil
     nil
     (nth 52 collection)) ; Week just before this one
     collection)
    ;; NEXT: Get the cdr, throw it into abs-from-iso and grego-from-abs
    ;; You will then have the week num (nth 0), and the start date in whatever format
    ;; The datetree has a function for week, figure it out, figure out what it wants
    ;; Then implement it, clean up reviews so it uses that
    ;; Then port over the monthly code and generally copy that
    ;; This function is just for prompt, copy the monthly one for the rest
  ))

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
           "_\n===\n"
           questions-string)))
      (nconc
       (hash-table-keys zwei/org-tag-goal-table)
       (list "OTHER"))))))


(defun zwei/reviews-month-to-orgql (date)
  "Takes a DATE from calendar-read-date and outputs date-time predicate
covering whole month of that DATE for use in org-ql."
  (let* ((month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (from-time (zwei/date-to-time date))
         (last-day (calendar-last-day-of-month month year))
         (to-time (zwei/date-to-time (list month last-day year))))
    `(:from ,from-time :to ,to-time)))

(defun zwei/reviews-monthly (questions-string)
  "Takes a string QUESTIONS-STRING and outputs a string consisting of the
goals and items accomplished divided by goal."
  (string-join
   (mapcar
    (lambda (tag)
      (concat
       "* "
       tag
       "\n*DONE:* "
       (number-to-string (zwei/reviews-goal-completed
                          tag
                          (zwei/reviews-month-to-orgql
                           zwei/reviews-current-working-date)))
       "\n*Focus:* _"
       (zwei/reviews-goal-string tag)
       "_\n-----\n"
       questions-string))
    (hash-table-keys zwei/org-tag-goal-table))))

(defun zwei/reviews-position-monthly-template ()
  "Will prompt for a year/month and create+goto a datetree for montly tree.
To be used in org-capture-template as the position function."
  (setq zwei/reviews-current-working-date (calendar-read-date t))
  (let ((marker (org-find-olp
                 (cons
                  (org-capture-expand-file zwei/org-agenda-reviews-file)
                  (list "Monthly Reviews")))))

    (set-buffer (marker-buffer marker))
    (widen)
    (goto-char marker)
    (set-marker marker nil)
    (require 'org-datetree)
    (org-datetree-find-month-create zwei/reviews-current-working-date 'subtree-at-point)
    ))

(defun zwei/reviews-generate-monthly-template ()
  "Will parse the monthly template and feed it to org-capture-fill-template.
To be used in org-capture-template as the template function."
  (org-capture-fill-template (org-file-contents
                              zwei/org-agenda-monthly-review-template-file)))

;;; +reviews.el ends here
