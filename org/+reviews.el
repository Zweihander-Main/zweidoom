;;; +reviews.el -- ~/.doom.d/org/+reviews.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Functions directly used in interval reviews.
;;;
;;; Code:

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
            "Focus:* _"
            (zwei/org-capture-goal-extract tag)
            "_\n\n"
            questions-string)))
       (nconc
        (hash-table-keys zwei/org-tag-goal-table)
        (list "OTHER"))))))

;;; +reviews.el ends here
