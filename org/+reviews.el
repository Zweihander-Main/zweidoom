;;; +reviews.el -- ~/.doom.d/org/+reviews.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Functions directly used in interval reviews.
;;;
;;; Code:

(defun zwei/reviews-weekly (questions-string)
  "Takes a string QUESTIONS-STRING and outputs a string consisting of the
agenda divided by goal."
  (let ((results '()))
    (maphash
     (lambda (tag v)
       (let ((numkey (plist-get v 'numkey)))
         (push
          (concat
           "*** "
           tag
           "\n"
           (save-window-excursion
             (org-agenda nil (concat "xw" (char-to-string numkey)))
             (progn (string-trim (buffer-string))))
           "\n\n*Focus:* _"
           (zwei/org-capture-goal-extract tag)
           "_\n\n"
           questions-string)
          results)))
     zwei/org-tag-goal-table)
    (string-join results)))

;;; +reviews.el ends here
