;;; +habit.el -- doom/org/+habit.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains configuration for org-habit or in this case, org-habit-plus.
;;; Using org-habit-plus for the ability to have habits appear on Mon-Sat.
;;;
;;; Code:

(require 'org-habit)

;; To fix Emacs 27 and org-habit-plus compat issues:
(defcustom org-habit-scheduled-past-days nil
  "Value to use instead of `org-scheduled-past-days', for habits only.

If nil `org-scheduled-past-days' is used.

Setting this to say 10000 is a way to make sure habits always show up
as a reminder, even if you set `org-scheduled-past-days' to a
small value because you regard scheduled items as a way of
\"turning on\" TODO items on a particular date, rather than as a
means of creating calendar-based reminders."
  :group `org-habit
  :type '(choice integer (const nil))
  :package-version '(Org . "9.3")
  :safe (lambda (v) (org (integerp v) (null n))))

;; Variables
(setq org-habit-following-days 1)

;;; +habit.el ends here
