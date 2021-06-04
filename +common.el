;;; +common.el -- doom/+common.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains common variables and functions used repeatedly throughout the
;;; config.
;;;
;;; Code:


;; ===============
;;   Directories
;; ===============

;; Load machine specific directories which includes org-directory
(load! "+machine_var") ;; At doom due to chezmoi config discrepencies

;; Set all directories around org
(setq default-directory org-directory
      org-roam-directory (concat org-directory "/zettel")
      deft-directory org-roam-directory
      org-journal-dir (concat org-directory "/dailies"))

(defvar zwei/org-agenda-directory (concat org-directory "/gtd")
  "Directory for GTD/work/agenda sytem.")

(defvar zwei/org-agenda-todo-file (concat zwei/org-agenda-directory "/inbox.org")
  "Inbox file for quickly capturing ideas/tasks.")

(defvar zwei/org-agenda-reviews-file (concat zwei/org-agenda-directory "/reviews.org")
  "Reviews files for interval reviews.")

(defvar zwei/org-agenda-templates-directory (concat org-directory "/templates")
  "Directory to store templates for GTD system (weekly review template for example).")

(defvar zwei/org-agenda-monthly-review-template-file (concat zwei/org-agenda-templates-directory "/monthly_review.org")
  "Template file for monthly review.")

(defvar zwei/org-agenda-weekly-review-template-file (concat zwei/org-agenda-templates-directory "/weekly_review.org")
  "Template file for weekly review.")

(defvar zwei/org-agenda-daily-review-template-file (concat zwei/org-agenda-templates-directory "/daily_review.org")
  "Template file for daily review.")

(defvar zwei/org-agenda-projects-file (concat zwei/org-agenda-directory "/projects.org")
  "File for all tasks that can be put into a given active project.")

(defvar zwei/org-agenda-tickler-file (concat zwei/org-agenda-directory "/tickler.org")
  "File for all tickler tasks. Can include projects but only non-active ones.")

(defvar zwei/org-agenda-next-file (concat zwei/org-agenda-directory "/next.org")
  "File for one-off tasks that should be done immediately or are currently being worked on.")

(defvar zwei/org-agenda-goals-file (concat zwei/org-agenda-directory "/goals.org")
  "File for overarching goal tracking.")

(setq +org-capture-todo-file zwei/org-agenda-todo-file
      org-default-notes-file zwei/org-agenda-todo-file)


;; ==================
;;  Common Functions
;; ==================

(defun zwei/which-linux-distro ()
  "Info from lsb_release. Will output strings such as 'Debian' or 'Arch'."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (shell-command-to-string "echo -n $(lsb_release -is)")))

;;; +common.el ends here
