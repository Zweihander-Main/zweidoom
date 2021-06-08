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
      org-roam-directory (expand-file-name "zettel" org-directory)
      deft-directory org-roam-directory
      org-journal-dir (expand-file-name "dailies" org-directory))

(defvar zwei/org-agenda-directory (expand-file-name "gtd" org-directory)
  "Directory for GTD/work/agenda sytem.")

(defvar zwei/org-agenda-todo-file (expand-file-name "inbox.org" zwei/org-agenda-directory)
  "Inbox file for quickly capturing ideas/tasks.")

(defvar zwei/org-agenda-reviews-file (expand-file-name "reviews.org" zwei/org-agenda-directory)
  "Reviews files for interval reviews.")

(defvar zwei/org-agenda-templates-directory (expand-file-name "templates" org-directory)
  "Directory to store org capture templates.")

(defvar zwei/org-agenda-projects-file (expand-file-name "projects.org" zwei/org-agenda-directory)
  "File for all tasks that can be put into a given active project.")

(defvar zwei/org-agenda-tickler-file (expand-file-name "tickler.org" zwei/org-agenda-directory)
  "File for all tickler tasks. Can include projects but only non-active ones.")

(defvar zwei/org-agenda-next-file (expand-file-name "next.org" zwei/org-agenda-directory)
  "File for one-off tasks that should be done immediately or are currently being worked on.")

(defvar zwei/org-agenda-goals-file (expand-file-name "goals.org" zwei/org-agenda-directory )
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
