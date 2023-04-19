;;; 01_vars-dirs --- common directories -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Common directories for use in other parts of the config.
;;;
;;; Code:

;; Load machine specific directories which includes org-directory
(load! "../+machine_var") ;; At doom due to chezmoi config discrepencies

;; Set all directories around org
(setq default-directory org-directory
      org-roam-directory (file-name-as-directory (expand-file-name "zettel" org-directory))
      deft-directory org-roam-directory
      org-journal-dir (file-name-as-directory (expand-file-name "dailies" org-directory)))

(defvar zwei/org-agenda-directory (file-name-as-directory (expand-file-name "gtd" org-directory))
  "Directory for GTD/work/agenda sytem.")

(defvar zwei/org-agenda-todo-file (expand-file-name "inbox.org" zwei/org-agenda-directory)
  "Inbox file for quickly capturing ideas/tasks.")

(defvar zwei/org-agenda-reviews-file (expand-file-name "reviews.org" zwei/org-agenda-directory)
  "Reviews files for interval reviews.")

(defvar zwei/org-agenda-projects-file (expand-file-name "projects.org" zwei/org-agenda-directory)
  "File for all tasks that can be put into a given active project.")

(defvar zwei/org-agenda-tickler-file (expand-file-name "tickler.org" zwei/org-agenda-directory)
  "File for all tickler tasks. Can include projects but only non-active ones.")

(defvar zwei/org-agenda-next-file (expand-file-name "next.org" zwei/org-agenda-directory)
  "File for one-off tasks or low urgency (no project).")

(defvar zwei/org-agenda-goals-file (expand-file-name "goals.org" zwei/org-agenda-directory )
  "File for overarching goal tracking.")

(defvar zwei/org-agenda-templates-directory (file-name-as-directory (expand-file-name "templates" org-directory))
  "Directory to store org capture templates.")

(defvar zwei/org-checklists-directory (file-name-as-directory (expand-file-name "checklists" org-directory))
  "Directory to store private `yasnippet' org mode checklists.")

(defvar zwei/org-roam-bib-directory (file-name-as-directory (expand-file-name "bib" org-roam-directory))
  "Directory for storing literature notes and bibliographic references.")

(defvar zwei/org-roam-bib-files-directory (file-name-as-directory (expand-file-name "files" zwei/org-roam-bib-directory))
  "Directory for storing files related to literature notes (ie PDFs).")

(defvar zwei/org-roam-bib-files '("books.bib" "papers.bib" "online.bib")
  "Filenames in the `org-roam-bib-directory' that correspond to different bibs.
Lead with book bibliography.")

(defvar zwei/assets-dir (file-name-as-directory (expand-file-name "assets" doom-user-dir))
  "Directory to store assets such as media files used in the user config.")

(setq +org-capture-todo-file zwei/org-agenda-todo-file
      org-default-notes-file zwei/org-agenda-todo-file)

;;; 01_vars-dirs ends here
