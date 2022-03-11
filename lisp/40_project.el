;;; 40_project.el --- project config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Code to work with projectile/projects.
;;;
;;; Code:

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories ".cask")
  (add-to-list 'projectile-globally-ignored-directories ".log")
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;;; 40_project.el ends here
