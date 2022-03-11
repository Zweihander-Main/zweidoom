;;; 40_tree.el --- sidebar tree config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Code to work with treemacs/
;;;
;;; Code:

(after! treemacs

  (defun zwei/treemacs-ignore-func (name absolute-path)
    "Check to ignore NAME in ABSOLUTE-PATH in treemacs.
Uses `projectile-globally-ignored-*' to determine."
    (or (member name projectile-globally-ignored-files)
        (member t
                (mapcar (lambda (dir) (string-suffix-p dir absolute-path))
                        projectile-globally-ignored-directories))
        (member t
                (mapcar (lambda (ext) (string-suffix-p ext name))
                        projectile-globally-ignored-file-suffixes))))

  (add-to-list 'treemacs-ignored-file-predicates #'zwei/treemacs-ignore-func))


;;; 40_tree.el ends here
