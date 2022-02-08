;;; 81_checklists --- yas based checklists -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config for YAS based checklists.
;;;
;;; Code:

(map! :map org-mode-map
      :localleader
      :prefix ("z" . "zwei checklists")
      :desc "New checklist" "n" #'zwei/checklist-new
      :desc "Find checklist" "f" #'zwei/checklist-find)

;;;###autoload
(defun zwei/checklist-find ()
  "Open a checklist in `zwei/org-checklists-directory'."
  (interactive)
  (load! "modules/editor/snippets/autoload/snippets" doom-emacs-dir)
  (let ((+snippets-dir zwei/org-checklists-directory))
    (+snippets/find-private)))

;;;###autoload
(defun zwei/snippet-ensure-dir (dir)
  "Ensure DIR exists and offer to create if not."
  (unless (file-directory-p dir)
    (if (y-or-n-p (concat (format "%S doesn't exist. Create it?"
                          (abbreviate-file-name dir))))
        (make-directory dir t)
      (error "%S doesn't exist" (abbreviate-file-name dir)))))

;;;###autoload
(defun zwei/checklist-new ()
  "Create a new checklist in `zwei/org-checklists-directory'."
  (interactive)
  (load! "modules/editor/snippets/autoload/snippets" doom-emacs-dir)
  (let* ((major-mode 'org-mode)
         (default-directory
           (expand-file-name (symbol-name major-mode)
                             zwei/org-checklists-directory)))
    (zwei/snippet-ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (yas-expand-snippet (concat "# -*- mode: snippet -*-\n"
                                  "# name: $1\n"
                                  "# uuid: $2\n"
                                  "# --\n"
                                  "- [ ] $0"))
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))

(use-package! yasnippet
  :commands (zwei/checklist-find)
  :commands (zwei/checklist-new)
  :config
  (add-to-list 'yas-snippet-dirs 'zwei/org-checklists-directory)
  (eval-when-compile
    (declare-function yas-reload-all "yassnippet"))
  (yas-reload-all))

;;; 81_checklists ends here
