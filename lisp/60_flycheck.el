;;; 60_flycheck --- flycheck config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `flycheck' config
;;;
;;; Code:

(eval-when-compile
  (defvar flycheck-disabled-checkers)
  (declare-function flycheck-error-new-at "flycheck")
  (declare-function org-current-line "org")
  (declare-function org-lint-link-to-local-file "org-lint")
  (declare-function org-element-parse-buffer "org-element"))

;;;###autoload
(defun zwei/add-checkdoc-back-in ()
  "Stop DOOM from removing checkdoc in config but keep the other mods."
  (setq flycheck-disabled-checkers
        (delete 'emacs-lisp-checkdoc flycheck-disabled-checkers)))

;;;###autoload
(defun flycheck-org-lint-start (checker callback)
  "Flycheck mode for org lint using flycheck CHECKER and CALLBACK args."
  (require 'org-lint)
  (funcall
   callback 'finished
   (save-excursion
     (mapcar
      (lambda (err)
        (goto-char (car err))
        (flycheck-error-new-at
         (org-current-line) (1+ (current-column))
         'warning (cadr err) :checker checker))
      (org-lint-link-to-local-file (org-element-parse-buffer))))))

(add-hook! 'emacs-lisp-mode-hook #'flycheck-elsa-setup)
(add-hook! 'emacs-lisp-mode-hook #'flycheck-package-setup)

(after! flycheck
  (flycheck-define-generic-checker 'org-lint
    "Syntax checker for org-lint."
    :start 'flycheck-org-lint-start
    :modes '(org-mode))

  (add-to-list 'flycheck-checkers 'org-lint)

  (setq-default flycheck-disabled-checkers '(proselint))

  (advice-add '+emacs-lisp-reduce-flycheck-errors-in-emacs-config-h
              :after #'zwei/add-checkdoc-back-in))

;;; 60_flycheck ends here
