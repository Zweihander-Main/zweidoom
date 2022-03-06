;;; 51_nyxt.el --- nyxt integration -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Code to work with Nyxt browser.
;;;
;;; Code:

;;;###autoload
(defun nyxt-connect ()
  "Connect to Nyxt with default settings."
  (interactive)
  (emacs-with-nyxt-connect "0.0.0.0" 4006))

(use-package! emacs-with-nyxt
  :defer t
  :commands (emacs-with-nyxt-connect)
  :config
  (require 'sly)
  (require 'dash)
  (require 's))

;;; 51_nyxt.el ends here
