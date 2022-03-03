;;; 72_reviews --- zwei-reviews config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to zwei-reviews
;;;
;;; Code:

(use-package! zweigtd-reviews
  :after-call org-capture
  :config
  (setq zweigtd-reviews-file zwei/org-agenda-reviews-file)
  (zweigtd-reviews-default-bootstrap))

;; Override doom popup rules for org-capture, allow fullscreen
(set-popup-rules!
  '(("^\\*Capture\\*$\\|CAPTURE-.*$"
     :size 1.00 ; Allow full screen for reviews
     :quit nil
     :select t
     :autosave ignore)))

;;; 72_reviews ends here
