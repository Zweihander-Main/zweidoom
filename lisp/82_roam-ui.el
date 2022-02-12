;;; 82_roam-ui --- org-roam-ui config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-roam-ui' configuration.
;;;
;;; Code:

(use-package! websocket
  :commands (org-roam-ui-mode)
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil
        org-roam-ui-port 38080
        org-roam-ui-find-ref-title t
        org-roam-ui-retitle-ref-nodes t))

;;; 82_roam-ui ends here
