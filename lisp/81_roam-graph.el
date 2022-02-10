;;; 81_roam-graph --- org-roam-graph config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-roam-graph' configuration.
;;;
;;; Code:

(use-package! org-roam-graph
  :commands (org-roam-graph)
  :config
  (setq org-roam-graph-viewer (pcase (zwei/which-linux-distro)
                                ("Arch" "/usr/bin/chromium")
                                (_ nil))))

;;; 81_roam-graph ends here
