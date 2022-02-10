;;; 81_roam-protocol --- org-roam-protocol config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-roam-protocol' configuration.
;;;
;;; Code:

(use-package! org-roam-protocol
  :after org-protocol
  :config
  (setq org-roam-capture-ref-templates
        '(("r" "ref (capture)"
           plain
           "%?"
           :target (file+head
                    ,(concat "bib/" "%<%Y%m%d%H%M%S>-${slug}" ".org")
                    ,(concat ":PROPERTIES\n"
                             ":ROAM_REFS: %{ref}\n"
                             ":END:\n"
                             "#+TITLE: ${title}\n"
                             "- related :: \n"
                             "\n"
                             "*  Notes\n"
                             "- "))
           :immediate-finish t
           :unnarrowed t))))

;;; 81_roam-protocol ends here
