;;; 12_completion --- company completion -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Settings related to company completion
;;;
;;; Code:

(map! (:when (modulep! :completion company)
        :i "C-S-SPC" (cmds! (not (minibufferp)) #'company-complete-common)
        (:after company
                (:map company-active-map
                      "C-S-SPC"   #'company-complete-common))))

;;; 12_completion ends here
