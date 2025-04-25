;;; 72_goals --- zwei-reviews config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to zwei-reviews
;;;
;;; Code:

(use-package! zweigtd-goals
  :after org
  :config
  (setq zweigtd-goals-file zwei/org-agenda-goals-file)
  ;; Using https://materialui.co/colors material+metro palettes
  (zweigtd-goals-init '((:name "1#PHYSICAL"   :key ?1 :color "#D32F2F")
                        (:name "2#MENTAL"     :key ?2 :color "#43A047")
                        (:name "3#EDUCATION"  :key ?3 :color "#F57C00")
                        (:name "4#AUTOMATION" :key ?4 :color "#64DD17")
                        (:name "5#PR"         :key ?5 :color "#42A5F5")
                        (:name "6#ENTR"       :key ?6 :color "#FBC02D")
                        (:name "7#WMARKETING" :key ?7 :color "#9C27B0")
                        (:name "8#WLIFE"      :key ?8 :color "#825A2C"))))

;;; 72_goals ends here
