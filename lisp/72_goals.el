;;; 72_goals --- zwei-reviews config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to zwei-reviews
;;;
;;; Code:

(use-package! zweigtd-goals
  :after org
  :defer t
  :config
  (setq zweigtd-goals-file zwei/org-agenda-goals-file)
  (zweigtd-goals-init '((:name "1#PHYSICAL"   :key ?1 :color "#CC2200")
                        (:name "2#MENTAL"     :key ?2 :color "#008F40")
                        (:name "3#CODING"     :key ?3 :color "#42A5F5")
                        (:name "4#AUTOMATION" :key ?4 :color "#00FF33")
                        (:name "5#BUSINESS"   :key ?5 :color "#F5C400")
                        (:name "6#WANKER"     :key ?6 :color "#6A3B9F"))))

;;; 72_goals ends here
