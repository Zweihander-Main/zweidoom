;;; +custom-commands-test.el -- doom/org/+custom-commands-test.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Test custom commands.
;;;
;;; Code:

(load "~/.config/emacs/init.el")

(require 'org)
(require 'org-agenda)
(require 'buttercup)

(describe "The Doom config"
  (it "loads." ;; Sporadically
    (expect t :to-equal t)))

;;; +custom-commands-test.el ends here
