;;; 72_anki --- anki config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to integration with Anki SRS.
;;;
;;; Code:

;; Mappings
(map! :map org-mode-map
      :localleader
      (:prefix ("m" . "roam")
       (:prefix ("a" . "anki")
        :desc "Add a note" "a" #'zwei/add-anki-note
        :desc "Push notes" "p" #'anki-editor-push-notes
        :desc "Insert note" "i" #'anki-editor-insert-note
        :desc "Cloze region" "c" #'anki-editor-cloze-region
        :desc "Convert region to HTML" "h" #'anki-editor-convert-region-to-html)))

;;;###autoload
(defun filter-out-p (str)
  "Filter out <p> tags from STR when exporting Anki notes."
  (replace-regexp-in-string "\n<p>\\|</p>\n\\|<p>\\|</p>"
                            "" str))

;;;###autoload
(defun zwei/add-anki-note ()
  "Open temp org buffer to add an anki note."
  (interactive)
  (+popup-buffer (get-buffer-create "*anki*") '(:select t))
  (select-window (get-buffer-window "*anki*"))
  (org-mode)
  (insert "* Anki cards to add:\n")
  (anki-editor-insert-note))

(use-package! anki-editor
  :commands (anki-editor-insert-note
             anki-editor-push-notes
             anki-editor-cloze-region
             anki-editor-convert-region-to-html
             zwei/add-anki-note)
  :config
  (anki-editor-mode t)
  (setq anki-editor-anki-connect-listening-port 38040
        anki-editor-create-decks nil
        anki-editor-org-tags-as-anki-tags nil
        anki-editor--ox-anki-html-backend
        (org-export-create-backend :parent 'html
                                   :filters '((:filter-paragraph . filter-out-p))))

  ;; Override doom popup rules for org-capture, allow fullscreen
  (set-popup-rules!
    '(("^\\*anki\\*$"
       :size 0.5
       :select t
       :autosave ignore))))

;;; 72_anki ends here
