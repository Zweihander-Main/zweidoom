;;; +roam.el -- doom/+roam.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains all org-roam related config (roam, anki, ect.).
;;; Anything that has to do with knowledge management should
;;; go here.
;;;
;;; Code:

(require 'org)
(require 'org-roam)


;; ==========
;;  org-roam
;; ==========
(setq  org-roam--extract-titles '(title alias)
       org-roam-tag-sources '(prop all-directories)
       org-roam-index-file (concat org-roam-directory "/20200724000434-index.org")
       org-roam-capture-templates
       '(("d" "default"
          plain
          (function org-roam--capture-get-point)
          "%?"
          :file-name "%<%Y%m%d%H%M%S>-${slug}"
          :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n#+ROAM_TAGS: \n- related :: \n\n* "
          :unnarrowed t))
       org-roam-capture-ref-templates
       '(("r" "ref"
          plain
          (function org-roam-capture--get-point)
          "%?"
          :file-name "websites/${slug}"
          :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}\n- related :: \n\n* "
          :unnarrowed t)))
(org-roam-db-build-cache) ;; Seems to be neccesary

;; Org-roam-server
(use-package! org-roam-server
  :after org-roam
  :defer t
  :config (setq org-roam-server-host "127.0.0.1"
                org-roam-server-port 38080
                org-roam-server-export-inline-images t
                org-roam-server-authenticate nil
                org-roam-server-network-poll t
                org-roam-server-network-arrows nil
                org-roam-server-network-label-truncate t
                org-roam-server-network-label-truncate-length 60
                org-roam-server-network-label-wrap-length 20))


;; =============
;;  Anki-editor
;; =============
(use-package! anki-editor
  :after org-roam
  :defer t
  :config
  (setq anki-editor-anki-connect-listening-port 38040)
  (defun filter-out-p (str backend comm)
    "Filter out <p> tags from STR when exporting Anki notes."
    (replace-regexp-in-string "\n<p>\\|</p>\n\\|<p>\\|</p>"
                              "" str))
  (setq anki-editor--ox-anki-html-backend
        (org-export-create-backend :parent 'html
                                   :filters '((:filter-paragraph . filter-out-p)))))

;;; +roam.el ends here
