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
(org-roam-db-build-cache) ;; Seems to be necessary
(when (string= (zwei/which-linux-distro) "Arch")
  (setq org-roam-graph-viewer "/usr/bin/chromium"))

;; Org-roam-server
(use-package! org-roam-server
  :after-call org-roam-server-mode
  :config
  (require 'org-roam-protocol)
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 38080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files t
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil  ; look into
        org-roam-server-cite-edge-dashes t
        org-roam-server-extra-cite-edge-options nil
        org-roam-server-style nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; Prevent doom error with smartparens and stop multiple instances
(after! org-roam
  (smartparens-global-mode -1)
  (unless org-roam-server-mode
    (org-roam-server-mode))
  (smartparens-global-mode 1))


;; =============
;;  Anki-editor
;; =============
(use-package! anki-editor
  :after org-roam
  :defer t
  :config
  (setq anki-editor-anki-connect-listening-port 38040)
  (defun filter-out-p (str)
    "Filter out <p> tags from STR when exporting Anki notes."
    (replace-regexp-in-string "\n<p>\\|</p>\n\\|<p>\\|</p>"
                              "" str))
  (setq anki-editor--ox-anki-html-backend
        (org-export-create-backend :parent 'html
                                   :filters '((:filter-paragraph . filter-out-p)))))


;; ===========================
;;   kindle-highlights-to-org
;; ===========================
(use-package! kindle-highlights-to-org
  :after org-roam
  :defer t)


;;; +roam.el ends here
