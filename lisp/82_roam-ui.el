;;; 82_roam-ui --- org-roam-ui config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-roam-ui' configuration.
;;;
;;; Code:

(eval-when-compile
  (defvar oru-ws)
  (declare-function f-split "f"))

;;;###autoload
(defun zwei/org-roam-ui-dirs-to-tags (args)
  "Add subdirectories as tags to roam nodes. ARGS same as `websocket-send-text'.
Advise `websocket-send-text' using `:filter-args' combinator to use.
Will only change the output if using the `oru-ws' websocket and the text arg is
of type `graphdata' (coming from `org-roam-ui--send-graphdata')."
  (let* ((websocket (nth 0 args))
         (raw-json-text (nth 1 args))
         (raw-text (json-read-from-string raw-json-text))
         (type-cons (assoc 'type raw-text)))
    (if (and (eq websocket oru-ws)
             (string= (cdr type-cons) "graphdata"))
        (let* ((response (cdr (assoc 'data raw-text)))
               (nodes (cdr (nth 0 response)))
               (tags (nth 2 response)))
          (when (vectorp (cdr tags))
            (setq tags `(tags ,(aref (cdr tags) 0))))
          (setf (cdr (elt response 0))
                (mapcar
                 (lambda (node)
                   (let* ((node-tags (assoc 'tags node))
                          (properties (cdr (assoc 'properties node)))
                          (file-name (cdr (assoc 'FILE properties)))
                          (dir-tag
                           (if-let
                               ((dirs (file-name-directory
                                       (file-relative-name file-name
                                                           org-roam-directory))))
                               (format "%s" (string-join (f-split dirs) "/")))))
                     (when (vectorp (cdr node-tags))
                       (setq node-tags `(tags ,(aref (cdr node-tags) 0))))
                     (when dir-tag
                       (if (assoc 'tags node-tags)
                           (setq node-tags (append node-tags `(,dir-tag)))
                         (setq node-tags `(tags ,dir-tag)))
                       (add-to-list 'tags dir-tag t))
                     (setf (alist-get 'tags node) (cdr node-tags))
                     node))
                 nodes))
          (setf (elt response 2) tags)
          `(,oru-ws ,(json-encode `((type . "graphdata") (data . ,response)))))
      args)))

(use-package! websocket
  :commands (org-roam-ui-mode)
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode)
  :init
  (advice-add 'websocket-send-text :filter-args #'zwei/org-roam-ui-dirs-to-tags)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil
        org-roam-ui-port 38080
        org-roam-ui-find-ref-title t
        org-roam-ui-retitle-ref-nodes t))

;;; 82_roam-ui ends here
