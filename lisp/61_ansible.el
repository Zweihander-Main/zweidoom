;;; 61_ansible.el --- settings for ansible -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Ansible settings.
;;;
;;; Code:

(after! ansible
  (setq ansible-vault-password-file "~/dev/sys/ansible/scripts/get-vault-secret.sh"))

;;; 60_ansible.el ends here
