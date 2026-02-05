;;; init-workspace.el --- Initialize workspace configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Workspace configurations.
;;

;;; Code:

(use-package tabspaces
  :diminish
  :commands tabspaces-mode
  :hook ((after-init . (lambda()
                         (tabspaces-mode 1)
                         (tab-bar-history-mode 1))))
  :custom
  (tab-bar-show nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-exclude-buffers '("*eat*" "*vterm*" "*shell*" "*eshell*" "*dashboard*"))
  (tab-bar-new-tab-choice "*scratch*")
  ;; sessions - disabled for faster startup/shutdown
  (tabspaces-session nil)
  (tabspaces-session-auto-restore nil)
  :config
  (with-no-warnings
    ;; Filter Buffers for Consult-Buffer
    (with-eval-after-load 'consult
      ;; hide full buffer list (still available with "b" prefix)
      (consult-customize consult-source-buffer :hidden t :default nil)
      ;; set consult-workspace buffer list
      (defvar consult-source-workspace
        (list :name     "Workspace Buffer"
              :narrow   ?w
              :history  'buffer-name-history
              :category 'buffer
              :state    #'consult--buffer-state
              :default  t
              :items    (lambda () (consult--buffer-query
                                    :predicate #'tabspaces--local-buffer-p
                                    :sort 'visibility
                                    :as #'buffer-name)))
        "Set workspace buffer list for consult-buffer.")
      (add-to-list 'consult-buffer-sources 'consult-source-workspace))))

(provide 'init-workspace)
;;; init-workspace.el ends here
