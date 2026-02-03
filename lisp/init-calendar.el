;;; init-calendar.el --- Initialize calendar configurations. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Calendar configuration.
;;

;;; Code:

(use-package calendar
  :ensure nil
  :custom
  (calendar-week-start-day 1) ;; Week starts on Monday
  (calendar-date-style 'iso)  ;; Use ISO date format (YYYY-MM-DD)
  (calendar-holidays nil))    ;; Disable default holidays


(provide 'init-calendar)
;;; init-calendar.el ends here
