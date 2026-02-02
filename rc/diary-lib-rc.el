;; -*- lexical-binding: t -*-
;; Diary was part of calendar

;; Handy function for those who work Monday to Friday
(defun diary-workday (&optional time)
  (memq (nth 6 (decode-time time)) '(1 2 3 4 5)))

(setq diary-comment-start "#")

(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
