(defun my-visit-8964museum ()
  "visit 8964museum.com to know our history"
  (interactive)
  (eww "https://8964museum.com"))

;; bind to key C-c m
(global-set-key (kbd "C-c m") 'my-visit-8964museum)
