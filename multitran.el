;;;

;; TODO add optional prompt C-u

(defun multitran-lookup-english (keyword)
  (interactive (list (thing-at-point 'word)))
  (w3m-goto-url (concat "http://multitran.ru/c/m.exe?l1=1&s=" keyword "&%CF%EE%E8%F1%EA=%CF%EE%E8%F1%EA"))
  (run-at-time 4 nil 'iconify-frame)
  )



