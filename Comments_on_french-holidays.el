Replace	(holiday-float 5 0 4 "Fête des mères") by 

(if (not (equal (calendar-nth-named-day -1 0 5 displayed-year) (caar (holiday-easter-etc 49))))
    (holiday-float 5 0 -1 "Fête des mères")
  (holiday-float 6 0 1 "Fête des mères"))

-- RwN 2024-03-05 16:21 UTC

