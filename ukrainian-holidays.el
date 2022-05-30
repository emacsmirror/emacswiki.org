Оновлена версія https://gitlab.com/LinuxOrgUa/ukrainian-holidays-el

<pre>
;; Щоб показувати лише українські свята, додай до .emacs:
;; (require 'ukrainian-holidays)
;; (setq calendar-holidays holiday-ukrainian-holidays)

(eval-when-compile
  (require 'calendar)
  (require 'holidays))
(defvar holiday-ukrainian-holidays nil
  "Ukrainian holidays")
(setq holiday-ukrainian-holidays
      `(
	(holiday-eastern-etc 0 "Воскресіння Христове")
	;; Дванадесяті свята
	(holiday-fixed 1 7 "Різдво")
	(holiday-fixed 1 19 "Водохреща")
	(holiday-fixed 2 15 "Стрітення")
	(holiday-fixed 4 7 "Благовіщення")
	(holiday-eastern-etc -7 "Вербна неділя")
	(holiday-eastern-etc 39 "Вознесіння Господнє")
	(holiday-eastern-etc 49 "Святої трійці (П'ятидесятниця)")
	(holiday-fixed 8 19 "Преображення Господнє (Яблучний Спас)")
	(holiday-fixed 8 28 "Успіння Пресвятої Богородиці")
	(holiday-fixed 9 21 "Різдво Пресвятої Богородиці")
	(holiday-fixed 9 27 "Воздвиження Хреста Господнього")
	(holiday-fixed 12 4 "Уведення в храм Пресвятої Богородиці")
	;; Інші релігійні свята
	(holiday-fixed 1 6 "Святий Вечір")
	(holiday-eastern-etc -3 "Великий четвер")
	(holiday-eastern-etc -2 "Велика пятниця")
	(holiday-eastern-etc -1 "Велика субота")
	(holiday-eastern-etc 1 "Світлий Понеділок")
	(holiday-eastern-etc 2 "Світлий Вівторок")
	(holiday-eastern-etc 7 "Провідна неділя")
	(holiday-eastern-etc 50 "Святого духа")
	(holiday-fixed 8 14 "Маковея (Медовий Спас)")
	(holiday-fixed 10 14 "Покрова Пресвятої Богородиці")
	;; Державні свята
	(holiday-fixed 1 1 "Новий рік")
	(holiday-fixed 3 8 "Міжнародний жіночий день")
	(holiday-fixed 5 1 "День Праці")
	(holiday-fixed 5 2 "День Праці")
	(holiday-fixed 5 9 "День Перемоги")
	(holiday-fixed 6 28 "День Конституції України")
	(holiday-fixed 8 24 "День Незалежності України")
	;; решта свят
	(holiday-fixed 1 13 "Старий Новий рік")
	(holiday-fixed 1 22 "День Соборності та Свободи України")
	(holiday-fixed 2 14 "День святого Валентина")
	(holiday-fixed 2 21 "Міжнародний день рідної мови")
	(holiday-fixed 2 23 "День захисника Вітчизни")
	(holiday-fixed 4 1 "День сміху")
      ))
(defun holiday-eastern-etc (&optional n string)
  "Date of Nth day after Orthodox Easter (named STRING), if visible in calendar window.
Negative values of N are interpreted as days before Easter.
STRING is used purely for display purposes.  The return value has
the form ((MONTH DAY YEAR) STRING), where the date is that of the
Nth day before or after Easter."
  (list (list
	 (if (= n 0)
	     (easter-eastern displayed-year)
	   (calendar-gregorian-from-absolute
	    (+ n (calendar-absolute-from-gregorian
		  (easter-eastern displayed-year)))))
	   string)))
(defun easter-eastern (year)
  (let* ((x (% (+ (* (% year 19) 19) 15) 30))
	 (day (- (+ x 10)
		 (% (+ (/ (* year 5) 4) x) 7))))
    (if (< day 31)
	(list 4 day year)
      (list 5 (- day 30) year))))
(provide 'ukrainian-holidays)
</pre>
