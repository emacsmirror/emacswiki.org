;;; xml-weather.el --- Get xml-weather infos in emacs. 
;; 
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.

;; Created: lun. août  3 10:41:46 2009 (+0200)
;; Version: 
;; X-URL: http://mercurial.intuxication.org/hg/xml-weather  
;; Keywords: 
;; Compatibility: emacs23
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Commentary:
;;
;; Install:
;; =======
;;
;; 1)
;; To obtain the feed, you must first register with weather.com[1] to obtain a license key.
;; The registration is free but mandatory.
;; Once you've registered, you'll receive a confirmation email and a link to the software developer's kit.
;; Included in the kit is a comprehensive user's guide, numerous icons, and instructions (and restrictions)
;; on use of The Weather Channel logos.
;; [1]http://www.weather.com/services/xmloap.html
;;
;; 2)
;; Once you have your Partner ID and License Key, you have two ways to set them up:
;;
;; A) The bad way:
;;    Add to .emacs:
;;    (setq xml-weather-login "your Partner ID")
;;    (setq xml-weather-key "your License Key")
;;
;; B) The good way:
;;    Be sure to require `auth-sources' and set it up correctly.
;;    Add to your ~/.authinfo file this line:(change login and pwd)
;;    machine xoap.weather.com port http login <Partner ID> password <License Key>
;;
;; 3) Add to .emacs (be sure xml-weather.el is in your load-path):
;;    (require 'xml-weather)
;;
;; 4) (Facultative) Get the icons set from the link to the software developer's kit
;;    you will find in your email.
;;    Put the icons in the directory of your choice and set it in your .emacs:
;;    (setq xml-weather-default-icons-directory "path/to/your/icons/31x31")
;;    NOTE: I use the 31x31 directory but you can choose bigger icons if you want.
;;          If `xml-weather-default-icons-directory' is nil or doesn't exist,
;;          your builtin will be displayed with text only.
;;
;; 5) (Facultative) Get the moon icons set:
;;    http://mercurial.intuxication.org/hg/xml-weather/file/66f18bcb2ed8/moon-icons2
;;    And
;;    (setq xml-weather-moon-icons-directory "Path/to/moon/icons/31X31")
;;    NOTE: If `xml-weather-moon-icons-directory' is not found, an empty image will be
;;    displayed.
;;
;; Usage:
;; =====
;; M-x xml-weather-today-at (you will have a button for forecast)
;; M-x xml-weather-forecast-at (go straight to forecast)
;; In these two functions you will have two prompt:
;; One (e.g CityName) where you enter the name of the place where you want meteo
;; and another where you will have completion for all the possible city names
;; xml-weather know.(Hit TAB ==> must match).
;; xml-weather.el provide a ticker that show a builtin for current conditions
;; all the `xml-weather-timer-delay' seconds.
;; You will have to set your current location with `xml-weather-default-id'.
;; You can fetch it with M-x xml-weather-show-id.
;; Run the ticker with M-x xml-weather-run-ticker
;; Stop timer with M-x xml-weather-ticker-cancel-timer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)
(require 'derived)
(require 'url)


;;;###autoload
(defvar xml-weather-format-id-url
  "http://xoap.weather.com/search/search?where=%s")

;;;###autoload
(defvar xml-weather-format-xml-from-id-url ; id, unit=m,day-forecast=5,login,key
  "http://xoap.weather.com/weather/local/%s?cc=*&unit=%s&dayf=%s&prod=xoap&par=%s&key=%s")

;;;###autoload
(defvar xml-weather-unit "m"
  "*m mean metric, you will have wind speed in km/h, temperature in °C and so on.")

;;;###autoload
(defvar xml-weather-temperature-sigle (if (equal xml-weather-unit "m") "°C" "°F")
  "*Temperature sigle to use depending you use metric or english system.")

;;;###autoload
(defvar xml-weather-wind-speed-sigle (if (equal xml-weather-unit "m") " Km/h" " Mp/h")
  "*Wind speed sigle to use depending you use metric or english system.")

;;;###autoload
(defvar xml-weather-login nil
  "*Your xml-weather Login.
You should not set this variable directly. See `xml-weather-authentify'.
If you have an xml-weather entry in ~/.authinfo, leave it to nil.")

;;;###autoload
(defvar xml-weather-key nil
  "*Your xml-weather key.
You should not set this variable directly. See `xml-weather-authentify'.
If you have an xml-weather entry in ~/.authinfo, leave it to nil.")

;;;###autoload
(defvar xml-weather-day-forecast-num 5
  "*Number of days for forecast; Maximum 5.")

;;;###autoload
(defvar xml-weather-default-show-message-times 1
  "*Number of times ticker will show message before stopping.")

;;;###autoload
(defvar xml-weather-default-location "Toulon, France"
  "*Your favorite location for xml-weather builtin.
You should get it with `xml-weather-show-id' to avoid error.")

;;;###autoload
(defvar xml-weather-default-id "FRXX0098"
  "*The ID corresponding to `xml-weather-default-location'.
You should get it with `xml-weather-show-id' to avoid error.")

;;;###autoload
(defvar xml-weather-timer-delay 3600
  "*Refresh the xml-weather Builtin all the `xml-weather-timer-delay' seconds.
Only used with ticker.")

;;;###autoload
(defvar xml-weather-default-icons-directory
  "/home/thierry/download/xml-weather-icons/icons/31x31"
  "Path to your icons directory given with the xml-weather kit.
You will have errors if you use another icons set than the xml-weather one.")

(defvar xml-weather-moon-icons-directory
  "~/download/xml-weather-icons/moon-icons2/31X31/"
  "The directory where your moon icons are.")

;;;###autoload
(defvar xml-weather-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'xml-weather-quit)
    (define-key map (kbd "<tab>") 'xml-weather-next-day)
    (define-key map (kbd "C-<tab>")   'xml-weather-precedent-day)
    (define-key map (kbd "S-<down>") 'xml-weather-next-day)
    (define-key map (kbd "S-<up>")   'xml-weather-precedent-day)
    (define-key map (kbd "<down>")   'xml-weather-next-button)
    (define-key map (kbd "<up>")     'xml-weather-precedent-button)
    (define-key map (kbd "<right>")  'xml-weather-press-button)
    (define-key map (kbd "<left>")   'xml-weather-toggle-today-forecast)
    map)
  "Keymap used for `xml-weather' commands.")

;;;###autoload
(define-derived-mode xml-weather-mode
    text-mode "xml-weather"
    "Major mode to get info from xml-weather.

Special commands:
\\{xml-weather-mode-map}")

;;;###autoload
(defun xml-weather-quit ()
  "Quit xml-weather without killing buffer."
  (interactive)
  (quit-window))

;;;###autoload
(defun xml-weather-next-day ()
  "Go to next day in xml-weather forecast."
  (interactive)
  (forward-char 1) (search-forward "*" nil t) (forward-line 0)
  (recenter 0))

;;;###autoload
(defun xml-weather-precedent-day ()
  "Go to precedent day in xml-weather forecast."
  (interactive)
  (forward-char -1) (search-backward "*" nil t) (forward-line 0)
  (recenter 0))

;;;###autoload
(defun xml-weather-next-button ()
  (interactive)
  (forward-button 1))

;;;###autoload
(defun xml-weather-precedent-button ()
  (interactive)
  (forward-button -1))

;;;###autoload
(defun xml-weather-press-button ()
  (interactive)
  (when (button-at (point))
    (push-button)))

(defvar xml-weather-today-buffer-p nil)
;;;###autoload
(defun xml-weather-toggle-today-forecast ()
  (interactive)
  (setq xml-weather-today-buffer-p (not xml-weather-today-buffer-p))
  (if xml-weather-today-buffer-p
      (xml-weather-forecast xml-weather-last-id)
      (xml-weather-now xml-weather-last-id)))

(defun xml-weather-authentify ()
  "Authentify user from .authinfo file.

You have to setup correctly `auth-source' to make this function
finding the path of your .authinfo file that is normally ~/.authinfo.
See \(info \"\(auth\)Top\"\).
Entry in .authinfo should be:

machine xoap.weather.com port http login xxxxx password xxxxxx

This function is intended to be called inside a `let' binding."
  (let ((xml-weather-auth
         (auth-source-user-or-password  '("login" "password")
                                        "xoap.weather.com"
                                        "http")))
    (when xml-weather-auth
      (setq xml-weather-login (car xml-weather-auth)
            xml-weather-key (cadr xml-weather-auth))
      nil)))

;; First step: Get ID of places
(defun xml-weather-get-place-id (place)
  "Return an alist of all ID corresponding to PLACE.
Each element is composed of a pair like \(\"Toulon, France\" . \"FRXX0098\"\)."
  (let* ((url              (format xml-weather-format-id-url place))
         (url-request-data (encode-coding-string place 'utf-8))
         (data             (with-current-buffer (url-retrieve-synchronously url)
                             (buffer-string))))
    (with-temp-buffer
      (erase-buffer)
      (insert data)
      (loop
         with l = (xml-get-children (car (xml-parse-region (point-min)
                                                           (point-max)))
                                    'loc)
         for i in l
         collect (cons (car (last i)) (xml-get-attribute i 'id))))))

;; Second step: Get the xml info for the ID choosen:
;; http://xoap.weather.com/weather/local/[locid]

;; Replace the [locid], of course, with the location ID obtained in the previous step.
;; Appended to this URL is a mix of other parameters,
;; some required and some optional. A typical example might be:

;; http://xoap.weather.com/weather/local/NLXX0002?cc=*&dayf=5
;; &prod=xoap&par=[partner id]&key=[license key]

(defun xml-weather-get-info-on-id (id)
  "Return an xml buffer with xml-weather infos on ID."
  (let* (xml-weather-login
         xml-weather-key
         (url  (progn
                 (unless (and xml-weather-login xml-weather-key)
                   (xml-weather-authentify))
                 (when (and xml-weather-login xml-weather-key)
                   (format xml-weather-format-xml-from-id-url
                           id
                           xml-weather-unit
                           xml-weather-day-forecast-num
                           xml-weather-login
                           xml-weather-key))))
         (data (when url (with-current-buffer (url-retrieve-synchronously url)
                           (buffer-string)))))
    (if data
        (with-current-buffer (get-buffer-create "*xml-weather*")
          (erase-buffer)
          (insert data))
        (error "Fail to retrieve data, please set up your login and password \
correctly or verify if your network is up."))))

;;;###autoload
(defun xml-weather-show-id (place)
  "Interactively show ID corresponding to PLACE."
  (interactive "sName: ")
  (let* ((id-list   (xml-weather-get-place-id place))
         (name-list (loop for i in id-list collect (car i)))
         (id-name   (completing-read "Choose a place: " name-list nil t))
         (id        (cdr (assoc id-name id-list))))
    (message "ID code for %s is %s" id-name id)))

(defun xml-weather-set-number-file-name (arg)
  "When ARG < 10 add a 0 before it.
ARG can be a string or a number."
  (let ((n (if (stringp arg) (string-to-number arg) arg)))
    (if (and (< n 10) (> n 0))
        (substring (int-to-string (/ (float n) 100)) 2)
        (int-to-string n))))

;; Third step convert xml info to alist
(defun xml-weather-get-alist ()
  "Parse the xml buffer and return an alist of all infos."
  (with-current-buffer "*xml-weather*"
    (let* ((loc           (xml-get-children (car (xml-parse-region (point-min)
                                                                   (point-max)))
                                            'loc))
           (cc            (xml-get-children (car (xml-parse-region (point-min)
                                                                   (point-max)))
                                            'cc))
           (info-cc       (loop for i in cc
                             for lsup = (caddr (assoc 'lsup i))
                             for obst = (caddr (assoc 'obst i))
                             for tmp = (caddr (assoc 'tmp i))
                             for flik = (caddr (assoc 'flik i))
                             for wea = (caddr (assoc 't i))
                             for icon = (xml-weather-set-number-file-name (caddr (assoc 'icon i)))
                             for bar = (caddr (assoc 'r (assoc 'bar i)))
                             for wind-dir-d = (caddr (assoc 'd (assoc 'wind i)))
                             for wind-dir = (caddr (assoc 't (assoc 'wind i)))
                             for gust = (caddr (assoc 'gust (assoc 'wind i)))
                             for moon = (caddr (assoc 't (assoc 'moon i)))
                             collect (list (cons "Date:" (or lsup ""))
                                           (cons "Observatory:" (or obst ""))
                                           (cons "Temperature:" (concat (or tmp "") xml-weather-temperature-sigle))
                                           (cons "Feel Like:" (concat (or flik "") xml-weather-temperature-sigle))
                                           (cons "Cond:" (or (list (concat icon ".png") wea) ""))
                                           (cons "Pression:" (or bar ""))
                                           (cons "Wind dir:" (or (concat wind-dir  "(" wind-dir-d "°)") ""))
                                           (cons "Gust:" (or gust ""))
                                           (cons "Moon:" (or moon "")))))
           (today-info    (loop for i in loc
                             for dnam = (caddr (assoc 'dnam i))
                             for tm = (caddr (assoc 'tm i))
                             for lat = (caddr (assoc 'lat i))
                             for lon = (caddr (assoc 'lon i))
                             for sunr = (caddr (assoc 'sunr i))
                             for suns = (caddr (assoc 'suns i))
                             collect (cons (concat dnam " " tm)
                                           (append (list (cons "Latitude:" (or lat ""))
                                                         (cons "Longitude: " (or lon ""))
                                                         (cons "Sunrise:" (or sunr ""))
                                                         (cons "Sunset:" (or suns "")))
                                                   (car info-cc)))))
           (dayf          (xml-get-children (car (xml-parse-region (point-min)
                                                                   (point-max)))
                                            'dayf))
           (day-list      (loop for i in (xml-get-children (car dayf) 'day)
                             collect i))
           (morning-alist (loop for i in day-list
                             for d = (concat (cdr (assoc 't (cadr i))) " " (cdr (assoc 'dt (cadr i))))
                             for hi-temp = (caddr (assoc 'hi (cdr i)))
                             for low-temp = (caddr (assoc 'low (cdr i)))
                             for sunr = (caddr (assoc 'sunr (cdr i)))
                             for suns = (caddr (assoc 'suns (cdr i)))
                             for wind-dir = (caddr (assoc 't (assoc 'wind (assoc 'part (cdr i)))))
                             for wind-spd = (caddr (assoc 's (assoc 'wind (assoc 'part (cdr i)))))
                             for wea = (caddr (assoc 't (assoc 'part (cdr i))))
                             for icon = (xml-weather-set-number-file-name (caddr (assoc 'icon (assoc 'part (cdr i)))))
                             for hmid = (caddr (assoc 'hmid (assoc 'part (cdr i))))
                             collect (cons d (list (cons "maxi:" (concat (or hi-temp "") xml-weather-temperature-sigle))
                                                   (cons "mini:" (concat (or low-temp "") xml-weather-temperature-sigle))
                                                   (cons "sunrise:" (or sunr ""))
                                                   (cons "sunset:" (or suns ""))
                                                   (cons "Wind direction:" (or wind-dir ""))
                                                   (cons "Wind speed:" (concat (or wind-spd "") xml-weather-wind-speed-sigle))
                                                   (cons "Cond:" (or (list (concat icon ".png") wea) ""))
                                                   (cons "Humidity:" (concat (or hmid "") "%"))))))
           (night-alist   (loop for i in day-list
                             for d = (concat (cdr (assoc 't (cadr i))) " " (cdr (assoc 'dt (cadr i))))
                             for hi-temp = (caddr (assoc 'hi (cdr i)))
                             for low-temp = (caddr (assoc 'low (cdr i)))
                             for sunr = (caddr (assoc 'sunr (cdr i)))
                             for suns = (caddr (assoc 'suns (cdr i)))
                             for all-part1 = (remove (assoc 'part (cdr i)) (cdr i))
                             for part2 = (assoc 'part all-part1)
                             for wind-dir = (caddr (assoc 't (assoc 'wind part2)))
                             for wind-spd = (caddr (assoc 's (assoc 'wind part2)))
                             for wea = (caddr (assoc 't part2))
                             for icon = (xml-weather-set-number-file-name (caddr (assoc 'icon (assoc 'part (cdr i)))))
                             for hmid = (caddr (assoc 'hmid part2))
                             collect (cons d (list (cons "maxi:" (concat (or hi-temp "") xml-weather-temperature-sigle))
                                                   (cons "mini:" (concat (or low-temp "") xml-weather-temperature-sigle))
                                                   (cons "sunrise:" (or sunr ""))
                                                   (cons "sunset:" (or suns ""))
                                                   (cons "Wind direction:" (or wind-dir ""))
                                                   (cons "Wind speed:" (concat (or wind-spd "") xml-weather-wind-speed-sigle))
                                                   (cons "Cond:" (or (list (concat icon ".png") wea) ""))
                                                   (cons "Humidity:" (concat (or hmid "") "%")))))))
      (setq morning-alist (cons 'morning morning-alist))
      (setq night-alist (cons 'night night-alist))
      (setq today-info (cons 'info today-info))
      (list today-info morning-alist night-alist))))

;; Last step pprint the infos in alist
(defun xml-weather-pprint-today ()
  "Print the xml-weather info of current day in *xml-weather-meteo* buffer."
  (let ((data (xml-weather-get-alist)))
    (with-current-buffer (get-buffer-create "*xml-weather-meteo*")
      (erase-buffer)
      (insert (propertize "* XML-WEATHER\n  ===========\n\n" 'face '((:foreground "Lightgreen"))))
      (loop for i in (cadr (assoc 'info data))
         if (listp i)
         do
           (xml-weather-insert-maybe-icons i)
         else
         do
           (insert (concat i "\n\n")))))
  (switch-to-buffer "*xml-weather-meteo*")
  (goto-char (point-max))
  (newline)
  (insert-button "[Forecast for next 4 days]"
                 'action 'xml-weather-button-func1
                 'face '((:background "green")))
  (newline 2)
  (insert-button "[New Search]"
                 'action 'xml-weather-button-func3
                 'face '((:background "green")))
  (newline 2)
  (insert-button "[Refresh]"
                 'action 'xml-weather-button-func4
                 'face '((:background "green")))
  (goto-char (point-min))
  (save-excursion
    (align-regexp (point-min) (point-max) "\\(:\\)" 1 1 nil))
  (xml-weather-mode))


(defun xml-weather-insert-maybe-icons (elm)
  "Insert infos in all entries of an xml-weather builtin.
Insert an icon in the Cond: entry only if `xml-weather-default-icons-directory' exists."
  (insert (concat "  " (car elm)))
  (let ((info (if (eq (safe-length elm) 1)
                  (cdr elm)
                  (car (last elm)))))
    (if info
        (cond ((and (file-exists-p xml-weather-default-icons-directory) (equal (car elm) "Cond:"))
               (let* ((fname (cadr elm))
                      (img   (unless (equal fname ".png")
                               (create-image (expand-file-name fname xml-weather-default-icons-directory)))))
                 (if img
                     (progn
                       (insert-image img)
                       (insert (propertize info 'face '((:foreground "red"))) "\n"))
                     (insert ""))))
              ((and (file-exists-p xml-weather-moon-icons-directory) (equal (car elm) "Moon:"))
               (let* ((lsname (split-string info))
                      (fname  (if (< (length lsname) 2)
                                  (concat (downcase (car lsname)) ".jpg")
                                  (concat (downcase (car lsname)) "_" (downcase (cadr lsname)) ".jpg")))
                      (img    (unless (or (not fname) (equal fname ".jpg"))
                                (create-image (expand-file-name fname xml-weather-moon-icons-directory)))))
                 (if img
                     (progn
                       (insert-image img)
                       (insert (propertize info 'face '((:foreground "red"))) "\n"))
                     (insert ""))))
              (t (insert (propertize info 'face '((:foreground "red"))) "\n")))
        (insert ""))))
    
(defun xml-weather-pprint-forecast (station)
  "Print the xml-weather info of forecast for STATION in *xml-weather-meteo* buffer."
  (let ((data (xml-weather-get-alist)))
    (with-current-buffer (get-buffer-create "*xml-weather-meteo*")
      (erase-buffer)
      (insert (propertize "* XML-WEATHER\n  ===========\n\n" 'face '((:foreground "Lightgreen"))))
      (insert (concat (propertize station 'face '((:foreground "magenta"))) "\n"))
      (loop
         for i in (assoc 'morning data)
         if (listp i) 
         do
           (loop
              for m in i
              if (listp m)
              do
                (xml-weather-insert-maybe-icons m)
              else
              do
                (insert (concat "\n* " (propertize m 'face '((:foreground "blue")))"\n\n"))
                (insert (propertize "Morning:\n" 'face '((:foreground "lightgreen")))))
         for j in (assoc 'night data)
         if (listp j)
         do
           (insert (propertize "Afternoon:\n" 'face '((:foreground "lightgreen"))))
           (loop
              for a in j
              if (listp a)
              do
                (xml-weather-insert-maybe-icons a)))
      (insert "\n\n")
      (insert-button "[Back To Today weather]"
                     'action 'xml-weather-button-func2
                     'face '((:background "green"))))
    (switch-to-buffer "*xml-weather-meteo*")
    (goto-char (point-min))
    (save-excursion
      (align-regexp (point-min) (point-max) "\\(:\\)" 1 1 nil))
    (xml-weather-mode)))

(defvar xml-weather-last-id nil
  "Remember the last ID used. it is a pair.")
(defun xml-weather-now (id-pair &optional update)
  "Call non interactively the pprinter for today weather."
  (let ((id      (cdr id-pair)))
    (setq xml-weather-last-id id-pair)
    (when update
      (xml-weather-get-info-on-id id))
    (xml-weather-pprint-today)))

(defun xml-weather-forecast (id-pair &optional update)
  "Call non interactively the pprinter for forecast."
  (let ((id      (cdr id-pair))
        (station (car id-pair)))
  (setq xml-weather-last-id id-pair)
  (when update
    (xml-weather-get-info-on-id  id))
  (xml-weather-pprint-forecast station)))

(defun xml-weather-button-func1 (button)
  "Function used by the forecast button."
  (setq xml-weather-today-buffer-p (not xml-weather-today-buffer-p))
  (xml-weather-forecast xml-weather-last-id))

(defun xml-weather-button-func2 (button)
  "Function used by the today weather button."
  (setq xml-weather-today-buffer-p (not xml-weather-today-buffer-p))
  (xml-weather-now xml-weather-last-id))

(defun xml-weather-button-func3 (button)
  "Function used by the search button."
  (let ((place (read-string "CityName: ")))
    (xml-weather-today-at place)))

(defun xml-weather-button-func4 (button)
  "Function used by the refresh button."
  (xml-weather-now xml-weather-last-id 'update))

;;;###autoload
(defun xml-weather-today-at (place)
  "Call interactively xml weather for meteo of today."
  (interactive "sCityName: ")
  (let* ((id-list   (xml-weather-get-place-id place))
         (name-list (loop for i in id-list collect (car i)))
         (id        (completing-read "Choose a place: " name-list nil t))
         (id-pair   (assoc id id-list)))
    (xml-weather-now id-pair 'update)))

;;;###autoload
(defun xml-weather-forecast-at (place)
  "Call interactively xml weather for forecast."
  (interactive "sCityName: ")
  (let* ((id-list   (xml-weather-get-place-id place))
         (name-list (loop for i in id-list collect (car i)))
         (id        (completing-read "Choose a place: " name-list nil t))
         (id-pair   (assoc id id-list)))
    (xml-weather-forecast id-pair 'update)))

;;;###autoload
(defun* xml-weather-today-favorite (&optional (location xml-weather-default-location)
                                              (id xml-weather-default-id)) 
  "Display an xml-weather builtin for `location'.
If `location' and/or `id' are nil, `xml-weather-today-at' will be used."
  (interactive)
  (if (and location
           id)
      (let ((id-pair (cons location
                           id)))
        (xml-weather-now id-pair 'update))
      (let ((place (read-string "CityName: ")))
        (xml-weather-today-at place))))

;;; xml-weather ticker
(defvar xml-weather-ticker-timer1 nil)
(defvar xml-weather-ticker-timer2 nil)

(defun xml-weather-get-today-list ()
  "Return a list that will be used to setup the ticker message.
The list is made with the current xml weather buffer."
  (let ((data (xml-weather-get-alist)))
    (loop for i in (cadr (assoc 'info data))
       if (listp i)
       collect (if (eq (safe-length i) 1)
                   (concat (car i) (cdr i))
                   (concat (car i) (car (last i)))) into a
       else
       collect i into b
       finally return (append a b))))

(defun xml-weather-get-today-string ()
  "Setup the message for ticker from the current xml buffer."
  (mapconcat 'identity (xml-weather-get-today-list) " | "))


(defun* xml-weather-message (&rest msg)
  "Send a rolling message of today xml-weather in minibuffer.
It will stop if keyboard is used or after `xml-weather-default-show-message-times'."
  (setq msg (concat "  <XML-WEATHER-BUILTIN>: "
                    (apply 'format msg) ; == (car msg)
                    "            "))
  (let* ((minibuf-size   (window-width (minibuffer-window)))
         (start-msg-size (+ 1 (length "[<] ")))
         (width          (- minibuf-size start-msg-size))
         (msglen         (length msg))
         submsg
         (count          0)
         (normal-range   (- msglen width)))
    (if (< msglen width)
        (message "%s" msg)
        (while t
          (when (> count xml-weather-default-show-message-times)
            (return-from xml-weather-message
              (when xml-weather-ticker-timer1
                (message "Next xml-weather Builtin in %d mn" (/ xml-weather-timer-delay 60)))))
          (incf count)
          (dotimes (i msglen)
            (setq submsg (if (< i normal-range)
                             (substring msg i (+ i width))
                             ;; Rolling is needed.
                             (concat (substring msg i)
                                     (substring msg 0 (- (+ i width) msglen)))))
            (message "[<] %s" submsg)
            (when (eq i 0) (incf count))
            (unless (sit-for (cond
                               ((eq i 0) 1.0)
                               (t 0.2)))
              (return-from xml-weather-message)))
          (garbage-collect)))))


(defun xml-weather-run-message-builtin (&optional id)
  "Run `xml-weather-message' with current infos.
If optional arg `id' is used refresh infos of this `id'."
  (when id
    (xml-weather-get-info-on-id id))
  (xml-weather-message (xml-weather-get-today-string)))

(defun xml-weather-start-ticker-timers ()
  "Start all timers used by `xml-weather-run-ticker'.
`xml-weather-ticker-timer1' update xml-weather buffer all the `xml-weather-timer-delay'
`xml-weather-ticker-timer2' run an idle timer every 120 sec."
  (setq xml-weather-ticker-timer1
        (run-with-timer 60
                        xml-weather-timer-delay
                        #'(lambda ()
                            (xml-weather-get-info-on-id xml-weather-default-id))))
  (setq xml-weather-ticker-timer2
        (run-with-idle-timer 120 'repeat
                             #'(lambda ()
                                 (xml-weather-run-message-builtin)))))
  
  
;;;###autoload
(defun xml-weather-run-ticker ()
  "Start interactively the xml weather timers for ticker.
A rolling message will be sent all the 65 sec and updated all the `xml-weather-timer-delay' sec."
  (interactive)
  (when (or xml-weather-ticker-timer1
            xml-weather-ticker-timer2)
    (xml-weather-ticker-cancel-timer))
  (xml-weather-start-ticker-timers))

;;;###autoload
(defun xml-weather-ticker-cancel-timer ()
  "Kill all the xml weather timers and stop ticker."
  (interactive)
  (cancel-timer xml-weather-ticker-timer1)
  (setq xml-weather-ticker-timer1 nil)
  (cancel-timer xml-weather-ticker-timer2)
  (setq xml-weather-ticker-timer2 nil))

;; Provide
(provide 'xml-weather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml-weather.el ends here
