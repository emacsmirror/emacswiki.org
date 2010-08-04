;;; yahoo-weather.el --- Get the weather from yahoo.
(defconst yahoo-weather-version "0.2a")
;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
(defgroup yahoo-weather '()
  "A small package to retrieve the weather from a yahoo webservice.

While this package does have one small command to show you the weahter
it is really meant to interface with other packages.  Currently it can
interface with jiseki.el, which is available at
http://www.emacswiki.org/emacs/jiseki.el")
  
;;; Installation:
;; Put yahoo-weather.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;   (require 'yahoo-weather)

;; you'll need to set up the location you want the weather from, and
;; whether or not you want ferenheight or celcius.  Do this through
;; the customization interface:
;   M-x customize-group yahoo-weather

;; Finally, if you are using Jiseki, you will want to add it to your
;; jiseki-sources variable like so:
; (setq jiseki-sources (append jiseki-sources (list yahoo-weather-jiseki-source)))


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `yahoo-weather-clear-cache'
;;    Sometimes the cache is bunk, so lets clear it.
;;  `yahoo-weather-conditions'
;;    Show the current conditions.  Also, an example of how to use yahoo-weather.el.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `yahoo-weather-location'
;;    Location to get yahoo weather from.
;;    default = "CAXX0054"
;;  `yahoo-weather-format'
;;    Format for degrees.  c for celcius f for ferinheight.
;;    default = "c"

;;; elisp:
;; 

;;; TODO:
;; 

;;; BUGS:
;; - pasrsing of the 2nd forcast is not working

;;; CHANGELOG:
;; v 0.1 - Initial release
;; v 0.2 - Fixed white-space at the top that broke emacs wiki. :s  Sorry.
;;       - Added jiseki source.

;;* custom
(defcustom yahoo-weather-location "CAXX0054"
   "Location to get yahoo weather from.
Use the search at http://weather.yahoo.com/ to get your location.  it is
inside of the result url:
i.e. http://weather.yahoo.com/forecast/CAXX0054.html
your locaiton is CAXX0054"
   :type 'string
   :group 'yahoo-weather)

;;* custom 
(defcustom yahoo-weather-format "c"
  "Format for degrees.  c for celcius f for ferinheight."
  :type 'string
  :group 'yahoo-weather)

;;* const retreieve
(defconst yahoo-weather-url "http://weather.yahooapis.com/forecastrss?p=%s&u=%s"
  "Url of weather, as format string.")

;;* const
(defconst yahoo-weather-stale-cache-time (* 60 30)
  "Time in seconds for the cache to become stale.")

;;* retrieve
(defun yahoo-weather-get ()
   "Return the raw xml from yahoo."
   (let ((result nil)
		 (url-request-method "GET"))
	 (save-window-excursion
	  (save-excursion
	   (set-buffer (url-retrieve-synchronously (format yahoo-weather-url yahoo-weather-location yahoo-weather-format)))
	   (goto-char (point-min))
	   (unwind-protect
		(setq result (xml-parse-fragment))
		(kill-buffer (current-buffer)))))
	 result))

;;* xmlparse
(defun yahoo-weather-parse ()
  "Return a much better data structure from yahoo.

Right now the forcast is just the first yweather:forecast element.
This will change (and not suck) in the future."
  (let* ((rss-data (fourth (cadr (yahoo-weather-get))))
		 (item-data (assoc 'item rss-data)))
	(list
	 (assoc 'yweather:location rss-data)
	 (assoc 'yweather:wind rss-data)
	 (assoc 'yweather:atmosphere rss-data)
	 (assoc 'yweather:condition item-data)
	 (cons 'yweather:forecast1
		   (cdr (assoc 'yweather:forecast item-data)))
	 (cons 'yweather:forecast2
		   (cdr  (assoc 'yweather:forecast
						(cdr (member (assoc 'yweather:forcast item-data)
									 item-data))))))))

;;* var retrive xmlparse cache
(defvar yahoo-weather-retrieval-cache nil
  "stores the las ttime the weather was retrieved, and the result of that retrival.
This is used so that we don't call the webservice all the time and we can provide
the most recent data if we cannot connect.")

;;* retrieve xmlparse cache
(defun yahoo-weather-retrieve ()
  "Get the most recent weather results if we haven't retrieved it in the last hour.

If the cache is null, or old, this function retrieves the weather, and if successful
stores it in `yahoo-weather-retrieval-cache' and returns it."
  (when (or (null yahoo-weather-retrieval-cache)
			(> (+ (float-time) yahoo-weather-stale-cache-time)
			   (car yahoo-weather-retrieval-cache)))
		(setq yahoo-weather-retrieval-cache (cons (float-time) (yahoo-weather-parse))))
  (cdr yahoo-weather-retrieval-cache))

;;* retrieve cache interactive
(defun yahoo-weather-clear-cache ()
  "Sometimes the cache is bunk, so lets clear it."
  (interactive)
  (setq yahoo-weather-retrieval-cache 'nil))

;;* helper
(defun yahoo-weather-get-conditions ()
  "Retrieve the conditions as a formatted scring."
  (let ((weather (yahoo-weather-retrieve)))
	(when (null weather)
		  (error "Yahoo Weather Retrieval Error."))
	(format "Current Conditions for %s: %s  %s degrees %s"
			(cdr (assoc 'city (cadr (assoc 'yweather:location weather))))
			(cdr (assoc 'text (cadr (assoc 'yweather:condition weather))))
			(cdr (assoc 'temp (cadr (assoc 'yweather:condition weather))))
			yahoo-weather-format)))

;;* jiseki
(defvar yahoo-weather-jiseki-source 
  '((name . "Yahoo Weather")
	(volatile)
	(display . yahoo-weather-get-conditions))
  "A jiseki source to show you teh current weather conditions.")

;;* interactive
(defun yahoo-weather-conditions ()
  "Show the current conditions.  Also, an example of how to use yahoo-weather.el."
  (interactive)
  (message (yahoo-weather-get-conditions)))

(provide 'yahoo-weather)

