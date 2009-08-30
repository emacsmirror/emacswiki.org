;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; suomalainen-kalenteri.el (2009-07-10)
;;
;; Suomalaiset merkkipäivät Emacsin         Finnish holidays for Emacs calendar.
;; kalenteriin.

;; Tekijä/Author: Teemu Likonen <tlikonen@iki.fi>
;; 
;; Tämä ohjelma on yleistä                  This program is placed in the public
;; omaisuutta.                              domain.

;; Asennus                                  Installation
;;
;; Lisää Emacsin käynnistys-                For example, add the following
;; tiedostoon esimerkiksi                   command to your Emacs initialization
;; seuraavanlainen komento:                 file:
;;
;;     (eval-after-load 'calendar
;;       '(load "~/path/suomalainen-kalenteri.el"))


(eval-when-compile
  (require 'calendar)
  (require 'holidays))


;; Yleiset suomalaisen kalenterin merkkipäivät

(defvar holiday-finnish-holidays nil
  "Finnish national and Christian holidays.
See variable `calendar-holidays' for information about the
format.")

(setq
 holiday-finnish-holidays
 `((holiday-fixed 1 1 "Uudenvuodenpäivä")
   (holiday-fixed 1 6 "Loppiainen")
   (holiday-fixed 1 27 "Vainojen uhrien muistopäivä")
   (holiday-sexp '(let ((day (calendar-nth-named-day 1 0 2 year 2))
                        (easter-49 (caar (holiday-easter-etc -49)))
                        (easter-56 (caar (holiday-easter-etc -56))))
                    (if (equal day easter-49)
                        easter-56
                      day))
                 "Kynttilänpäivä")
   (holiday-fixed 2 5 "J. L. Runebergin päivä (liputus)")
   (holiday-fixed 2 6 "Saamelaisten kansallispäivä")
   (holiday-easter-etc -49 "Laskiaissunnuntai")
   (holiday-easter-etc -47 "Laskiaistiistai")
   (holiday-fixed 2 14 "Ystävänpäivä")
   (holiday-fixed 2 28 "Kalevalan päivä, suomalaisen kulttuurin päivä (liputus)")
   (holiday-fixed 2 29 "Karkauspäivä")
   (holiday-fixed 3 8 "Kansainvälinen naistenpäivä")
   (holiday-fixed 3 19 "Minna Canthin päivä, tasa-arvon päivä (liputus)")
   (holiday-sexp '(let ((day (calendar-nth-named-day 1 0 3 year 22))
                        (easter-0 (caar (holiday-easter-etc 0)))
                        (easter-7 (caar (holiday-easter-etc -7)))
                        (easter-14 (caar (holiday-easter-etc -14))))
                    (if (or (equal day easter-0)
                            (equal day easter-7))
                        easter-14
                      day))
                 "Marian ilmestyspäivä")
   (holiday-easter-etc -7 "Palmusunnuntai")
   (holiday-easter-etc -2 "Pitkäperjantai")
   (holiday-easter-etc 0 "Pääsiäispäivä")
   (holiday-easter-etc 1 "2. pääsiäispäivä")
   (holiday-fixed 4 9 "Mikael Agricolan päivä, suomen kielen päivä (liputus)")
   (holiday-fixed 4 27 "Kansallinen veteraanipäivä (liputus)")
   (holiday-fixed 5 1 "Vappu, suomalaisen työn päivä (liputus)")
   (holiday-fixed 5 9 "Eurooppa-päivä")
   (holiday-fixed 5 12 "J. V. Snellmanin päivä (liputus)")
   (holiday-easter-etc 39 "Helatorstai")
   (holiday-easter-etc 49 "Helluntaipäivä")
   (holiday-float 5 0 2 "Äitienpäivä (liputus)")
   (holiday-float 5 0 3 "Kaatuneitten muistopäivä (liputus, puolitangossa)")
   (holiday-fixed 6 4 "Puolustusvoimain lippujuhlan päivä (liputus)")
   (holiday-fixed 6 5 "Maailman ympäristöpäivä")
   (holiday-float 6 6 1 "Juhannuspäivä, Suomen lipun päivä (liputus)" 20)
   (holiday-fixed 7 6 "Eino Leinon päivä, runon ja suven päivä (liputus)")
   (holiday-fixed 7 27 "Unikeonpäivä")
   (holiday-float 9 0 1 "Mikkelinpäivä" 29)
   (holiday-fixed 10 10 (concat "Aleksis Kiven päivä, suomalaisen "
                                "kirjallisuuden päivä (liputus)"))
   (holiday-fixed 10 24 "YK:n päivä (liputus)")
   (holiday-float 10 6 1 "Pyhäinpäivä" 31)
   (holiday-fixed 11 6 "Ruotsalaisuuden päivä, svenska dagen (liputus)")
   (holiday-float 11 0 2 "Isänpäivä (liputus)")
   (holiday-fixed 11 20 "Lapsen oikeuksien päivä")
   (holiday-fixed 12 6 "Itsenäisyyspäivä (liputus)")
   (holiday-float 11 0 1 "1. adventtisunnuntai" 27)
   (holiday-float 12 0 1 "2. adventtisunnuntai" 4)
   (holiday-float 12 0 1 "3. adventtisunnuntai" 11)
   (holiday-float 12 0 1 "4. adventtisunnuntai" 18)
   (holiday-fixed 12 25 "Joulupäivä")
   (holiday-fixed 12 26 "Tapaninpäivä")
   (holiday-fixed 12 28 "Viattomien lasten päivä")
   (holiday-float 3 0 -1 "Kesäaika alkaa (klo 03.00, UTC+2 -> UTC+3)")
   (holiday-float 10 0 -1 "Kesäaika päättyy (klo 04.00, UTC+3 -> UTC+2)")
   ,(if (fboundp 'atan) '(solar-equinoxes-solstices))))


;; Muita merkkipäiviä
(setq holiday-finnish-holidays
      (append holiday-finnish-holidays
              '((holiday-fixed 3 30 (concat "Ahvenanmaan demilitarisoinnin "
                                            "ja neutralisoinnin juhlapäivä"))
                (holiday-fixed 4 1 "Aprillipäivä")
                (holiday-fixed 4 7 "Maailman terveyspäivä")
                (holiday-float 4 0 -1 "Ahvenanmaan lipun päivä")
                (holiday-fixed 5 3 "Kansainvälinen lehdistönvapauden päivä")
                (holiday-fixed 6 9 "Ahvenanmaan itsehallintopäivä")
                (holiday-fixed 7 14 "Kansanvallanpäivä")
                (holiday-fixed 9 22 "Kansainvälinen autoton päivä")
                (holiday-fixed 10 16 "Maailman ravintopäivä")
                (holiday-fixed 10 31 "Halloween")
                (holiday-float 11 5 -1 "Älä osta mitään -päivä")
                (holiday-fixed 12 10 "Ihmisoikeuksien päivä")
                (holiday-fixed 12 13 "Lucian päivä"))))


(setq solar-n-hemi-seasons '("Kevätpäiväntasaus" "Kesäpäivänseisaus"
                             "Syyspäiväntasaus" "Talvipäivänseisaus")
      solar-s-hemi-seasons '("Syyspäiväntasaus" "Talvipäivänseisaus"
                             "Kevätpäiväntasaus" "Kesäpäivänseisaus"))


(setq calendar-holidays holiday-finnish-holidays)

(provide 'suomalainen-kalenteri)
