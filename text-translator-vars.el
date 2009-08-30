;;; text-translator-vars.el --- Text Translator

;; Copyright (C) 2007-2009  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Variables for text-translator

;;; Code:

(defconst text-translator-version "0.6.6.2"
  "version numbers of this version of text-translator")

(defconst text-translator-buffer "*translated*"
  "Buffer name that displays translation result.")

(defconst text-translator-mode-name "Translator"
  "Major mode name for displaying to mode line.")

(defconst text-translator-work-buffer (concat " " text-translator-buffer)
  "Output Buffer name from translation site.")

(defgroup text-translator nil
  "Text Translator"
  :tag "Text Translator"
  :group 'text-translator)

(defcustom text-translator-prefix-key "\C-c"
  "*Prefix key for text-translator commands."
  :tag "Prefix Key of text-translator"
  :type '(string :size 10)
  :group 'text-translator)

(defcustom text-translator-auto-window-adjust t
  "*Whether or not you adjust height of window displayed by dividing."
  :type  'boolean
  :group 'text-translator)

(defcustom text-translator-window-min-height 4
  "*Specify minimum height of the translation result display buffer."
  :type  'integer
  :group 'text-translator)

(defcustom text-translator-leave-string nil
  "*Whether or not you leave the character string before the translating."
  :type  'boolean
  :group 'text-translator)

(defcustom text-translator-pre-string-replace-alist
  '(("+" . "＋") ("&#8211;" . "-")  ("&#8226;" . "・"))
  "*Rule that converts character string that wants to translate."
  :type  '(repeat
           (cons :tag "Rule"
                 (string :tag "Letter before the converting.")
                 (string :tag "Letter after the converting.")))
  :group 'text-translator)

(defcustom text-translator-post-string-replace-alist
  '(("\r" . "") ("&#39;" . "'") ("&quot;" . "\"")
    ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&#8211;" . "-")
    ("&#264;" . "Ĉ") ("&#265;" . "ĉ") ("&#284;" . "Ĝ") ("&#285;" . "ĝ")
    ("&#292;" . "Ĥ") ("&#293;" . "ĥ") ("&#308;" . "Ĵ") ("&#309;" . "ĵ")
    ("&#348;" . "Ŝ") ("&#349;" . "ŝ") ("&#364;" . "Ŭ") ("&#365;" . "ŭ"))
  "*Rule that converts character string after the translation."
  :type  '(repeat
           (cons :tag "Rule"
                 (string :tag "Letter before the converting.")
                 (string :tag "Letter after the converting.")))
  :group 'text-translator)

(defcustom text-translator-proxy-server
  (let ((proxy (or (getenv "HTTP_PROXY") "")))
    (and (string-match "^\\(http://\\)?\\(.+\\):\\([0-9]+\\)" proxy)
         (match-string 2 proxy)))
  "*Proxy server used."
  :type  '(choice (string :tag "specify proxy")
                  (const :tag "not use proxy" nil))
  :group 'text-translator)

(defcustom text-translator-proxy-port
  (let ((proxy (or (getenv "HTTP_PROXY") "")))
    (or (and (string-match "^\\(http://\\)?\\(.+\\):\\([0-9]+\\)" proxy)
             (string-to-number (match-string 3 proxy)))
        8080))
  "*Proxy port number used."
  :type  'integer
  :group 'text-translator)

(defcustom text-translator-proxy-user nil
  "*Basic proxy authorization user name."
  :type  '(choice (string :tag "Basic proxy authorization user name")
                  (const :tag "Not use Basic proxy authorization" nil))
  :group 'text-translator)

(defcustom text-translator-proxy-password nil
  "*Basic proxy authorization password."
  :type  '(choice (string :tag "Basic proxy authorization password")
                  (const :tag "Not use Basic proxy authorization" nil))
  :group 'text-translator)

(defcustom text-translator-site-data-alist
  '(;; google.com (English, Japanese)
    ("google.com_enja"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|ja&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_jaen"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=ja|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English, Spanish)
    ("google.com_enes"
     "translate.google.com" "/translate_t HTTP/1.0"
     "langpair=en|es&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_esen"
     "translate.google.com" "/translate_t HTTP/1.0"
     "langpair=es|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English, French)
    ("google.com_enfr"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|fr&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_fren"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=fr|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English, German)
    ("google.com_ende"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|de&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_deen"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=de|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English, Italian)
    ("google.com_enit"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|it&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_iten"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=it|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English, Arabia)
    ("google.com_enar"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|ar&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_aren"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=ar|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (German, French)
    ("google.com_defr"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=de|fr&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_frde"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=fr|de&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English, Portuguese)
    ("google.com_enpt"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|pt&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_pten"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=pt|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English, Russian)
    ("google.com_enru"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|ru&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_ruen"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=ru|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English, Korean)
    ("google.com_enko"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|ko&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_koen"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=ko|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English => Chinese(ch))
    ("google.com_ench"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|zh-CN&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (English => Chinese(tw))
    ("google.com_entw"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=en|zh-TW&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (Chinese => English)
    ("google.com_chen"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=zh|en&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))

    ;; google.com (Chinese(ch), Chinese(tw))
    ("google.com_chtw"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=zh-CN|zh-TW&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))
    ("google.com_twch"
     "translate.google.com"
     "/translate_t HTTP/1.0"
     "langpair=zh-TW|zh-CN&ie=utf-8&oe=utf-8&text=%s"
     utf-8-dos
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<div id=result_box dir=\"ltr\">\\(.*\\)</div></td></tr>")))


    ;; yahoo.com (Japanese, English)
    ("yahoo.com_enja"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_ja&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_jaen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=ja_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Dutch)
    ("yahoo.com_ennl"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_nl&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_nlen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=nl_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, French)
    ("yahoo.com_enfr"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_fr&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_fren"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, German)
    ("yahoo.com_ende"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_de&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_deen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=de_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Greek)
    ("yahoo.com_enel"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_el&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_elen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=el_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Italian)
    ("yahoo.com_enit"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_it&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_iten"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=it_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Korean)
    ("yahoo.com_enko"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_ko&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_koen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=ko_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Portuguese)
    ("yahoo.com_enpt"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_pt&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_pten"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=pt_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Russian)
    ("yahoo.com_enru"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_ru&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_ruen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=ru_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Spanish)
    ("yahoo.com_enes"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_es&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_esen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=es_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (Dutch, French)
    ("yahoo.com_nlfr"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=nl_fr&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_frnl"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_nl&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (French, German)
    ("yahoo.com_frde"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_de&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_defr"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
    "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=de_fr&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo (French, Greek)
    ("yahoo.com_frel"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_el&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_elfr"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=el_fr&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo (French, Italian)
    ("yahoo.com_frit"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_it&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_itfr"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=it_fr&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (French, Portuguese)
    ("yahoo.com_frpt"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_pt&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_ptfr"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=pt_fr&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (French, Spanish)
    ("yahoo.com_fres"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_es&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_esfr"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=es_fr&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Chinese(ch))
    ("yahoo.com_ench"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_zh&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_chen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=zh_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")

    ;; yahoo.com (English, Chinese(tw))
    ("yahoo.com_entw"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_zt&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")
    ("yahoo.com_twen"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=zt_en&btnTrTxt=Translate"
     utf-8
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>")


    ;; freetranslation.com (English, Spanish)
    ("freetranslation.com_enes"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Spanish&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")
    ("freetranslation.com_esen"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Spanish/English&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, French)
    ("freetranslation.com_enfr"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/French&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")
    ("freetranslation.com_fren"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=French/English&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, German)
    ("freetranslation.com_ende"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/German&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")
    ("freetranslation.com_deen"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=German/English&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, Italian)
    ("freetranslation.com_enit"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Italian&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")
    ("freetranslation.com_iten"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Italian/English&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, Dutch)
    ("freetranslation.com_ennl"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Dutch&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")
    ("freetranslation.com_nlen"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Dutch/English&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, Portuguese)
    ("freetranslation.com_enpt"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Portuguese&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")
    ("freetranslation.com_pten"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Portuguese/English&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, Russian)
    ("freetranslation.com_enru"
     "ets6.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Russian&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")
    ("freetranslation.com_ruen"
     "ets6.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Russian/English&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, Chinese(ch))
    ("freetranslation.com_ench"
     "ets6.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/SimplifiedChinese&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, Chinese(tw))
    ("freetranslation.com_entw"
     "ets6.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/TraditionalChinese&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, Norwegian)
    ("freetranslation.com_enno"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Norwegian&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")

    ;; freetranslation.com (English, Japanese)
    ("freetranslation.com_enja"
     "tets9.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Japanese&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")
    ("freetranslation.com_jaen"
     "tets9.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Japanese/English&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>")


    ;; livedoor.com (Japanese, English)
    ("livedoor.com_enja"
     "translate.livedoor.com"
     "/ HTTP/1.1"
     "clear_flg=1&src_text=%s&trns_type=1,2&sumit=翻訳"
     utf-8
     "<textarea name=\"tar_text\" cols=\"40\" rows=\"10\" wrap=\"physical\">\\([^<]*\\)</textarea>")
    ("livedoor.com_jaen"
     "translate.livedoor.com"
     "/ HTTP/1.1"
     "clear_flg=1&src_text=%s&trns_type=2,1&sumit=翻訳"
     utf-8
     "<textarea name=\"tar_text\" cols=\"40\" rows=\"10\" wrap=\"physical\">\\([^<]*\\)</textarea>")

    ;; livedoor.com (Japanese, Korean)
    ("livedoor.com_jako"
     "translate.livedoor.com"
     "/korean/ HTTP/1.1"
     "clear_flg=1&src_text=%s&trns_type=2,9&sumit=翻訳"
     utf-8
     "<textarea name=\"tar_text\" cols=\"40\" rows=\"10\" wrap=\"physical\">\\([^<]*\\)</textarea>")
    ("livedoor.com_koja"
     "translate.livedoor.com"
     "/korean/ HTTP/1.1"
     "clear_flg=1&src_text=%s&trns_type=9,2&sumit=翻訳"
     utf-8
     "<textarea name=\"tar_text\" cols=\"40\" rows=\"10\" wrap=\"physical\">\\([^<]*\\)</textarea>")

    ;; livedoor.com (Japanese, Chinese)
    ("livedoor.com_jach"
     "translate.livedoor.com"
     "/chinese/ HTTP/1.1"
     "clear_flg=1&src_text=%s&trns_type=2,6&sumit=翻訳"
     utf-8
     "<textarea name=\"tar_text\" cols=\"40\" rows=\"10\" wrap=\"physical\">\\([^<]*\\)</textarea>")
    ("livedoor.com_chja"
     "translate.livedoor.com"
     "/chinese/ HTTP/1.1"
     "clear_flg=1&src_text=%s&trns_type=6,2&sumit=翻訳"
     utf-8
     "<textarea name=\"tar_text\" cols=\"40\" rows=\"10\" wrap=\"physical\">\\([^<]*\\)</textarea>")


    ;; fresheye.com (Japanese, English)
    ("fresheye.com_enja"
     "mt.fresheye.com"
     "/ft_result.cgi HTTP/1.1"
     "gen_text=%s"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>")
    ("fresheye.com_jaen"
     "mt.fresheye.com"
     "/ft_result.cgi HTTP/1.1"
     "gen_text=%s&e=JE"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>")

    ;; fresheye.com (Japanese, Chinese(ch))
    ("fresheye.com_jach"
     "mt.fresheye.com"
     "/ft_cjresult.cgi HTTP/1.1"
     "gen_text=%s&charset=gb2312&cjjc=jc"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>")
    ("fresheye.com_chja"
     "mt.fresheye.com"
     "/ft_cjresult.cgi HTTP/1.1"
     "gen_text=%s&charset=gb2312&cjjc=cj"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>")

    ;; fresheye.com (Japanese, Chinese(tw))
    ("fresheye.com_jatw"
     "mt.fresheye.com"
     "/ft_cjresult.cgi HTTP/1.1"
     "gen_text=%s&charset=big5&cjjc=jc"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>")
    ("fresheye.com_twja"
     "mt.fresheye.com"
     "/ft_cjresult.cgi HTTP/1.1"
     "gen_text=%s&charset=big5&cjjc=cj"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>")

    ;; fresheye.com (English, Chinese(ch))
    ("fresheye.com_ench"
     "mt.fresheye.com"
     "/ft_ceresult.cgi HTTP/1.1"
     "gen_text=%s&charset=gb2312&ceec=ec"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>")

    ;; fresheye.com (English, Chinese(tw))
    ("fresheye.com_entw"
     "mt.fresheye.com"
     "/ft_ceresult.cgi HTTP/1.1"
     "gen_text=%s&charset=big5&ceec=ec"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>")


    ;; excite.co.jp (English, Japanese)
    ("excite.co.jp_enja"
     "www.excite.co.jp"
     "/world/english/ HTTP/1.0"
     "wb_lp=ENJA&before=%s"
     japanese-shift-jis-unix
     "<textarea cols=36 rows=15 name=\"after\" id=\"after\" wrap=\"virtual\" style=\"width:320px;height:270px;\" onselect=\"insertDictionaryKeyword(this);\">\\([^<]*\\)</textarea>")
    ("excite.co.jp_jaen"
     "www.excite.co.jp"
     "/world/english/ HTTP/1.0"
     "wb_lp=JAEN&before=%s"
     japanese-shift-jis-unix
     "<textarea cols=36 rows=15 name=\"after\" id=\"after\" wrap=\"virtual\" style=\"width:320px;height:270px;\" onselect=\"insertDictionaryKeyword(this);\">\\([^<]*\\)</textarea>")

    ;; excite.co.jp (English, Chinese)
    ("excite.co.jp_jach"
     "www.excite.co.jp"
     "/world/chinese/ HTTP/1.0"
     "wb_lp=JACH&before=%s"
     utf-8
     "<textarea cols=36 rows=15 name=\"after\" id=\"after\" wrap=\"virtual\" style=\"width:320px;height:270px;.*\">\\([^<]*\\)</textarea>")
    ("excite.co.jp_chja"
     "www.excite.co.jp"
     "/world/chinese/ HTTP/1.0"
     "wb_lp=CHJA&before=%s"
     utf-8
     "<textarea cols=36 rows=15 name=\"after\" id=\"after\" wrap=\"virtual\" style=\"width:320px;height:270px;.*\">\\([^<]*\\)</textarea>")

    ;; excite.co.jp (English, Chinese(tw))
    ("excite.co.jp_jatw"
     "www.excite.co.jp"
     "/world/chinese/ HTTP/1.0"
     "wb_lp=JACH&big5=yes&before=%s"
     utf-8
     "<textarea cols=36 rows=15 name=\"after\" id=\"after\" wrap=\"virtual\" style=\"width:320px;height:270px;.*\">\\([^<]*\\)</textarea>")
    ("excite.co.jp_twja"
     "www.excite.co.jp"
     "/world/chinese/ HTTP/1.0"
     "wb_lp=CHJA&big5=yes&before=%s"
     utf-8
     "<textarea cols=36 rows=15 name=\"after\" id=\"after\" wrap=\"virtual\" style=\"width:320px;height:270px;.*\">\\([^<]*\\)</textarea>")

    ;; excite.co.jp (English, Korean)
    ("excite.co.jp_jako"
     "www.excite.co.jp"
     "/world/korean/ HTTP/1.0"
     "wb_lp=JAKO&before=%s"
     utf-8
     "<textarea cols=36 rows=15 name=\"after\" id=\"after\" wrap=\"virtual\" style=\"width:320px;height:270px;.*\">\\([^<]*\\)</textarea>")
    ("excite.co.jp_koja"
     "www.excite.co.jp"
     "/world/korean/ HTTP/1.0"
     "wb_lp=KOJA&before=%s"
     utf-8
     "<textarea cols=36 rows=15 name=\"after\" id=\"after\" wrap=\"virtual\" style=\"width:320px;height:270px;.*\">\\([^<]*\\)</textarea>")


    ;; yahoo.co.jp (Japanese, English)
    ("yahoo.co.jp_enja"
     "honyaku.yahoo.co.jp"
     "/transtext HTTP/1.1"
     "both=TH&text=%s&clearFlg=1&eid=CR-EJ"
     utf-8
     "<textarea rows=12 cols=30 name=\"trn_text\" id=\"trn_textText\" class=\"smaller\">\\([^<]*\\)</textarea>")
    ("yahoo.co.jp_jaen"
     "honyaku.yahoo.co.jp"
     "/transtext HTTP/1.1"
     "both=TH&text=%s&clearFlg=1&eid=CR-JE"
     utf-8
     "<textarea rows=12 cols=30 name=\"trn_text\" id=\"trn_textText\" class=\"smaller\">\\([^<]*\\)</textarea>")

    ;; yahoo.co.jp (Chinese, Japanese)
    ("yahoo.co.jp_chja"
     "honyaku.yahoo.co.jp"
     "/transtext HTTP/1.1"
     "both=TH&text=%s&clearFlg=1&eid=CR-CJ"
     utf-8
     "<textarea rows=12 cols=30 name=\"trn_text\" id=\"trn_textText\" class=\"smaller\">\\([^<]*\\)</textarea>")
    ("yahoo.co.jp_jach"
     "honyaku.yahoo.co.jp"
     "/transtext HTTP/1.1"
     "both=TH&text=%s&clearFlg=1&eid=CR-JC-CN"
     utf-8
     "<textarea rows=12 cols=30 name=\"trn_text\" id=\"trn_textText\" class=\"smaller\">\\([^<]*\\)</textarea>")

    ;; yahoo.co.jp (Japanese, Korean)
    ("yahoo.co.jp_koja"
     "honyaku.yahoo.co.jp"
     "/transtext HTTP/1.1"
     "both=TH&text=%s&clearFlg=1&eid=CR-KJ"
     utf-8
     "<textarea rows=12 cols=30 name=\"trn_text\" id=\"trn_textText\" class=\"smaller\">\\([^<]*\\)</textarea>")
    ("yahoo.co.jp_jako"
     "honyaku.yahoo.co.jp"
     "/transtext HTTP/1.1"
     "both=TH&text=%s&clearFlg=1&eid=CR-JK"
     utf-8
     "<textarea rows=12 cols=30 name=\"trn_text\" id=\"trn_textText\" class=\"smaller\">\\([^<]*\\)</textarea>")


    ;; ocn.ne.jp (English, Japanese)
    ("ocn.ne.jp_enja"
     "cgi01.ocn.ne.jp"
     "/cgi-bin/translation/index.cgi HTTP/1.1"
     "langpair=enja&sourceText=%s"
     utf-8
     "<TEXTAREA NAME=\"responseText\" ROWS=\"5\" COLS=\"41\" WRAP=\"virtual\" CLASS=\"in2\">\\([^<]*\\)</TEXTAREA>")
    ("ocn.ne.jp_jaen"
     "cgi01.ocn.ne.jp"
     "/cgi-bin/translation/index.cgi HTTP/1.1"
     "langpair=jaen&sourceText=%s"
     utf-8
     "<TEXTAREA NAME=\"responseText\" ROWS=\"5\" COLS=\"41\" WRAP=\"virtual\" CLASS=\"in2\">\\([^<]*\\)</TEXTAREA>")

    ;; ocn.ne.jp (English, Korean)
    ("ocn.ne.jp_jako"
     "cgi01.ocn.ne.jp"
     "/cgi-bin/translation/index.cgi HTTP/1.1"
     "langpair=jako&sourceText=%s"
     utf-8
     "<TEXTAREA NAME=\"responseText\" ROWS=\"5\" COLS=\"41\" WRAP=\"virtual\" CLASS=\"in2\">\\([^<]*\\)</TEXTAREA>")
    ("ocn.ne.jp_koja"
     "cgi01.ocn.ne.jp"
     "/cgi-bin/translation/index.cgi HTTP/1.1"
     "langpair=koja&sourceText=%s"
     utf-8
     "<TEXTAREA NAME=\"responseText\" ROWS=\"5\" COLS=\"41\" WRAP=\"virtual\" CLASS=\"in2\">\\([^<]*\\)</TEXTAREA>")

    ;; ocn.ne.jp (Japanese, Chinese)
    ("ocn.ne.jp_jach"
     "cgi01.ocn.ne.jp"
     "/cgi-bin/translation/index.cgi HTTP/1.1"
     "langpair=jazh&sourceText=%s"
     utf-8
     "<TEXTAREA NAME=\"responseText\" ROWS=\"5\" COLS=\"41\" WRAP=\"virtual\" CLASS=\"in2\">\\([^<]*\\)</TEXTAREA">)
    ("ocn.ne.jp_chja"
     "cgi01.ocn.ne.jp"
     "/cgi-bin/translation/index.cgi HTTP/1.1"
     "langpair=zhja&sourceText=%s"
     utf-8
     "<TEXTAREA NAME=\"responseText\" ROWS=\"5\" COLS=\"41\" WRAP=\"virtual\" CLASS=\"in2\">\\([^<]*\\)</TEXTAREA>")


    ;; lou5.jp (Japanese, Lou)
    ("lou5.jp-normal"
     "lou5.jp"
     "/ HTTP/1.1"
     "v=1&text=%s"
     utf-8
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<p class=\"large align-left box\">\\(\\(.\\|\n\\)*?\\)</p>"
        t)))
    ;; lou5.jp (Japanese, Lou Blog)
    ("lou5.jp-blog"
     "lou5.jp"
     "/ HTTP/1.1"
     "v=2&text=%s"
     utf-8
     "<p class=\"large align-left box\">\\(\\(.\\|\n\\)*?\\)</p>")

    ;; traduku.net (Esperanto, English)
    ("traduku.net_eoen"
     "traduku.net"
     "/cgi-bin/traduku HTTP/1.0"
     "eo_en&t=%s"
     utf-8
     "<div lang=\"en\">\\(\\(.\\|\n\\)*?\\)</div>")
    ("traduku.net_eneo"
     "traduku.net"
     "/cgi-bin/traduku HTTP/1.0"
     "en_eo&t=%s"
     utf-8
     "<div id=\"rezulto\">\\(\\(.\\|\n\\)*?\\)</div>"))

  "*The alist where setting of the site which is used for text translation is
described."
  :type  '(repeat
           (list :tag "Web Site"
                 (string :tag "Web site name and translation type")
                 (string :tag "Host name")
                 (string :tag "POST path and HTTP version")
                 (string :tag "POST contents")
                 (symbol :tag "Character code")
                 (choice (string :tag "regexp") (symbol :tag "function"))))
  :group 'text-translator)

(defcustom text-translator-default-engine "google.com_enja"
  "*Translation engine used by default."
  :type  (cons 'radio
               (mapcar
                (lambda (x)
                  (list 'const (car x)))
                text-translator-site-data-alist))
  :group 'text-translator)

(defcustom text-translator-user-agent
  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.4) Gecko/20070515 Firefox/2.0.0.4"
  "*text-translator's User Agent. Default is Firefox."
  :type  'string
  :group 'text-translator)

(defcustom text-translator-mode-hook nil
  "*Hook run at the end of function `text-translator-mode'."
  :type 'hook
  :group 'text-translator)

(defcustom text-translator-auto-selection-func nil
  "*Value is function that select translation engine automatic.
this value is function for `text-translator-translate-by-auto-selection'."
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-do-fill-region nil
  "*Default is nil. if value is non-nil, it deletes
linefeed\\(and CR\\) from pre translation string(\"\\n\" -> \" \",
\"\r\" -> \"\"). and processing to straighten faces with
fill-paragraph after the translation. it is countermeasure
against the translation engines that processes per line."
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-space-division-languages
  '("en" "es" "fr" "de" "it" "pt" "ru" "nl" "el" "no")
  "*List of language that word is delimited by blank."
  :type '(repeat (string :tag "language(2char)"))
  :group 'text-translator)

(defvar text-translator-last-string ""
  "The last time, character string which was thrown to the translation site.")

(defvar text-translator-engine-history nil
  "The history of translation engine which you used.")

(defvar text-translator-search-regexp-or-func nil)

(provide 'text-translator-vars)
;;; text-translator-vars.el ends here

;; Local Variables:
;; Coding: iso-2022-7bit
;; End:

