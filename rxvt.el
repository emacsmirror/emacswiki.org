;;; rxvt.el --- define function key sequences for rxvt

;; Author: Dean Scarff <dos at scarff.id.au>
;; Keywords: terminals

;; Copyright (C) 2004 Dean Scarff

;; This program is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as 
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:
;; See: Mark Olesen, The Rxvt Technical Reference, 1997

;; Most distributions use "xterm" as rxvt's TERM environment variable.
;; While the wisdom of this is debatable (and there sure have been a
;; lot of work-arounds for the incompatiblities) it may mean you have
;; to change this behaviour or override the xterm terminal setup 
;; (which may break xterm).

;; Some distributions fix this by frobnicating terminal behaviour with
;; termcap/terminfo and hacks to rxvt itself.

;; See Debian policy's "Keyboard configuration" section for how keys 
;; *should* work.

;;;; Usage:
;; Place as term/rxvt.el somewhere in your load-path.  You may wish to
;; modify term/xterm.el to require rxvt.el.  You may also wish to
;; byte-compile-file this.

;;; Code:

;; Fix backspace and delete to work as Debian Policy says they should
(define-key function-key-map "\C-H" [backspace])
(define-key function-key-map "\C-?" [delete])
; stty handles delete via erase
;(define-key function-key-map "\e[3~" [delete])
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\e\C-h" 'backward-kill-word)
(define-key function-key-map [backspace] [?\C-h])
(define-key function-key-map [M-backspace] [?\M-\C-?])

(define-key function-key-map "\e[7~" [home])
(define-key function-key-map "\e[8~" [end])
(define-key function-key-map "\e[1~" [find])
(define-key function-key-map "\e[2~" [insert])
(define-key function-key-map "\e[4~" [select])
(define-key function-key-map "\e[5~" [prior])
(define-key function-key-map "\e[6~" [next])
(define-key function-key-map "\e[11~" [f1])
(define-key function-key-map "\e[12~" [f2])
(define-key function-key-map "\e[13~" [f3])
(define-key function-key-map "\e[14~" [f4])
(define-key function-key-map "\e[15~" [f5])
(define-key function-key-map "\e[17~" [f6])
(define-key function-key-map "\e[18~" [f7])
(define-key function-key-map "\e[19~" [f8])
(define-key function-key-map "\e[20~" [f9])
(define-key function-key-map "\e[21~" [f10])
(define-key function-key-map "\e[23~" [f11])
(define-key function-key-map "\e[24~" [f12])
(define-key function-key-map "\e[25~" [f13])
(define-key function-key-map "\e[26~" [f14])
(define-key function-key-map "\e[28~" [help])
(define-key function-key-map "\e[29~" [menu])
(define-key function-key-map "\e[31~" [f17])
(define-key function-key-map "\e[32~" [f18])
(define-key function-key-map "\e[33~" [f19])
(define-key function-key-map "\e[34~" [f20])

; Arrows *should* be defined by terminfo
(define-key function-key-map "\e[A" [up])
(define-key function-key-map "\e[B" [down])
(define-key function-key-map "\e[C" [right])
(define-key function-key-map "\e[D" [left])
(define-key function-key-map "\eOa" [(control up)])
(define-key function-key-map "\eOb" [(control down)])
(define-key function-key-map "\eOc" [(control left)])
(define-key function-key-map "\eOd" [(control right)])
(define-key function-key-map "\eOA" [up])
(define-key function-key-map "\eOB" [down])
(define-key function-key-map "\eOC" [left])
(define-key function-key-map "\eOD" [right])

(define-key function-key-map "^M"   [kp-enter])
(define-key function-key-map "\eOM" [kp-enter])

(define-key function-key-map "\eOj" [kp-multiply])
(define-key function-key-map "\eOk" [kp-add])
(define-key function-key-map "\eOl" [kp-separator])
(define-key function-key-map "\eOm" [kp-subtract])
(define-key function-key-map "\eOn" [kp-decimal])
(define-key function-key-map "\eOo" [kp-divide])
(define-key function-key-map "\eOp" [kp-0])
(define-key function-key-map "\eOq" [kp-1])
(define-key function-key-map "\eOr" [kp-2])
(define-key function-key-map "\eOs" [kp-3])
(define-key function-key-map "\eOt" [kp-4])
(define-key function-key-map "\eOu" [kp-5])
(define-key function-key-map "\eOv" [kp-6])
(define-key function-key-map "\eOw" [kp-7])
(define-key function-key-map "\eOx" [kp-8])
(define-key function-key-map "\eOy" [kp-9])

;;; rxvt.el deliberately doesn't provide a feature
;;; rxvt.el ends here
