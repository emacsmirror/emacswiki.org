;;; procomm-mode.el --- Major mode for editing PROCOMM ASPECT scripts

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Edward Robinson <osnibore@gmail.com>
;; Keywords: languages, ASPECT, Procomm, script
;; Maintainer: Edward Robinson <osnibore@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ASPECT -- PROCOMM terminal emulator scripting language. ()

;; This is major-mode for ASPECT Procomm scripts.  You are welcome to
;; try it and submit changes or report bugs.
;;
;; This mode was based in part on cobol-mode.el by Rick Bielawski

;;; Installing:

;; Before you can use procomm-mode, emacs needs to be able to find it.  Place
;; the procomm-mode.el file in a directory on the load-path; typically the
;; .../site-lisp or perhaps .../lisp/progmods directory.  Usually you would
;; also want to byte compile procomm-mode.el but this is not required.  To do
;; this, visit the procomm-mode.el file, type: M-x emacs-lisp-byte-compile <ret>
;; There should be no warnings or errors during byte compilation.
;;
;; There are 4 basic ways to use PROCOMM-MODE on a file.  The first method
;; manually selects procomm-mode as the editing mode.  The other 3 cause emacs
;; to recognize automatically that you want to visit the file using
;; procomm-mode.
;;
;; Pick one:
;; 1. While visiting a file, type: M-x procomm-mode <ret>
;; 2. Put the string -*-procomm-*- in a comment on the first line of the file.
;;    Save the file and close it.  Now any time you open it procomm-mode starts.
;; 3. Create an association between a particular file naming convention and
;;    procomm-mode.  This is done by adding an association to auto-mode-alist.
;; For example:
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.WAS\\'" . procomm-mode)  ;extension of .WAS means procomm-mode
;;          ("\\.WUD\\'" . procomm-mode)  ;extension of .WUD means procomm-mode (dialog)
;;          )
;;        auto-mode-alist))
;; 4. Advise set-auto-mode to look at the buffer contents upon loading.
;;
;; The above all tell emacs that you want to use procomm-mode but you must load
;; procomm-mode before you can use it.  There are 2 methods of telling emacs to
;; load the procomm-mode routines.  The first unconditionally loads procomm-mode
;; definitions immediately.  The second tells emacs to automatically load
;; procomm-mode only when you try to use it.  Add one of the following lines to
;; your .emacs file.
;;
;;(require 'procomm-mode)      ; Unconditional load
;;  OR
;;(autoload 'procomm-mode "procomm-mode" "Major mode for PROCOMM ASPECT scripts." t nil)
;;
;; Please report any bugs!

;;; History:

;; 2010-1-14  EHR Initial version.
;;

;;; Code:

(defvar procomm-mode-hook nil)

(defvar procomm-mode-map
  (let ((procomm-mode-map (make-keymap)))
    (define-key procomm-mode-map "\C-j" 'newline-and-indent)
    procomm-mode-map)
  "Keymap for PROCOMM major mode")

;; ASPECT Primary Commands
(defvar aspect-primary-commands
  '(
    "call"      "CALL"      "case"      "CASE"      "endcase"   "ENDCASE"   "default"    "DEFAULT"
    "else"      "ELSE"      "elseif"    "ELSEIF"    "exitfor"   "EXITFOR"   "exitswitch" "EXITSWITCH"
    "exitwhile" "EXITWHILE" "for"       "FOR"       "endfor"    "ENDFOR"    "func"       "FUNC"
    "endfunc"   "ENDFUNC"   "goto"      "GOTO"      "if"        "IF"        "endif"      "ENDIF"
    "loopfor"   "LOOPFOR"   "loopwhile" "LOOPWHILE" "param"     "PARAM"     "proc"       "PROC"
    "endproc"   "ENDPROC"   "return"    "RETURN"    "switch"    "SWITCH"    "endswitch"  "ENDSWITCH"
    "while"     "WHILE"     "endwhile"  "ENDWHILE"
    )
  "List of ASPECT primary command keywords.
Used to create the `font-lock-keywords' table.")

;; ASPECT Types
(defvar aspect-types
  '(
    "float"    "FLOAT"      "integer"  "INTEGER"   "long"      "LONG"     "string"   "STRING"
    )
  "List of ASPECT type keywords.
Used to create the `font-lock-keywords' table.")

;; Clipboard Commands
(defvar aspect-clipboard-commands
  '(
    "cliptofile" "CLIPTOFILE" "cliptostr" "CLIPTOSTR" "filetoclip" "FILETOCLIP" "pastetext" "PASTETEXT"
    "strtoclip"  "STRTOCLIP"
    )
  "List of ASPECT Clipboard Commands keywords.
Used to create the `font-lock-keywords' table.")

;; COM-Related Commands
(defvar aspect-com-commands
  '(
    "break"     "BREAK"     "clearxoff" "CLEARXOFF" "comgetc"    "COMGETC"    "computc"     "COMPUTC"
    "comread"   "COMREAD"   "comwrite"  "COMWRITE"  "disconnect" "DISCONNECT" "hangup"      "HANGUP"
    "rget"      "RGET"      "rxflush"   "RXFLUSH"   "transmit"   "TRANSMIT"   "txflush"     "TXFLUSH"
    "waitfor"   "WAITFOR"   "waitquiet" "WAITQUIET" "when quiet" "WHEN QUIET" "when target" "WHEN TARGET"
    )
  "List of ASPECT COM command keywords.
Used to create the `font-lock-keywords' table.")

;; Date and Time Commands
(defvar aspect-date-and-time-commands
  '(
    "intsltime"   "INTSLTIME"   "ltimeelapsed" "LTIMEELAPSED" "ltimeints" "LTIMEINTS" "ltimemisc" "LTIMEMISC"
    "ltimestring" "LTIMESTRING" "ltimestrs"    "LTIMESTRS"    "monthstr"  "MONTHSTR"  "strsltime" "STRSLTIME"
    "weekdaystr"  "WEEKDAYSTR"
    )
  "List of ASPECT Date and Time Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Dialing Commands
(defvar aspect-dialing-commands
  '(
    "connect"    "CONNECT"    "connectmanual" "CONNECTMANUAL" "dial"       "DIAL"       "dialadd"    "DIALADD"
    "dialcancel" "DIALCANCEL" "dialclass"     "DIALCLASS"     "dialcount"  "DIALCOUNT"  "dialcreate" "DIALCREATE"
    "dialdelete" "DIALDELETE" "dialfind"      "DIALFIND"      "dialinsert" "DIALINSERT" "dialload"   "DIALLOAD"
    "dialname"   "DIALNAME"   "dialnumber"    "DIALNUMBER"    "dialsave"   "DIALSAVE"   "dialstats"  "DIALSTATS"
    )
  "List of ASPECT Dialing Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Dialog Box Commands
(defvar aspect-dialog-commands
  '(
    "bitmap"      "BITMAP"      "checkbox"   "CHECKBOX"   "combobox"  "COMBOBOX"  "dialogbox"  "DIALOGBOX"
    "enddialog"   "ENDDIALOG"   "dirlistbox" "DIRLISTBOX" "dirpath"   "DIRPATH"   "dlgctrlwin" "DLGCTRLWIN"
    "dlgdestroy"  "DLGDESTROY"  "dlgevent"   "DLGEVENT"   "dlgexists" "DLGEXISTS" "dlglist"    "DLGLIST"
    "dlgsave"     "DLGSAVE"     "dlgshow"    "DLGSHOW"    "dlgupdate" "DLGUPDATE" "dlgwin"     "DLGWIN"
    "dlgwinctrl"  "DLGWINCTRL"  "editbox"    "EDITBOX"    "fcombobox" "FCOMBOBOX" "feditbox"   "FEDITBOX"
    "flistbox"    "FLISTBOX"    "ftext"      "FTEXT"      "groupbox"  "GROUPBOX"  "icon"       "ICON"
    "iconbutton"  "ICONBUTTON"  "listbox"    "LISTBOX"    "metafile"  "METAFILE"  "pushbutton" "PUSHBUTTON"
    "radiobutton" "RADIOBUTTON" "radiogroup" "RADIOGROUP" "endgroup"  "ENDGROUP"  "text"       "TEXT"
    )
  "List of ASPECT dialog command keywords.
Used to create the `font-lock-keywords' table.")

;; DOS- or Disk-Related Commands
(defvar aspect-dos-or-disk-related-commands
  '(
    "addfilename" "ADDFILENAME" "chdir"     "CHDIR"     "copyfile"  "COPYFILE"  "delfile"     "DELFILE"
    "dir"         "DIR"         "diskfree"  "DISKFREE"  "dos"       "DOS"       "fileget"     "FILEGET"
    "fileset"     "FILESET"     "fileview"  "FILEVIEW"  "findfirst" "FINDFIRST" "findnext"    "FINDNEXT"
    "fullpath"    "FULLPATH"    "getdir"    "GETDIR"    "getenv"    "GETENV"    "getfilename" "GETFILENAME"
    "getpathname" "GETPATHNAME" "getvolume" "GETVOLUME" "isfile"    "ISFILE"    "makepath"    "MAKEPATH"
    "mkdir"       "MKDIR"       "putenv"    "PUTENV"    "rename"    "RENAME"    "rmdir"       "RMDIR"
    "run"         "RUN"         "shell"     "SHELL"     "shortpath" "SHORTPATH" "splitpath"   "SPLITPATH"
    )
  "List of ASPECT DOS- or Disk-Related Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Dynamic Data Exchange Commands
(defvar aspect-dynamic-data-exchange-commands
  '(
    "ddeadvise"  "DDEADVISE"  "ddeexecute"   "DDEEXECUTE"   "ddeinit"     "DDEINIT"     "ddepoke" "DDEPOKE"
    "dderequest" "DDEREQUEST" "ddeterminate" "DDETERMINATE" "ddeunadvise" "DDEUNADVISE"
    )
  "List of ASPECT Dynamic Data Exchange Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Fax Commands
(defvar aspect-fax-commands
  '(
    "faxcancel" "FAXCANCEL" "faxlist"   "FAXLIST"   "faxmodem" "FAXMODEM" "faxpoll"   "FAXPOLL"
    "faxprint"  "FAXPRINT"  "faxremove" "FAXREMOVE" "faxsend"  "FAXSEND"  "faxstatus" "FAXSTATUS"
    "faxview"   "FAXVIEW"
    )
  "List of ASPECT Fax Commands keywords.
Used to create the `font-lock-keywords' table.")

;; File I/O Commands
(defvar aspect-file-i-o-commands
  '(
    "fclear"    "FCLEAR"    "fclose"    "FCLOSE"    "fdelblock" "FDELBLOCK" "feof"    "FEOF"
    "ferror"    "FERROR"    "fflush"    "FFLUSH"    "fgetc"     "FGETC"     "fgets"   "FGETS"
    "finsblock" "FINSBLOCK" "flength"   "FLENGTH"   "fopen"     "FOPEN"     "fputc"   "FPUTC"
    "fputs"     "FPUTS"     "fread"     "FREAD"     "fseek"     "FSEEK"     "fstrfmt" "FSTRFMT"
    "ftell"     "FTELL"     "ftruncate" "FTRUNCATE" "fwrite"    "FWRITE"    "rewind"  "REWIND"
    )
  "List of ASPECT File I/O Commands keywords.
Used to create the `font-lock-keywords' table.")

;; File Transfer Commands
(defvar aspect-file-transfer-commands
  '(
    "ftp"        "FTP"        "getfile" "GETFILE" "kermserve" "KERMSERVE" "sendfile" "SENDFILE"
    "xfercancel" "XFERCANCEL"
    )
  "List of ASPECT File Transfer Commands keywords.
Used to create the `font-lock-keywords' table.")

;; General Procomm Plus Commands
(defvar aspect-general-procomm-plus-commands
  '(
    "alarm"      "ALARM"      "beep"       "BEEP"       "capture"   "CAPTURE"   "capturestr" "CAPTURESTR"
    "crc16"      "CRC16"      "decrypt"    "DECRYPT"    "encrypt"   "ENCRYPT"   "errormsg"   "ERRORMSG"
    "fetch"      "FETCH"      "help"       "HELP"       "mapisend"  "MAPISEND"  "metakey"    "METAKEY"
    "mspause"    "MSPAUSE"    "pause"      "PAUSE"      "playback"  "PLAYBACK"  "pwexit"     "PWEXIT"
    "pwmode"     "PWMODE"     "pwtitlebar" "PWTITLEBAR" "sdlgfopen" "SDLGFOPEN" "sdlginput"  "SDLGINPUT"
    "sdlgmsgbox" "SDLGMSGBOX" "sdlgsaveas" "SDLGSAVEAS" "set"       "SET"       "setpointer" "SETPOINTER"
    "setup"      "SETUP"      "statclear"  "STATCLEAR"  "statmsg"   "STATMSG"   "usermsg"    "USERMSG"
    "waituntil"  "WAITUNTIL"  "when"       "WHEN"       "xlatin"    "XLATIN"    "xlatout"    "XLATOUT"
    "xlatstr"    "XLATSTR"    "wizard"     "WIZARD"
    )
  "List of ASPECT General Procomm Plus Commands keywords.
Used to create the `font-lock-keywords' table.")

;; General Windows Commands
(defvar aspect-general-windows-commands
  '(
    "disable"    "DISABLE"    "dllcall"     "DLLCALL"     "dllfree"     "DLLFREE"     "dllload" "DLLLOAD"
    "enable"     "ENABLE"     "exitwindows" "EXITWINDOWS" "mciexec"     "MCIEXEC"     "mcisend" "MCISEND"
    "profilerd"  "PROFILERD"  "profilewr"   "PROFILEWR"   "screentowin" "SCREENTOWIN" "sendkey" "SENDKEY"
    "sendkeystr" "SENDKEYSTR" "sendvkey"    "SENDVKEY"    "wintoscreen" "WINTOSCREEN"
    )
  "List of ASPECT General Windows Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Keyboard Commands
(defvar aspect-keyboard-commands
  '(
    "ansitokey" "ANSITOKEY" "keyflush" "KEYFLUSH" "keyget"   "KEYGET"   "keystate" "KEYSTATE"
    "keytoansi" "KEYTOANSI" "keytooem" "KEYTOOEM" "oemtokey" "OEMTOKEY"
    )
  "List of ASPECT Keyboard Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Internet Commands
(defvar aspect-internet-commands
  '(
    "connect" "CONNECT" "connectmanual" "CONNECTMANUAL" "disconnect" "DISCONNECT" "ftp" "FTP"
    )
  "List of ASPECT Internet Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Memory Commands
(defvar aspect-memory-commands
  '(
    "memaddress" "MEMADDRESS" "memalloc" "MEMALLOC" "memavail" "MEMAVAIL" "memchr"     "MEMCHR"
    "memcmp"     "MEMCMP"     "memfree"  "MEMFREE"  "memgetc"  "MEMGETC"  "memicmp"    "MEMICMP"
    "memmove"    "MEMMOVE"    "memputc"  "MEMPUTC"  "memread"  "MEMREAD"  "memrealloc" "MEMREALLOC"
    "memset"     "MEMSET"     "memsize"  "MEMSIZE"  "memwrite" "MEMWRITE"
    )
  "List of ASPECT Memory Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Menu Commands
(defvar aspect-menu-commands
  '(
    "menubar"       "MENUBAR"       "menucheck"   "MENUCHECK"   "menuitem"   "MENUITEM"   "menuitemcount" "MENUITEMCOUNT"
    "menupopup"     "MENUPOPUP"     "menupopupid" "MENUPOPUPID" "menuselect" "MENUSELECT" "menushow"      "MENUSHOW"
    "menushowpopup" "MENUSHOWPOPUP" "menustate"   "MENUSTATE"
    )
  "List of ASPECT Menu Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Option Set Commands
(defvar aspect-option-set-commands
  '(
    "itemcount"  "ITEMCOUNT" "itemcreate" "ITEMCREATE" "itemfind" "ITEMFIND" "itemname" "ITEMNAME"
    "itemremove" "ITEMREMOVE"
    )
  "List of ASPECT Option Set Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Numeric, String Conversion Commands
(defvar aspect-numeric-string-conversion-commands
  '(
    "atof"     "ATOF"     "atoi" "ATOI" "atol"     "ATOL"    "ceil" "CEIL"
    "floor"    "FLOOR"    "ftoa" "FTOA" "itoa"     "ITOA"    "ltoa" "LTOA"
    "numtostr" "NUMTOSTR" "rand" "RAND" "strtonum" "STRTONUM"
    )
  "List of ASPECT Numeric, String Conversion Commands keywords.
Used to create the `font-lock-keywords' table.")

;; OEM/ANSI Commands
(defvar aspect-oem-ansi-commands
  '(
    "ansitokey" "ANSITOKEY" "ansitooem" "ANSITOOEM" "oemtoansi" "OEMTOANSI" "oemtokey" "OEMTOKEY"
    )
  "List of ASPECT OEM/ANSI Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Packet Mode Commands
(defvar aspect-packet-mode-commands
  '(
    "ansitokey" "ANSITOKEY" "ansitooem" "ANSITOOEM" "oemtoansi" "OEMTOANSI" "oemtokey" "OEMTOKEY"
    )
  "List of ASPECT Packet Mode Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Preprocessor Commands
(defvar aspect-preprocessor-commands
  '(
    "#comment"  "#COMMENT"  "#endcomment" "#ENDCOMMENT" "#define"   "#DEFINE"   "#else"    "#ELSE"
    "#elif"     "#ELIF"     "#elifdef"    "#ELIFDEF"    "#elifndef" "#ELIFNDEF" "#if"      "#IF"
    "#ifdef"    "#IFDEF"    "#endif"      "#ENDIF"      "#ifndef"   "#IFNDEF"   "#include" "#INCLUDE"
    "#undef "   "#UNDEF"
    )
  "List of ASPECT preprocessor keywords.
Used to create the `font-lock-keywords' table.")

;; Printer Commands
(defvar aspect-printer-commands
  '(
    "printalign" "PRINTALIGN" "printattr" "PRINTATTR" "printcapture" "PRINTCAPTURE" "printchar"   "PRINTCHAR"
    "printer"    "PRINTER"    "printfit"  "PRINTFIT"  "printfont"    "PRINTFONT"    "printmargin" "PRINTMARGIN"
    "printstr"   "PRINTSTR"   "printtabs" "PRINTTABS" "printtabstr"  "PRINTTABSTR"
    )
  "List of ASPECT Printer Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Script Control Commands
(defvar aspect-script-control-commands
  '(
    "breakpoint" "BREAKPOINT" "chain" "CHAIN" "compile" "COMPILE" "execute" "EXECUTE"
    "exit"       "EXIT"       "halt"  "HALT"  "longjmp" "LONGJMP" "setjmp"  "SETJMP"
    "yield"      "YIELD"
    )
  "List of ASPECT Script Control Commands keywords.
Used to create the `font-lock-keywords' table.")

;; String Commands
(defvar aspect-string-commands
  '(
    "nullstr"    "NULLSTR"    "rstrcmp"   "RSTRCMP"   "strcat"     "STRCAT"     "strchr"    "STRCHR"
    "strcmp"     "STRCMP"     "strcpy"    "STRCPY"    "strcspn"    "STRCSPN"    "strdelete" "STRDELETE"
    "strextract" "STREXTRACT" "strfind"   "STRFIND"   "strfmt"     "STRFMT"     "strgetc"   "STRGETC"
    "stricmp"    "STRICMP"    "strinsert" "STRINSERT" "strlen"     "STRLEN"     "strlwr"    "STRLWR"
    "strncmp"    "STRNCMP"    "strnicmp"  "STRNICMP"  "strputc"    "STRPUTC"    "strquote"  "STRQUOTE"
    "strrchr"    "STRRCHR"    "strread"   "STRREAD"   "strreplace" "STRREPLACE" "strrev"    "STRREV"
    "strright"   "STRRIGHT"   "strsearch" "STRSEARCH" "strset"     "STRSET"     "strspn"    "STRSPN"
    "strtok"     "STRTOK"     "strupdt"   "STRUPDT"   "strupr"     "STRUPR"     "strwrite"  "STRWRITE"
    "substr"     "SUBSTR"
    )
  "List of ASPECT string keywords.
Used to create the `font-lock-keywords' table.")

;; Task/Window Manipulation Commands
(defvar aspect-task-window-manipulation-commands
  '(
    "firsttask"   "FIRSTTASK"   "nexttask"   "NEXTTASK" "taskactivate" "TASKACTIVATE" "taskexists"  "TASKEXISTS"
    "taskexit"    "TASKEXIT"    "taskname"   "TASKNAME" "taskpath"     "TASKPATH"     "taskwin"     "TASKWIN"
    "winactivate" "WINACTIVATE" "winclose"   "WINCLOSE" "wincoord"     "WINCOORD"     "winenabled"  "WINENABLED"
    "winexists"   "WINEXISTS"   "winfocus"   "WINFOCUS" "winhide"      "WINHIDE"      "winmaximize" "WINMAXIMIZE"
    "winminimize" "WINMINIMIZE" "winmove"    "WINMOVE"  "winowner"     "WINOWNER"     "winrestore"  "WINRESTORE"
    "winshow"     "WINSHOW"     "winsize"    "WINSIZE"  "winstate"     "WINSTATE"     "wintask"     "WINTASK"
    "wintext"     "WINTEXT"     "winvisible" "WINVISIBLE"
    )
  "List of ASPECT Task/Window Manipulation Commands keywords.
Used to create the `font-lock-keywords' table.")

;; Terminal Commands
(defvar aspect-terminal-commands
  '(
    "clear"      "CLEAR"      "commandmode" "COMMANDMODE" "getcur"    "GETCUR"    "locate"   "LOCATE"
    "sbsave"     "SBSAVE"     "snapshot"    "SNAPSHOT"    "termgetc"  "TERMGETC"  "termgets" "TERMGETS"
    "termkey"    "TERMKEY"    "termmsg"     "TERMMSG"     "termputc"  "TERMPUTC"  "termputs" "TERMPUTS"
    "termreadc"  "TERMREADC"  "termreads"   "TERMREADS"   "termreset" "TERMRESET" "termvkey" "TERMVKEY"
    "termwritec" "TERMWRITEC" "termwrites"  "TERMWRITES"
    )
  "List of ASPECT Terminal Commands keywords.
Used to create the `font-lock-keywords' table.")

;; User Window Commands
(defvar aspect-user-window-commands
  '(
    "bitmap"     "BITMAP"     "bitmapbkg"   "BITMAPBKG"   "dllobject"  "DLLOBJECT"  "dllobjfile" "DLLOBJFILE"
    "dllobjupdt" "DLLOBJUPDT" "hotspot"     "HOTSPOT"     "icon"       "ICON"       "iconbutton" "ICONBUTTON"
    "metafile"   "METAFILE"   "metafilebkg" "METAFILEBKG" "objcoord"   "OBJCOORD"   "objhide"    "OBJHIDE"
    "objmove"    "OBJMOVE"    "objpaint"    "OBJPAINT"    "objpointid" "OBJPOINTID" "objremove"  "OBJREMOVE"
    "objshow"    "OBJSHOW"    "pushbutton"  "PUSHBUTTON"  "uwincreate" "UWINCREATE" "uwinpaint"  "UWINPAINT"
    "uwinremove" "UWINREMOVE" "uwutowin"    "UWUTOWIN"    "wintouwu"   "WINTOUWU"
    )
  "List of ASPECT User Window Commands keywords.
Used to create the `font-lock-keywords' table.")



(defun aspect-keyword-anywhere-regexp ( word-list )
  "Returns a regexp that finds any of the words in WORD-LIST.
But only if the keyword is surrounded by non-word chars."
  (concat "\\<"(regexp-opt word-list t)"\\W"))
;;  (regexp-opt word-list 'words))

;; (list-faces-display)
(defvar procomm-font-lock-keywords
  ;; font-lock-keywords is a symbol or list of symbols yielding the keywords to
  ;; be fontified.  Keywords are listed here using either (MATCHER . FACENAME)
  ;; or (MATCHER . (MATCH FACENAME)) syntax.  Other options are available but
  ;; not used here.  For simplicity, all regexp's were designed so MATCH would
  ;; be 1.  Nothing forced this but to me it makes debug/maintenance easier.
  `(
    (,(aspect-keyword-anywhere-regexp aspect-primary-commands)                   . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-types)                              . font-lock-type-face)
    (,(aspect-keyword-anywhere-regexp aspect-clipboard-commands)                 . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-com-commands)                       . font-lock-function-name-face)
    (,(aspect-keyword-anywhere-regexp aspect-date-and-time-commands)             . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-dialing-commands)                   . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-dialog-commands)                    . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-dos-or-disk-related-commands)       . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-dynamic-data-exchange-commands)     . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-fax-commands)                       . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-file-i-o-commands)                  . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-file-transfer-commands)             . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-general-procomm-plus-commands)      . font-lock-function-name-face)
    (,(aspect-keyword-anywhere-regexp aspect-general-windows-commands)           . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-keyboard-commands)                  . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-internet-commands)                  . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-memory-commands)                    . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-menu-commands)                      . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-option-set-commands)                . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-numeric-string-conversion-commands) . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-oem-ansi-commands)                  . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-packet-mode-commands)               . font-lock-keyword-face)
    (,(regexp-opt aspect-preprocessor-commands)                                  . font-lock-builtin-face)
    (,(aspect-keyword-anywhere-regexp aspect-printer-commands)                   . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-script-control-commands)            . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-string-commands)                    . font-lock-function-name-face)
    (,(aspect-keyword-anywhere-regexp aspect-task-window-manipulation-commands)  . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-terminal-commands)                  . font-lock-keyword-face)
    (,(aspect-keyword-anywhere-regexp aspect-user-window-commands)               . font-lock-keyword-face)
    ))



;; Indentation function based on code from http://www.emacswiki.org/emacs/ModeTutorial#toc3

;; Handle empty structures (e.g. IF/ENDIF with nothing in between)
;; IF looking at an end marker
;;    find the next preceding non-blank line
;;    IF looking at a start marker
;;       decrement indent and apply to the current line
;;    ELSE
;;       apply indent to the current line
;;    ENDIF
;; ELSE
;;    WHILE (1)
;;       IF previous line is a start marker
;;          increment indent and apply to the current line
;;          exit
;;       ENDIF
;;       IF previous line is an end marker
;;          decrement indent and apply to the current line
;;          exit
;;       ENDIF
;;       IF previous line is the beginning of the buffer
;;          apply indent to the current line
;;          exit
;;       ENDIF
;;    END WHILE

(defun procomm-indent-line () ; from wpdl-indent-line
  "Indent current line as PROCOMM Aspect script code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)   ;; First line is always non-indented
    (let ((not-indented t) cur-indent (is-case nil))
      (if (looking-at "^[ \t]*case")
          (setq is-case t))
      ;; If the line we are looking at is the end of a block, then decrease the indentation
      (if (or (looking-at "^[ \t]*endproc")
              (looking-at "^[ \t]*endif")
              (looking-at "^[ \t]*else")
              (looking-at "^[ \t]*elseif")
              (looking-at "^[ \t]*endfor")
              (looking-at "^[ \t]*endswitch")
              (looking-at "^[ \t]*endcase")
              (looking-at "^[ \t]*endwhile")
              (looking-at "^[ \t]*enddialog"))
          (progn
            (save-excursion
              (forward-line -1)
              ;; Skip past any preceding blank lines
              (while (looking-at "^$")
                (forward-line -1)
                )
              (if (not (or (looking-at "^[ \t]*proc")
                           (looking-at "^[ \t]*if")
                           (looking-at "^[ \t]*else")
                           (looking-at "^[ \t]*elseif")
                           (looking-at "^[ \t]*for")
                           (looking-at "^[ \t]*switch")
                           (looking-at "^[ \t]*case")
                           (looking-at "^[ \t]*default")
                           (looking-at "^[ \t]*while")
                           (looking-at "^[ \t]*dialogbox")))
                  (setq cur-indent (- (current-indentation) tab-width))
                (setq cur-indent (current-indentation))))
            (if (< cur-indent 0) ;; We can't indent past the left margin
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented ;; Iterate backwards until we find an indentation hint
            (forward-line -1)
            ;; This hint indicates that we need to indent at the level of the END token
            (if (or (looking-at "^[ \t]*endproc")
                    (looking-at "^[ \t]*endif")
                    ;; not "else" since is is both a begin and an end
                    ;; not "elseif" since is is both a begin and an end
                    (looking-at "^[ \t]*endfor")
                    (looking-at "^[ \t]*endswitch")
                    (looking-at "^[ \t]*endcase")
                    (looking-at "^[ \t]*endwhile")
                    (looking-at "^[ \t]*enddialog"))
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              ;; This hint indicates that we need to indent an extra level
              (if (or (looking-at "^[ \t]*proc")
                      (looking-at "^[ \t]*if")
                      (looking-at "^[ \t]*else")
                      (looking-at "^[ \t]*elseif")
                      (looking-at "^[ \t]*for")
                      (looking-at "^[ \t]*switch")
                      (looking-at "^[ \t]*case")
                      (looking-at "^[ \t]*default")
                      (looking-at "^[ \t]*while")
                      (looking-at "^[ \t]*dialogbox"))
                  (progn
                    (if (and (looking-at "^[ \t]*case") is-case)
                        (setq cur-indent (current-indentation) );; Do the actual indenting
                      (setq cur-indent (+ (current-indentation) tab-width));; Do the actual indenting
                      )
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))));; If we didn't see an indentation hint, then allow no indentation

(defvar procomm-mode-syntax-table
  (let ((procomm-mode-syntax-table (make-syntax-table)))

    ;; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w"     procomm-mode-syntax-table)

    ;; Comment styles are same as lisp
    (modify-syntax-entry ?\; "<   " procomm-mode-syntax-table) ; semicolon is comment starter
    (modify-syntax-entry ?\n ">   " procomm-mode-syntax-table) ; newline is comment ender
    procomm-mode-syntax-table)
  "Syntax table for procomm-mode")

(defun procomm-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'procomm-mode)
  (use-local-map procomm-mode-map)
  (set-syntax-table procomm-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(procomm-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'procomm-indent-line)
  (set (make-local-variable 'comment-start) "\;\;")
  (setq major-mode 'procomm-mode)
  (setq mode-name "Procomm")
  (run-hooks 'procomm-mode-hook)
  )

(provide 'procomm-mode)



;;; procomm-mode.el ends here
