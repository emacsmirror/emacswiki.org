;;; vbs-repl.el --- Support for VBScript programming in Emacs

;;; Copyright: (C) 2009 Charles Sebold
;;
;;     This program is NOT part of GNU Emacs.
;; 
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public License
;;     along with GNU Emacs; if not, write to the Free Software
;;     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;     02110-1301 USA
;;
;; Latest version should be available at:
;;     <URL:http://www.emacswiki.org/emacs/VbsReplMode>
;;

;; Filename: vbs-repl.el
;; Description: Support for VBScript programming in Emacs
;; Author: Charles Sebold <csebold at gmail.com>
;; Maintainer: Charles Sebold <csebold at gmail.com>
;; Created: Thu 19 Feb 2009 10:00
;; Version: 2.1
;; Last-Updated: Fri 20 Feb 2009 17:01
;;           By: Charles Sebold
;;     Update #: 1
;; URL: http://www.emacswiki.org/emacs/VbsReplMode
;; Keywords: vbscript, repl, programming, mode
;; Compatibility: GNU Emacs 22+, untested with Emacs21- or XEmacs
;; 
;; Features that might be required by this library:
;;
;;   visual-basic-mode.el <http://www.emacswiki.org/emacs/VisualBasicMode>

;;; Commentary:
;; 
;; At this point, there are a few different forks of visual basic modes
;; to choose from, if you want to develop VBScript.  The hard thing
;; about scripting in VBScript (especially if you're more accustomed to
;; Lispy sorts of languages) is the lack of a REPL
;; (Read-Eval-Print-Loop), a way to try things and see if they work.
;; The aim of this Emacs package is to provide functionality similar to
;; a REPL, although it won't really be one.
;;
;; The biggest thing holding me back from making this a full-blown REPL
;; is the painful truth that VBScript's "=" operator is ambiguous by
;; itself; it needs context.  For example, do you want to assign the
;; value 5 to the variable x?  Then you can say "x=5" and there, you
;; have assigned that value.  But how do you test for equality between x
;; and 5?  The same way.  In the context of things like If...Then
;; statements, If x=5 Then... will not assign 5 to x, it will test for
;; equality between x and 5.  Hence, just like the difference between
;; Subs and Functions, there are two ways of evaluating a string into
;; VBScript code: Execute (corresponds to Sub), and Eval (corresponds to
;; Function).
;;
;; For the purposes of this code, by default everything is an Execute.
;; If you want something to be Evaled rather than Executed, run it with
;; the prefix argument.  For example, if I'm sitting with the cursor at
;; the end of a variable assignment, like "x = 5", with my keybindings
;; I'll hit C-c C-e to execute that line.  But if I want to know if x =
;; 5, then I'll use the prefix arg.  In that case, I hit C-u C-c C-e,
;; and I get either "True" or "False" in response.
;;
;; Previous versions of this REPL tried to guess whether you wanted code
;; Evaled or Executed.  That was too problematic; maybe in the future we
;; will do that again, but for now you can guide it to do what you want.
;;
;; New in this version: we have a vbscript-mode now, derived from
;; visual-basic-mode.
;;
;; To use this, put it in your load-path and do something like the
;; following in your .emacs file:
;;
;; ; VBScript editing
;; (setq auto-mode-alist
;;       (append '(("\\.\\(vbs\\|wsf\\)$" . vbscript-mode))
;;               auto-mode-alist))
;; 
;; To start the repl, load a VBScript file, make sure you're in
;; visual-basic mode, and hit C-c C-z (or whatever you mapped to
;; vbs-setup-repl).  To execute from the beginning of the line (or the
;; beginning of the statement; it does recognize multiline VBScript),
;; use C-c C-e.  Select a region and execute it with C-c C-r.  If you
;; want it to Eval rather than Execute (like, for example, you want to
;; know what "x & y" would be), Use C-u before the above keystrokes.

;;; History:
;; 
;;  v1.0: implemented a REPL that tried to guess whether you wanted your
;;        code Evaled or Executed
;;  v2.0: new REPL, easier to run without Emacs, now defaults to
;;        Executing everything but you decide when you want it Evaled
;;  v2.1: Implemented `vbscript-mode' derived from visual-basic-mode

;;; Code:

(defvar vbs-repl-script "ReplPrompt = \"> \"
Set ReplFso = CreateObject(\"Scripting.FileSystemObject\")

Sub ReplErrprint(errno)
    WScript.StdOut.Write \"Error # \" & CStr(errno.Number) & \" \" & _
        errno.Description
End Sub

Function ReplWhyMulti(firstLine)
    If Right(firstLine,1) = \"_\" Then
        ReplWhyMulti = \"_\"
    Elseif LCase(Left(firstLine,3)) = \"sub\" Then
        ReplWhyMulti = \"Sub\"
    Elseif LCase(Left(firstLine,8)) = \"function\" Then
        ReplWhyMulti = \"Function\"
    Else
        ReplWhyMulti = \"?\"
    End If
End Function

Function ReplTestEndMulti(lastLine,why)
    lastLine = LTrim(RTrim(lastLine))
    If why = \"_\" And Right(lastLine,1) <> \"_\" Then
        ReplTestEndMulti = True
    Elseif why = \"Sub\" And LCase(lastLine) = \"end sub\" Then
        ReplTestEndMulti = True
    Elseif why = \"Function\" And LCase(lastline) = \"end function\" Then
        ReplTestEndMulti = True
    Else
        ReplTestEndMulti = False
    End If
End Function

Function ReplBufferIn(firstLine)
    firstLine = LTrim(RTrim(firstLine))
    ReplWhy = ReplWhyMulti(firstLine)
    ReplPrompt = ReplWhy & \"> \"
    ReplBufferIn = firstLine
    Do
        WScript.StdOut.Write ReplPrompt
        nextLine = WScript.StdIn.ReadLine
        ReplBufferIn = ReplBufferIn & vbCRLF & nextLine
    Loop Until ReplTestEndMulti(nextLine, ReplWhy)
    ReplPrompt = \"> \"
End Function

While 1:
    WScript.StdOut.Write ReplPrompt
    ReplInput = WScript.StdIn.ReadLine
    If ReplInput <> \"\" Then
        If ReplWhyMulti(ReplInput) <> \"?\" Then
            ReplInput = ReplBufferIn(ReplInput)
            On Error Resume Next
            Execute(ReplInput)
        Else
            ReplInputArgs = Split(LTrim(ReplInput))
            If ReplInputArgs(0) = \"ReplLoad\" Then
                Set ReplFile = ReplFso.OpenTextFile(ReplInputArgs(1), 1)
                ReplInput = \"\"
                Do While ReplFile.AtEndOfStream <> True
                    ReplThisLine = ReplFile.ReadLine
                    ReplInput = ReplInput & ReplThisLine & vbCRLF
                Loop
                ReplFile.Close()
                On Error Resume Next
                Execute(ReplInput)
            Elseif ReplInputArgs(0) = \"ReplLoadEval\" Then
                Set ReplFile = ReplFso.OpenTextFile(ReplInputArgs(1), 1)
                ReplInput = \"\"
                Do While ReplFile.AtEndOfStream <> True
                    ReplThisLine = ReplFile.ReadLine
                    ReplInput = ReplInput & ReplThisLine & vbCRLF
                Loop
                ReplFile.Close()
                On Error Resume Next
                WScript.StdOut.Write Eval(ReplInput) & vbCRLF
            Elseif ReplInputArgs(0) = \"ReplEval\" Then
                On Error Resume Next
                WScript.StdOut.Write Eval(Right(ReplInput,Len(ReplInput)-9)) & _
                       vbCRLF
            Else
                On Error Resume Next
                Execute(ReplInput)
            End If
        End If
        If Err.Number <> 0 Then
            ReplErrprint(Err)
            WScript.StdOut.Write vbCRLF
            Err.Clear
        End If
    End If
    On Error GoTo 0
Wend
")

(defun vbs-run-repl ()
  "Deletes all tempfiles and run a new REPL."
  (interactive)
  (dolist (i (directory-files temporary-file-directory t
                              "vbsrepl.*\\.vbs"))
    (delete-file i))
  (let ((tempfile (make-temp-file "vbsrepl" nil ".vbs")))
    (with-temp-file tempfile
      (insert vbs-repl-script))
    (make-comint "VBScript" "cscript" nil tempfile)))

(defun vbs-setup-repl ()
  "Set up a REPL in the opposite window, restarting it if necessary."
  (interactive)
  (delete-other-windows)
  (if (> (frame-parameter nil 'width) 160)
      (split-window-horizontally)
    (split-window-vertically))
  (other-window 1)
  (if (and (get-buffer "*VBScript*") (get-process "VBScript"))
      (switch-to-buffer "*VBScript*")
    (vbs-run-repl)
    (switch-to-buffer "*VBScript*"))
  (other-window 1))

(defun vbs-execute-expression (arg evalp)
  "Send expression to REPL and evaluate it.
Argument ARG is the prefix argument.
Argument EVALP is whether to sent this to Eval (nil) or Execute (t)."
  (if evalp
      (if (string-match "\n" arg)
          (let ((tempfile (make-temp-file "vbsrepl" nil ".vbs")))
            (with-temp-file tempfile
              (insert arg))
            (comint-send-string "*VBScript*"
                                (concat "ReplLoadEval " tempfile "\n")))
        (comint-send-string "*VBScript*" (concat "ReplEval " arg "\n")))
    (if (string-match "\n" arg)
        (let ((tempfile (make-temp-file "vbsrepl" nil ".vbs")))
          (with-temp-file tempfile
            (insert arg))
          (comint-send-string "*VBScript*"
                              (concat "ReplLoad " tempfile "\n")))
      (comint-send-string "*VBScript*" (concat arg "\n")))))

(defun vbs-last-exp (arg)
  "Return last expression in current buffer before ARG."
  (let ((beginning-pt
         (let ((possible (save-excursion
                           (beginning-of-line)
                           (point))))
           (if (= possible (point-min))
               possible
             (save-excursion
               (while
                   (progn
                     (forward-line -1)
                     (beginning-of-line)
                     (and (not (= (point) (point-min)))
                          (looking-at "[^\n]*?_ *\n"))))
               (if (and (= (point) (point-min))
                        (looking-at "[^\n]*?_ *\n"))
                   (point)
                 (forward-line 1)
                 (point)))))))
    (buffer-substring-no-properties beginning-pt arg)))

(defun vbs-execute-sub ()
  "When within a Sub or Function definition, Execute it."
  (interactive)
  (save-excursion
    (let* ((current (point))
           (end (progn
                  (beginning-of-line)
                  (re-search-forward "^ *end \\(sub\\|function\\)" nil t)
                  (point)))
           (s-or-f (match-string-no-properties 1))
           (start (progn
                    (re-search-backward
                     (concat "^ *" s-or-f " ") nil t)
                    (point))))
      (if (> start current)
          (message "Not in a sub or function.")
        (vbs-execute-expression
         (buffer-substring-no-properties start end)
         nil)))))

(defun vbs-execute-region (start end)
  "Send region from START to END to REPL."
  (interactive "r")
  (vbs-execute-expression
   (buffer-substring-no-properties start end)
   current-prefix-arg))

(defun vbs-execute (arg)
  "Evaluate VBScript expression before point.
Argument ARG is the prefix arg, add prefix if you want this Evaled
rather than Executed."
  (interactive "P")
  (let ((eval-this (vbs-last-exp (point))))
    (if eval-this
        (vbs-execute-expression eval-this arg)
      (message "Unable to find last expression."))))

(require 'visual-basic-mode)

;; This is some approximation of the set of reserved words in VBScript.
(eval-and-compile
  (defvar vbscript-all-keywords
    '("Abs" "And" "Array" "Asc" "AscB" "AscW" "Assignments" "Atn"
      "CBool" "CByte" "CCur" "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr"
      "Call" "Case" "Chr" "ChrB" "ChrW" "Cint" "Class" "Clear" "Const"
      "Conversions" "Cos" "CreateObject" "Date" "DateAdd" "DateDiff"
      "DatePart" "DateSerial" "DateValue" "Day" "Derived" "Description"
      "Dim" "Do" "Each" "Else" "ElseIf" "Empty" "End" "Eqv" "Erase" "Err"
      "Error" "Eval" "Execute" "ExecuteGlobal" "Exit" "Exp" "Explicit"
      "False" "Filter" "FirstIndex" "Fix" "Fixs" "For" "FormatCurrency"
      "FormatDateTime" "FormatNumber" "FormatPercent" "Function" "GetLocale"
      "GetObject" "GetRef" "Global" "HelpContext" "HelpFile" "Hex" "Hour"
      "If" "IgnoreCase" "Imp" "InStr" "InStrB" "InStrRev" "InputBox" "Instr"
      "Int" "Int," "Is" "IsArray" "IsArray " "IsDate" "IsEmpty" "IsNull"
      "IsNumeric" "IsObject" "Join" "LBound" "LCase" "LTrim" "Lcase" "Left"
      "LeftB" "Len" "LenB" "Length" "Let" "LoadPicture" "Log" "Loop" "Ltrim"
      "Match" "Math" "Maths" "Mid" "MidB" "Minute" "Mod" "Month" "MonthName"
      "MsgBox" "New" "Next" "Not" "Nothing" "Now" "Null" "Number" "Oct" "On"
      "Option" "Or" "Pattern" "Private" "Procedures" "Public" "RGB" "RTrim"
      "Raise" "Randomize" "ReDim" "Rem" "Replace" "Right" "RightB" "Rnd"
      "Round" "Rtrim" "ScriptEngine" "ScriptEngineBuildVersion"
      "ScriptEngineMajorVersion" "ScriptEngineMinorVersion" "Second"
      "Select" "Set" "SetLocale" "Sgn" "Sin" "Source" "Space" "Split" "Sqr"
      "StrComp" "String" "Strings" "Sub" "SubMatches" "Tan" "Test" "Then"
      "Time" "TimeSerial" "TimeValue" "Timer" "Trim" "Trims" "True"
      "TypeName" "UBound" "UCase" "Ucase" "Until" "Value" "VarType"
      "Variants" "VbCrLf" "Weekday" "WeekdayName" "Wend" "While" "With"
      "Xor" "Year" "vbAbort" "vbAbortRetryIgnore" "vbApplicationModal"
      "vbArray" "vbBinaryCompare" "vbBlack" "vbBlue" "vbBoolean" "vbByte"
      "vbCancel" "vbCr" "vbCritical" "vbCurrency" "vbCyan" "vbDataObject"
      "vbDate" "vbDecimal" "vbDefaultButton1" "vbDefaultButton2"
      "vbDefaultButton3" "vbDefaultButton4" "vbEmpty" "vbError"
      "vbExclamation" "vbFalse" "vbFirstFourDays" "vbFirstFullWeek"
      "vbFirstJan1" "vbFormFeed" "vbFriday" "vbGeneralDate" "vbGreen"
      "vbIgnore" "vbInformation" "vbInteger" "vbLf" "vbLong" "vbLongDate"
      "vbLongTime" "vbMagenta" "vbMonday" "vbNewLine" "vbNo" "vbNull"
      "vbNullChar" "vbNullString" "vbOK" "vbOKCancel" "vbOKOnly" "vbObject"
      "vbObjectError" "vbQuestion" "vbRed" "vbRetry" "vbRetryCancel"
      "vbSaturday" "vbShortDate" "vbShortTime" "vbSingle" "vbString"
      "vbSunday" "vbSystemModal" "vbTab" "vbTextCompare" "vbThursday"
      "vbTrue" "vbTuesday" "vbUseDefault" "vbUseSystemDayOfWeek" "vbVariant"
      "vbVerticalTab" "vbWednesday" "vbWhite" "vbYellow" "vbYes" "vbYesNo"
      "vbYesNoCancel")))

(defvar vbscript-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Names of functions.
     (list visual-basic-defun-start-regexp
           '(1 font-lock-keyword-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-function-name-face))

     ;; Statement labels
     (cons visual-basic-label-regexp 'font-lock-keyword-face)

     ;; Case values
     ;; String-valued cases get font-lock-string-face regardless.
     (list "^[ \t]*case[ \t]+\\([^'\n]+\\)" 1 'font-lock-keyword-face t)

     ;; Any keywords you like.
     (list (regexp-opt
            '("Dim" "If" "Then" "Else" "ElseIf" "End If") 'words)
           1 'font-lock-keyword-face))))

(defvar vbscript-font-lock-keywords-2
  (append vbscript-font-lock-keywords-1
          (eval-when-compile
            `((, (regexp-opt vbscript-all-keywords 'words)
                   1 font-lock-keyword-face)))))

(defvar vbscript-font-lock-keywords vbscript-font-lock-keywords-1)

(define-derived-mode vbscript-mode visual-basic-mode "VBScript"
  "Major mode for VBScript.
\\{vbscript-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        `((vbscript-font-lock-keywords
           vbscript-font-lock-keywords-1
           vbscript-font-lock-keywords-2)
          nil t ((,(string-to-char "_") . "w")))))

(define-key vbscript-mode-map "\C-c\C-z" 'vbs-setup-repl)
(define-key vbscript-mode-map "\C-c\C-e" 'vbs-execute)
(define-key vbscript-mode-map "\C-c\C-r" 'vbs-execute-region)
(define-key vbscript-mode-map "\C-\M-x" 'vbs-execute-sub)

(provide 'vbs-repl)
(provide 'vbs-repl)

;;; vbs-repl.el ends here
