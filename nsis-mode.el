;;; nsis-mode.el --- NSIS-mode
;;
;; Filename: nsis-mode.el
;; Description: NSIS mode
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Nov 16 15:48:02 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Mon Feb  7 11:04:22 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 1414
;; URL: http://www.emacswiki.org/emacs/download/nsis-mode.el
;; Keywords: NSIS
;; Compatibility: Emacs 23.2
;;
;; Features that might be required by this library:
;;
;;   `cl', `easymenu', `font-lock', `hideshow', `syntax',
;;   `w32-browser'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Put this in the load path, then add the following to your Emacs:
;;
;;  (autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
;;
;; (setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
;;                                  nsis-mode)) auto-mode-alist))
;;
;; (setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
;;                                  nsis-mode)) auto-mode-alist))
;;
;; Thats it.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 07-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Mon Feb  7 11:03:14 2011 (-0600) #1413 (Matthew L. Fidler)
;;    Added check to make sure compile went OK before launching executable.
;; 25-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Jan 18 14:31:23 2011 (-0600) #1410 (us041375)
;;    Added more explicit setup instructions
;; 06-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Dec  6 19:11:01 2010 (-0600) #1408 (Matthew L. Fidler)
;;    Changed comment start and comment stop to single line semi-colons
;; 06-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Dec  6 09:05:34 2010 (-0600) #1404 (Matthew L. Fidler)
;;    Made nsis-yas-description not depend on finding MUI_FUNCTION_DESCRIPTION_BEGIN.  If MUI_DESCRIPTION_TEXT is found, insert there.
;; 06-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Dec  6 09:05:06 2010 (-0600) #1403 (Matthew L. Fidler)
;;    Updated indentation line function (bug-fix)
;; 23-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov 23 08:49:52 2010 (-0600) #1288 (Matthew L. Fidler)
;;    Macros that end with END or BEGIN are indentation keywords.
;; 23-Nov-2010      
;;    Last-Updated: Tue Nov 23 08:19:48 2010 (-0600) #1277 (Matthew L. Fidler)
;;    Changed indentation routine (bugfix)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; Code:

(require 'font-lock)
(require 'cl)

(defvar nsis-version "0.1"
  "NSIS-mode version")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords from HM NIS Edit source Syntax.ini and the manuals.  Added logic lib
;; and other libraries.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar nsis-syntax-reserved-word
  '(
    "AddBrandingImage"
    "AddSize"
    "AllowRootDirInstall"
    "AllowSkipFiles"
    "AutoCloseWindow"
    "BGFont"
    "BGGradient"
    "BrandingText"
    "CRCCheck"
    "Caption"
    "ChangeUI"
    "CheckBitmap"
    "CompletedText"
    "ComponentText"
    "DetailsButtonText"
    "DirShow"
    "DirText"
    "DirVar"
    "DirVerify"
    "FileBufSize"
    "FileErrorText"
    "FunctionEnd"
    "GetInstDirError"
    "Icon"
    "InstProgressFlags"
    "InstType"
    "InstallButtonText"
    "InstallColors"
    "InstallDir"
    "InstallDirRegKey"
    "LangString"
    "LangStringUP"
    "LicenseBkColor"
    "LicenseData"
    "LicenseForceSelection"
    "LicenseLangString"
    "LicenseText"
    "LoadLanguageFile"
    "MiscButtonText"
    "Name"
    "OutFile"
    "Page"
    "PageCallbacks"
    "PageEx"
    "PageExEnd"
    "RequestExecutionLevel"
    "Section"
    "SectionEnd"
    "SectionGroup"
    "SectionGroupEnd"
    "SectionIn"
    "SetCompress"
    "SetCompressionLevel"
    "SetCompressor"
    "SetCompressorDictSize"
    "SetDatablockOptimize"
    "SetDateSave"
    "SetFont"
    "SetOverwrite"
    "SetPluginUnload"
    "ShowInstDetails"
    "ShowUninstDetails"
    "SilentInstall"
    "SilentUnInstall"
    "SpaceTexts"
    "SubCaption"
    "SubSection"
    "SubSectionEnd"
    "UninstPage"
    "UninstallButtonText"
    "UninstallCaption"
    "UninstallIcon"
    "UninstallSubCaption"
    "UninstallText"
    "VIAddVersionKey"
    "VIProductVersion"
    "Var"
    "WindowIcon"
    "XPStyle"
    "Function"
    )
  "Reserved Words"
  )
(defvar nsis-syntax-function
  '(
    "File"
    "Exec"
    "ExecWait"
    "ExecShell"
    "Rename"
    "Delete"
    "RMDir"
    "WriteRegStr"
    "WriteRegExpandStr"
    "WriteRegDWORD"
    "WriteRegBin"
    "WriteINIStr"
    "ReadRegStr"
    "ReadRegDWORD"
    "ReadEnvStr"
    "ExpandEnvStrings"
    "DeleteRegValue"
    "DeleteRegKey"
    "EnumRegKey"
    "EnumRegValue"
    "DeleteINISec"
    "DeleteINIStr"
    "CreateDirectory"
    "CopyFiles"
    "SetFileAttributes"
    "CreateShortCut"
    "GetFullPathName"
    "SearchPath"
    "GetTempFileName"
    "CallInstDLL"
    "RegDLL"
    "UnRegDLL"
    "GetDLLVersion"
    "GetDLLVersionLocal"
    "GetFileTime"
    "GetFileTimeLocal"
    "Goto"
    "Call"
    "Return"
    "IfErrors"
    "ClearErrors"
    "SetErrors"
    "FindWindow"
    "SendMessage"
    "IsWindow"
    "IfFileExists"
    "MessageBox"
    "StrCmp"
    "IntCmp"
    "IntCmpU"
    "Abort"
    "Quit"
    "GetFunctionAddress"
    "GetLabelAddress"
    "GetCurrentAddress"
    "FindFirst"
    "FindNext"
    "FindClose"
    "FileOpen"
    "FileClose"
    "FileRead"
    "FileWrite"
    "FileReadByte"
    "FileWriteByte"
    "FileSeek"
    "SetDetailsView"
    "SetDetailsPrint"
    "SetAutoClose"
    "DetailPrint"
    "Sleep"
    "BringToFront"
    "HideWindow"
    "SetShellVarContext"
    "StrCpy"
    "StrLen"
    "Push"
    "Pop"
    "Exch"
    "IntOp"
    "IntFmt"
    "Reboot"
    "IfRebootFlag"
    "SetRebootFlag"
    "WriteUninstaller"
    "LogSet"
    "LogText"
    "SectionSetFlags"
    "SectionGetFlags"
    "SectionSetText"
    "SectionGetText"
    "GetDlgItem"
    "SetBrandingImage"
    "CreateFont"
    "ReserveFile"
    "GetWindowText"
    "ShowWindow"
    "FlushINI"
    "SectionSetInstTypes"
    "SectionGetInstTypes"
    "InitPluginsDir"
    "ReadIniStr"
    "IfAbort"
    "SectionSetSize"
    "SectionGetSize"
    "SetCurInstType"
    "GetCurInstType"
    "InstTypeSetText"
    "InstTypeGetText"
    "Nop"
    "EnableWindow"
    "SetCtlColors"
    "IfSilent"
    "SetSilent"
    "SetOutPath"
    "LockWindow"
    "GetErrorLevel"
    "SetErrorLevel")
  "* nsis syntax function")
(defvar nsis-syntax-directive
  '(
    "!system"
    "!include"
    "!cd"
    "!packhdr"
    "!define"
    "!undef"
    "!ifdef"
    "!ifndef"
    "!endif"
    "!else"
    "!macro"
    "!macroend"
    "!insertmacro"
    "!verbose"
    "!warning"
    "!error"
    "!echo"
    "!addIncludeDir"
    "!addplugindir"
    "!ifmacrodef"
    "!ifmacrondef"
    "!execute"
    )
  "nsis syntax directive")
(defvar nsis-syntax-parameter
  '(
    "custom"
    "license"
    "components"
    "directory"
    "instfiles"
    "uninstConfirm"
    "true"
    "false"
    "on"
    "off"
    "force"
    "show"
    "hide"
    "nevershow"
    "normal"
    "silent"
    "silentlog"
    "auto"
    "zlib"
    "bzip2"
    "lzma"
    "try"
    "ifnewer"
    "manual"
    "alwaysoff"
    "RO"
    "SW_SHOWNORMAL"
    "SW_SHOWMAXIMIZED"
    "SW_SHOWMINIMIZED"
    "HKCR"
    "HKEY_CLASSES_ROOT"
    "HKLM"
    "HKEY_LOCAL_MACHINE"
    "HKCU"
    "HKEY_CURRENT_USER"
    "HKU"
    "HKEY_USERS"
    "HKCC"
    "HKEY_CURRENT_CONFIG"
    "HKDD"
    "HKEY_DYN_DATA"
    "HKPD"
    "HKEY_PERFORMANCE_DATA"
    "SHCTX"
    "FILE_ATTRIBUTE_NORMAL"
    "ARCHIVE"
    "FILE_ATTRIBUTE_ARCHIVE"
    "HIDDEN"
    "FILE_ATTRIBUTE_HIDDEN"
    "OFFLINE"
    "FILE_ATTRIBUTE_OFFLINE"
    "READONLY"
    "FILE_ATTRIBUTE_READONLY"
    "SYSTEM"
    "FILE_ATTRIBUTE_SYSTEM,TEMPORARY"
    "FILE_ATTRIBUTE_TEMPORARY"
    "MB_OK"
    "MB_OKCANCEL"
    "MB_ABORTRETRYIGNORE"
    "MB_RETRYCANCEL"
    "MB_YESNO"
    "MB_YESNOCANCEL"
    "MB_ICONEXCLAMATION"
    "MB_ICONINFORMATION"
    "MB_ICONQUESTION"
    "MB_ICONSTOP"
    "MB_TOPMOST"
    "MB_SETFOREGROUND"
    "MB_RIGHT"
    "MB_DEFBUTTON1"
    "MB_DEFBUTTON2"
    "MB_DEFBUTTON3"
    "MB_DEFBUTTON4"
    "MB_RTLREADING"
    "IDABORT"
    "IDCANCEL"
    "IDIGNORE"
    "IDNO"
    "IDOK"
    "IDRETRY"
    "IDYES"
    "SW_HIDE"
    "current"
    "all"
    "none"
    "listonly"
    "textonly"
    "both"
    "lastused"
    "checkbox"
    "radiobuttons"
    "ifdiff"
    "leave"
    )
  "* nsis parameters"
  )
(defvar nsis-syntax-parameter-slash
  '(
    "/COMPONENTSONLYONCUSTOM"
    "/CUSTOMSTRING"
    "/FILESONLY"
    "/IMGID"
    "/ITALIC"
    "/NOCUSTOM"
    "/NOUNLOAD"
    "/REBOOTOK"
    "/RESIZETOFIT"
    "/SD"
    "/SHORT"
    "/SOLID"
    "/STRIKE"
    "/TIMEOUT"
    "/TRIMCENTER"
    "/TRIMLEFT"
    "/TRIMRIGHT"
    "/UNDERLINE"
    "/WAIT"
    "/a"
    "/components"
    "/e"
    "/ifempty"
    "/lang"
    "/nonfatal"
    "/o"
    "/oname"
    "/r"
    "/silent"
    "/windows"
    "/x"
    "/GRADIENT"
    )
  "* nsis Parameters (w/slash)")

(defvar nsis-syntax-variable
  (append
   ;; Unlike what is in HM NIS editor, there are 20 registers, put them ALL in.
   (mapcar (lambda(x) (format "$%s" x))
           (number-sequence 0 20))
   (mapcar (lambda(x) (format "$R%s" x))
           (number-sequence 0 20))
   '(
     "$INSTDIR"
     "$OUTDIR"
     "$CMDLINE"
     "$PROGRAMFILES"
     "$PROGRAMFILES32"
     "$PROGRAMFILES64"
     "$DESKTOP"
     "$EXEDIR"
     "$EXEFILE"
     "$EXEPATH"
     "$WINDIR"
     "$SYSDIR"
     "$TEMP"
     "$STARTMENU"
     "$SMPROGRAMS"
     "$SMSTARTUP"
     "$QUICKLAUNCH"
     "$HWNDPARENT"
     "$LANGUAGE"
     "$PLUGINSDIR"
     "$COMMONFILES"
     "$COMMONFILES32"
     "$COMMONFILES64"
     "$DOCUMENTS"
     "$SENDTO"
     "$RECENT"
     "$FAVORITES"
     "$MUSIC"
     "$PICTURES"
     "$VIDEOS"
     "$NETHOOD"
     "$FONTS"
     "$TEMPLATES"
     "$APPDATA"
     "$LOCALAPPDATA"
     "$PRINTHOOD"
     "$INTERNET_CACHE"
     "$COOKIES"
     "$HISTORY"
     "$PROFILE"
     "$ADMINTOOLS"
     "$RESOURCES"
     "$RESOURCES_LOCALIZED"
     "$CDBURN_AREA"
     "$HWNDPARENT"
     ;; Extra undefined in
     "$nsisDIR"
     ))
  "* nsis syntax variables"
  )
(defvar nsis-syntax-callback
  '(
    ".onGUIEnd"
    ".onGUIInit"
    ".onInit"
    ".onInstFailed"
    ".onInstSuccess"
    ".onMouseOverSection"
    ".onRebootFailed"
    ".onSelChange"
    ".onUserAbort"
    ".onVerifyInstDir"
    "un.onGUIEnd"
    "un.onGUIInit"
    "un.onInit"
    "un.onRebootFailed"
    "un.onUninstFailed"
    "un.onUninstSuccess"
    "un.onUserAbort"
    )
  "nsis callback syntax")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Keywords from Logic Lib
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar nsis-syntax-logiclib
  '(
    "${Abort}"
    "${AndIfNot}"
    "${AndIf}"
    "${AndUnless}"
    "${Break}"
    "${Case2}"
    "${Case3}"
    "${Case4}"
    "${Case5}"
    "${CaseElse}"
    "${Case}"
    "${Cmd}"
    "${Cmd}"
    "${Continue}"
    "${Default}"
    "${DoUntil}"
    "${DoWhile}"
    "${Do}"
    "${ElseIfNot}"
    "${ElseIf}"
    "${ElseUnless}"
    "${Else}"
    "${EndIf}"
    "${EndSelect}"
    "${EndSwitch}"
    "${EndUnless}"
    "${EndWhile}"
    "${Errors}"
    "${ExitDo}"
    "${ExitFor}"
    "${ExitWhile}"
    "${FileExists}"
    "${ForEach}"
    "${For}"
    "${IfCmd}"
    "${IfNotThen}"
    "${IfNot}"
    "${IfThen}"
    "${If}"
    "${LoopUntil}"
    "${LoopWhile}"
    "${Loop}"
    "${Next}"
    "${OrIfNot}"
    "${OrIf}"
    "${OrUnless}"
    "${RebootFlag}"
    "${SectionIsBold}"
    "${SectionIsExpanded}"
    "${SectionIsPartiallySelected}"
    "${SectionIsReadOnly}"
    "${SectionIsSectionGroupEnd}"
    "${SectionIsSectionGroup}"
    "${SectionIsSelected}"
    "${Select}"
    "${Silent}"
    "${Switch}"
    "${Unless}"
    "${While}"
    "="
    "=="
    "L="
    "S<"
    "S=="
    "U<"
    )
  "* nsis logic-lib keywords"
  )
(defvar nsis-syntax-logiclib-regexp (regexp-opt nsis-syntax-logiclib t))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other keywords from nsDialogs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar nsis-syntax-nsd-macros
  '(
    "${NSD_CreateHLine}"
    "${NSD_CreateVLine}"
    "${NSD_CreateLabel}"
    "${NSD_CreateIcon}"
    "${NSD_CreateBitmap}"
    "${NSD_CreateBrowseButton}"
    "${NSD_CreateLink}"
    "${NSD_CreateButton}"
    "${NSD_CreateGroupBox}"
    "${NSD_CreateCheckBox}"
    "${NSD_CreateRadioButton}"
    "${NSD_CreateText}"
    "${NSD_CreatePassword}"
    "${NSD_CreateNumber}"
    "${NSD_CreateFileRequest}"
    "${NSD_CreateDirRequest}"
    "${NSD_CreateComboBox}"
    "${NSD_CreateDropList}"
    "${NSD_CreateListBox}"
    "${NSD_CreateProgressBar}"
    "${NSD_OnBack}"
    "${NSD_OnChange}"
    "${NSD_OnClick}"
    "${NSD_OnNotify}"
    "${NSD_CreateTimer}"
    "${NSD_KillTimer}"
    "${NSD_AddStyle}"
    "${NSD_AddExStyle}"
    "${NSD_GetText}"
    "${NSD_SetText}"
    "${NSD_SetTextLimit}"
    "${NSD_GetState}"
    "${NSD_SetState}"
    "${NSD_Check}"
    "${NSD_Uncheck}"
    "${NSD_CB_AddString}"
    "${NSD_CB_SelectString}"
    "${NSD_LB_AddString}"
    "${NSD_LB_DelString}"
    "${NSD_LB_Clear}"
    "${NSD_LB_GetCount}"
    "${NSD_LB_SelectString}"
    "${NSD_LB_GetSelection}"
    "${NSD_SetFocus}"
    "${NSD_SetImage}"
    "${NSD_SetStretchedImage}"
    "${NSD_SetIcon}"
    "${NSD_SetIconFromInstaller}"
    "${NSD_ClearImage}"
    "${NSD_ClearIcon}"
    "${NSD_FreeImage}"
    "${NSD_FreeIcon}"
    )
  "NSD Macros")
(defvar nsis-syntax-nsd
  '(
    "Create"
    "CreateControl"
    "Show"
    "SelectFileDialog"
    "SelectFolderDialog"
    "SetRTL"
    "GetUserData"
    "SetUserData"
    "OnBack"
    "OnChange"
    "OnClick"
    "OnNotify"
    "CreateTimer"
    "KillTimer"
    )
  "NSD functions")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font Lock Keywords
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nsis-font-lock-extend-region-continue ()
  "Extends region for multi-line matches, and multi-line comments"
  (interactive)
  (condition-case error
      (progn
        (let (ret tmp end
                  (debug-on-error t))
          (save-excursion
            (if (boundp 'font-lock-beg)
                (goto-char font-lock-beg))
            (beginning-of-line)
            (while (re-search-backward "[\\][ \t]*\n\=" nil t)
              (setq ret t)
              (beginning-of-line)))
          (if ret
              (setq font-lock-beg (point)))
          (symbol-value 'ret)))
    (error
     (message "Error in `nsis-font-lock-extended-region-continue': %s" (error-message-string error))
     nil)))

(defface nsis-font-lock-bold-string-face nil
  "Font lock bold string face."
  :group 'nsis-mode)

(defface nsis-font-lock-italic-string-face nil
  "Font lock italic string face."
  :group 'nsis-mode)

(defface nsis-font-lock-italic-type-face nil
  "Font lock italic constant face."
  :group 'nsis-mode)

(defface nsis-font-lock-bold-function-name-face nil
  "Face for font lock bold functions"
  :group 'nsis-mode)
(defface nsis-font-lock-italic-function-name-face nil
  "Face for font lock italic functions"
  :group 'nsis-mode)

(defun nsis-construct-faces ()
  "* Construct modified faces"
  (copy-face 'font-lock-string-face 'nsis-font-lock-bold-string-face)
  (set-face-attribute 'nsis-font-lock-bold-string-face nil
                      :weight 'bold)
  (copy-face 'font-lock-string-face 'nsis-font-lock-italic-string-face)
  (set-face-attribute 'nsis-font-lock-italic-string-face nil
                      :slant 'italic)
  (copy-face 'font-lock-type-face 'nsis-font-lock-italic-type-face)
  (set-face-attribute 'nsis-font-lock-italic-type-face nil
                      :slant 'italic)
  (copy-face 'font-lock-function-name-face 'nsis-font-lock-bold-function-name-face)
  (set-face-attribute 'nsis-font-lock-bold-function-name-face nil
                      :weight 'bold)
  (copy-face 'font-lock-function-name-face 'nsis-font-lock-italic-function-name-face)
  (set-face-attribute 'nsis-font-lock-italic-function-name-face nil
                      :slant 'italic)
  )

(defun nsis-font-lock-section-quote (limit)
  "Font Locking for section with quotes.  Puts bolds & italics on ! and - sections respectively."
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (let ((ret
               (re-search-forward "\\<\\(Section\\(?:Group\\)?\\(?:[ \t]+/[oe]\\)?\\)[ \t]+\\([\"`']\\)\\([!-]\\)?\\(\\(?:.*?[$][\\\\]\\2\\)*.*?\\)\\(\\2\\)\\(?:[ \t]+\\([A-Za-z][A-Za-z0-9_]*\\)\\)?" limit t)))
          (when (match-string 3)
            (cond
             ((string= "!" (match-string 3))
              ;; Bold
              (add-text-properties (match-beginning 4) (match-end 4)
                                   '(face nsis-font-lock-bold-string-face)))
             ((string= "-" (match-string 3))
              ;; Hidden
              (add-text-properties (match-beginning 4) (match-end 4)
                                   '(face nsis-font-lock-italic-string-face)))))
          (symbol-value 'ret))
        )
    (error
     (message "font-lock-error `nsis-font-lock-section-quote': %s" (error-message-string error))
     nil)))
(defun nsis-font-lock-section-no-quote (limit)
  "Font locking for section without quotes"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (let ((ret
               (re-search-forward "\\<\\(Section\\(?:Group\\)?\\(?:[ \t]+/[oe]\\)?\\)[ \t]+\\([!-]\\)?\\([A-Za-z_][A-Za-z0-9_]*\\)\\(?:[ \t]+\\([A-Za-z][A-Za-z0-9_]*\\)\\)?" limit t)))
          ;; Add type to /o
          (when (match-string 2)
            (cond
             ((string= "!" (match-string 2))
              ;; Bold
              (add-text-properties (match-beginning 3) (match-end 3)
                                   '(face nsis-font-lock-bold-function-name-face))
              ;; Not sure why, but drops the next face if not explicitly used
              (if (match-string 4)
                  (add-text-properties (match-beginning 4) (match-end 4)
                                       '(face font-lock-variable-name-face)))
              )
             ((string= "-" (match-string 2))
              ;; Italic
              (add-text-properties (match-beginning 3) (match-end 3)
                                   '(face nsis-font-lock-italic-function-name-face))
              ;; Not sure why, but drops the next face if not explicitly used
              (if (match-string 4)
                  (add-text-properties (match-beginning 4) (match-end 4)
                                       '(face font-lock-variable-name-face))))))
          (symbol-value 'ret))
        )
    (error
     (message "Font Lock error in `nsis-font-lock-section-no-quote': %s" (error-message-string error))
     nil)))

(defun nsis-font-lock-unknown-switches (limit)
  "nsis font lock unknown switches"
  (interactive (list (point-max)))
  (condition-case error
      (progn
        (let ((ret (re-search-forward "[ \t]\([/]\w+\)" limit t)))
          (when (and ret (not (memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-string-face))))
            (add-text-properties (match-beginning 1) (match-end 1)
                                 '(face nsis-font-lock-italic-type-face)))
          (symbol-value 'ret)))
    (error
     (message "Font Lock Error in `nsis-font-lock-unknown-switches': %s" (error-message-string error))
     nil)))
(defun nsis-font-lock-no-comment (limit expr &optional excluded-face-list)
  "Make sure that this expression does not start in a comment."
  (interactive (list (point-max) "\\([\"`']\\)\\(\\(?:\n\\|.\\)*?[$][\\\\]\\1\\)*\\(?:\n\\|.\\)*?\\1"))
  (let* (
         (md (match-data))
         (faces (or excluded-face-list '(font-lock-comment-face)))
         (debug-on-quit t)
         (debug-on-error t)
         (done nil)
         (pt (point))
         (ret (re-search-forward expr limit t)))
    (condition-case error
        (while (not done)
          (cond
           ((not ret) ;; Not found.  Go to original point and restore match data.
            (setq done t)
            (goto-char pt)
            (set-match-data md))
           ((memq (get-text-property (match-beginning 0) 'face) faces) ;; Found face, this should not be highlighted
            (goto-char (min (+ 1 (match-beginning 0)) limit)))
           ((memq (get-text-property (- (match-end 0) 1) 'face) faces) ;; Found face, this should not be highlighted
            (goto-char (min (match-end 0) limit)))
           (t ;; Found match.
            (setq done t)))
          (unless done
            (setq ret (re-search-forward expr limit t)))
          )
      (error
       (message "Font Lock Error `nsis-font-lock-no-comment': %s" (error-message-string error))
       (setq ret nil))
      )
    (symbol-value 'ret)
    ))

(defun nsis-font-lock-multi-line-string (limit)
  "Font lock for multi-line string.  Make sure the string is not in a comment."
  (interactive (list (point-max)))
  (nsis-font-lock-no-comment limit "\\([\"`']\\)\\(\\(?:\n\\|.\\)*?[$][\\\\]\\1\\)*\\(?:\n\\|.\\)*?\\1"))

(defun nsis-font-lock-syntax-variable (limit)
  "Font lock of syntax variable -- not allowed in comments"
  (interactive (list (point-max)))  
  (nsis-font-lock-no-comment limit (eval-when-compile
                                     (replace-regexp-in-string "@" "\\>"
                                                               (regexp-opt
                                                                (append
                                                                 (list
                                                                  "$(^Font)"
                                                                  "$(^FontSize)"
                                                                  "$\\n"
                                                                  "$\\r"
                                                                  "$\\t"
                                                                  "$\\\""
                                                                  "$\\'"
                                                                  "$\\`"
                                                                  "$$")
                                                                 (mapcar (lambda(x)
                                                                           (if (string-match "[0-9]$" x)
                                                                               x
                                                                             (concat x "@"))
                                                                           )
                                                                         nsis-syntax-variable)
                                                                 (mapcar (lambda(x)
                                                                           (concat "${" (substring x 1) "}")
                                                                           )
                                                                         nsis-syntax-variable))
                                                                t) t  t))))
(setq nsis-font-lock-syntactic-keywords
      '(
        ("[$]\\([\\\\]\\)" 1 "\\")
        ("\\(/\\)[*]" 1 "<")
        ("[*]\\(/\\)" 1 ">")
        )
      )
(setq nsis-font-lock-keywords
      `(
        ;; Multi-line comment #1
        (,(lambda(limit)
            (nsis-font-lock-no-comment limit "[;#]\\(.*?[\\\\][ \t]*\n\\)*.*"
                                       '(font-lock-string-face)))
         (0 font-lock-comment-face t))
        (,(eval-when-compile
            (regexp-opt
             (append
              nsis-syntax-callback
              nsis-syntax-directive)
             'words))
         (1 font-lock-builtin-face))
        (,(eval-when-compile
            (regexp-opt (append
                         nsis-syntax-reserved-word
                         nsis-syntax-function)
                        'words))
         (1 font-lock-builtin-face))
        ("\\<\\(Var\\)\\>[ \t]+\\(/GLOBAL\\)[ \t]+\\<\\([A-Za-z][A-Za-z0-9_]*\\)\\>"
         (1 font-lock-builtin-face)
         (2 font-lock-type-face)
         (3 font-lock-variable-name-face))
        ("^[ \t]*\\([^-+!$0-9\n \t;#][^ \t\n]*?:\\)[ \t]*\\($\\|[#;]\\|/[*].*?[*]/[ \t]*$\\|/[*].*?$\\)"
         (1 font-lock-type-face))
        
        ("\\<\\(Var\\|!define\\)\\>[ \t]+\\<\\([A-Za-z][A-Za-z0-9_]*\\)\\>"
         (1 font-lock-builtin-face)
         (2 font-lock-variable-name-face))
        ("\\<\\(!insertmacro\\|Page\\|Uninstpage\\|PageEx\\|Function\\)\\>[ \t]+\\<\\([A-Za-z][A-Za-z0-9_]*\\)\\>"
         (1 font-lock-builtin-face)
         (2 font-lock-function-name-face))
        (,nsis-syntax-logiclib-regexp
         (0 font-lock-keyword-face t))
        ("$\\([A-Za-z_][A-Za-z0-9_]*\\>\\|{[A-Za-z_][A-Za-z0-9_]*}\\)"
         (0 font-lock-variable-name-face t))
        
        (nsis-font-lock-unknown-switches )
        
        
        (nsis-font-lock-syntax-variable
         (1 font-lock-constant-face t))
        
        (,(lambda(limit)
            (nsis-font-lock-no-comment limit "\\<un[.]"
                                       '(font-lock-comment-face font-lock-string-face)))
         (0 font-lock-warning-face t))
        ("\\([\\\\]\\)[ \t]*$"
         (0 font-lock-warning-face t))
        ("\\<[A-Za-z_][A-Za-z_0-9]*::"
         (0 font-lock-constant-face t))
        (,(lambda(limit)
            (nsis-font-lock-no-comment limit
                                       (eval-when-compile
                                         (replace-regexp-in-string "'" "\\>"
                                                                   (replace-regexp-in-string "`" "\\<"
                                                                                             (regexp-opt
                                                                                              (append (mapcar (lambda(x) (concat "`" x "'")) nsis-syntax-parameter)
                                                                                                      (mapcar (lambda(x) (concat x "'")) nsis-syntax-parameter-slash))
                                                                                              't) nil t) nil t))
                                       '(font-lock-comment-face font-lock-string-face)))
         (1 font-lock-type-face t))
        (nsis-font-lock-section-no-quote
         ;;         (1 font-lock-builtin-face t)
         (2 font-warning-face t t)
         (3 font-lock-function-name-face t)
         (4 font-lock-variable-name-face t t))
        (nsis-font-lock-section-quote
         ;;(1 font-lock-builtin-face)
         (2 font-lock-string-face t)
         (3 font-lock-warning-face t t)
         (4 font-lock-string-face t)
         (5 font-lock-string-face t)
         (6 font-lock-variable-name-face t t))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imenu
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq nsis-imenu-generic-expression
      (eval-when-compile
        (reverse
         (list
          ;; Sections
          (list "Sections" "^[ \t]*\\<Section\\>[ \t]+\\(?:/o[ \t]+\\)?\\(\"[!-]?\\(?:.*?\\)\"\\|[!-]?\\w+\\)" 1)
          
          (list "SectionGroups" "^[ \t]*\\<SectionGroup\\>[ \t]+\\(?:/e[ \t]+\\)?\\(\"[!-]?\\(?:.*?\\)\"\\|[!-]?\\w+\\)" 1)
          
          (list "Functions" "^[ \t]*\\<Function\\>[ \t]+\\<\\([A-Za-z][A-Za-z0-9_]*\\)\\>[^.]" 1)
          
          (list "Pages" "^[ \t]*\\<\\(?:Uninstpage\\|Page\\(?:Ex\\)?\\)\\>[ \t]+\\<\\([A-Za-z][A-Za-z0-9_]*\\)\\>[^.]" 1)
          
          (list "Inserted Macro" "^[ \t]*\\<!insertmacro\\>[ \t]+\\([A-Za-z][A-Za-z0-9_]*\\)" 1)
          (list "User Variables"
                "^[ \t]*\\<Var\\>[ \t]+\\(?:/GLOBAL[ \t]+\\)?\\<\\([A-Za-z][A-Za-z0-9_]*\\)\\>" 1)
          (list "User Constants"
                "^[ \t]*\\<!define\\>[ \t]+\\<\\([A-Za-z][A-Za-z0-9_]*\\)\\>" 1)
          (list "Labels"
                "^[ \t]*\\([^-+!$0-9\n \t;#][^\n\t ]*?\\):[ \t]*\\(?:$\\|[#;]\\)" 1)
          )))
                                        ;  "* Imenu list"
      )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nsis mode map
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar nsis-mode-map nil
  "Keymap used in `nsis-mode' buffers.")
(if (and nil nsis-mode-map)
    nil
  (setq nsis-mode-map (make-sparse-keymap))
  ;; electric keys
  ;; indentation level modifiers
  ;; subprocess commands
  (define-key nsis-mode-map (kbd "C-c C-e") 'nsis-execute-buffer) 
  (define-key nsis-mode-map (kbd "C-c C-r") 'nsis-run-file)
  (define-key nsis-mode-map (kbd "C-c C-c") 'nsis-compile-and-run)
  (define-key nsis-mode-map "\177" 'backward-delete-char-untabify)
  ;;  (define-key nsi-mode-map [C-f9] 'nsis-execute-buffer)
  )
(defvar nsis-menu nil
  "Menu for Nsi Mode.
This menu will get created automatically if you have the `easymenu'
package.")

(require 'easymenu nil t)
(require 'w32-browser nil t)
(when (featurep 'easymenu)
  (easy-menu-define nsis-menu
    nsis-mode-map "nsis Mode Menu"
    `(
      "NSIS"
      ("Help"
       ["nsis Menu" nsis-nsis-menu]
       ["nsis User Manual (Requires w32-browser.el)" nsis-nsis-user-manual :active (featurep 'w32-browser)]
       "--"
       ["nsis Site" nsis-nsis-website]
       ["nsis forum" nsis-nsis-forum]
       "--"
       ,(concat "nsis Version " nsis-version)
       )
      ["Generate Yasnippet Code Templates" nsis-yas]
      "--"
      ["Compile Buffer & Run" nsis-compile-and-run t]
      ["Compile Buffer" nsis-execute-buffer t]
      ["Run" nsis-run-file t]
      )
    )
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet support
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nsis-yas-desc (secn descn)
  "Yasnippet check for description update.  For use with `nsis-yas-description'"
  (unless (or yas/moving-away-p yas/modified-p)
    (let (
          (sec (nsis-yas-sec (yas/field-value secn)))
          (desc (yas/field-value descn))
          )
      (run-with-timer 0.25 nil `(lambda() (interactive) (nsis-yas-description ,sec ,desc)))
      ""
      )
    ))
(defun nsis-yas-description (sec desc)
  "Expands a Yasnippet of descriptions after sec and seg have been expanded."
  (let (found)
    (yas/exit-all-snippets)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward "^[ \t]*LangString" nil t)
        (setq found t)
        )
      (if found
          (end-of-line)
        (insert "\n;--------------------------------\n;Description(s)"))
      (insert (format "\nLangString DESC_sec_%s ${LANG_ENGLISH} \"%s\"" sec desc))
      (goto-char (point-max))
      (setq found nil)
      (when (re-search-backward "^[ \t]*!insertmacro[ \t]+MUI_FUNCTION_DESCRIPTION_BEGIN[ \t]*+" nil t)
        (setq found t)
        (goto-char (match-end 0)))
      (unless found
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^[ \t]*!insertmacro[ \t]+MUI_DESCRIPTION_TEXT" nil t)
            (end-of-line)
            (setq found t))))
      (unless found
        (insert "\n!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN")
        )
      (insert (format "\n  !insertmacro MUI_DESCRIPTION_TEXT ${sec_%s} $(DESC_sec_%s)" sec sec) )
      (unless found
        (insert "\n!insertmacro MUI_FUNCTION_DESCRIPTION_END")))))

(defun nsis-yas-q ( &optional txt)
  "Yasnippet quote transform"
  (let ((ret yas/text))
    (if (not (and ret (or (< 0 (length ret)) (string-match "^[ \t]$" ret))))
        (setq ret "")
      (setq ret (or txt"\"")))))

(defun nsis-yas-sec (&optional txt)
  "Yasnippet section transform"
  (let ((ret (or txt yas/text)))
    (while (string-match "[^A-Za-z0-9_]" ret)
      (setq ret (replace-match "_" nil t ret))
      )
    (while (string-match "__+" ret)
      (setq ret (replace-match "_" nil t ret)))
    (while (string-match "^_" ret)
      (setq ret (replace-match "" nil t ret)))
    (while (string-match "_$" ret)
      (setq ret (replace-match "" nil t ret)))
    (setq ret (downcase ret))
    (symbol-value 'ret)))

(defun nsis-yas-hidden-bold ()
  "Yasnippet section transform"
  (let ((ret yas/text))
    (if (not (and ret (< 0 (length ret))))
        (setq ret "")
      (setq ret (substring ret 0 1))
      (cond
       ((string= ret "!")
        (setq ret ", Bold")
        )
       ((string= ret "-")
        (setq ret ", Hidden")
        )
       (t
        (setq ret ""))))
    (symbol-value 'ret)))
(defun yas-munge-callback-key-1 (val)
  "Create first key based on callback"
  (let ((x val))
    (when (string-match "^\\(un.on\\)" x)
      (setq x (replace-match "u" nil t x)))
    (when (string-match "on" x)
      (setq x (replace-match "" nil t x)))
    (while (string-match "[.]" x)
      (setq x (replace-match "" nil t x)))
    (setq x (downcase x))
    (symbol-value 'x)
    )
  )
(defun yas-munge-callback-name (val)
  "Changes Callback names to human-readable names"
  (let ((x val)
        (start 0)
        (uninstall "")
        case-fold-search)
    (while (string-match "Init\\([A-Z]\\|$\\)" x)
      (setq x (replace-match "Initialization\\1" t nil x)))
    (while (string-match "Inst\\([A-Z]\\|$\\)" x)
      (setq x (replace-match "Install\\1" t nil x)))
    (while (string-match "Sel\\([A-Z]\\|$\\)" x)
      (setq x (replace-match "Selection\\1" t nil x)))
    (while (string-match "^\\(un\\)?.on" x)
      (setq x (replace-match "On" t nil x)))
    ;; Add Spaces
    (while (string-match "\\([A-Z]\\)" x start)
      (setq start (+ 1 (match-end 1)))
      (setq x (replace-match " \\1" t nil x)))
    (when (string-match "G U I" x)
      (setq x (replace-match "GUI" t t x)))
    (setq x (concat x uninstall))
    (symbol-value 'x)))

;; Left off at Section Management

(setq nsis-yas-snippets
      '(("General Attributes"
         ( 
          "AddBrandingImage ${1:$$(yas/choose-value '(\"left\" \"right\" \"top\" \"bottom\"))} ${2:width/height} ${3:padding}"
          "AllowRootDirInstall ${1:$$(yas/choose-value '(\"true\" \"false\"))}"
          "AutoCloseWindow ${1:$$(yas/choose-value '(\"true\" \"false\"))}"
          "BGFont \"${1:Times New Roman}\"${2: ${3:12} ${4:/ITALIC} ${5:/UNDERLINE} ${6:/STRIKE}}"
          ;;                           BGGradient [off|(topc botc [textcolor|notext])]
          "BrandingText${1: /TRIM${2:$$(yas/choose-value '(\"LEFT\" \"RIGHT\" \"CENTER\"))}} \"$3\""
          "Caption \"$1\""
          ;; ChangeUI
          "CheckBitmap \"$1.bmp\""
          "CompletedText \"$1\""
          "ComponentText \"${1:Text above Controls}\" ${2:$(nsis-yas-q)}${2:Text next to installation type$(yas/ma \"\")}${2:$(nsis-yas-q)}"
          "CRCCheck ${1:$$(yas/choose-value '(\"on\" \"off\" \"force\"))}"
          "DetailsButtonText \"$1\""
          "DirText \"${1:text}\"${2: \"${3:subtext}\"${4: \"${5:browse_button_text}\"${6: \"$7\"}}}"
          ;;DirVar
          "DirVerify ${1:$$(yas/choose-value '(\"auto\" \"leave\"))}"
          "FileErrorText \"$1\""
          "Icon $1"
          "InstallButtonText \"$1\""
          ;;InstallColors /windows | (foreground_color background_color)
          "InstallDir \"$1\""
          "InstallDirRegKey HKLM \"Software\\\\$1\" \"${2:KeyName}\""
          "InstProgressFlags ${1:$$(yas/choose-value '(\"smooth\" \"colored\" \"smooth colored\"))}"
          "InstType \"$1\"${2: /NOCUSTOM}${3: /CUSTOMSTRING=\"${4:str}\"}${5: /COMPONENTSONLYONCUSTOM}"
          ;; LicenseBkColor color | /gray | /windows
          "LicenseData \"${1:licdata.(txt|rtf)}\""
          ;;LicenseForceSelection (checkbox [accept_text] | radiobuttons [accept_text] [decline_text] | off)
          "LicenseText \"${1:text}\"${2: \"${3:button_text}\"}"
          "Name \"${1:name_doubled_ampersands}\""
          "OutFile \"${1:install.exe}\""
          "RequestExecutionLevel ${1:$$(yas/choose-value '(\"none\" \"user\" \"highest\" \"admin\"))}"
          "SetFont${1: /LANG=${2:LANGUAGE_ID}} \"${3:font_face_name}\" ${4:font_size}"
          "ShowInstDetails ${1:$$(yas/choose-value '(\"hide\" \"show\" \"nevershow\"))}"
          "ShowUninstDetails ${1:$$(yas/choose-value '(\"hide\" \"show\" \"nevershow\"))}"
          "SilentInstall ${1:$$(yas/choose-value '(\"normal\" \"silent\" \"silentlog\"))}"
          "SilentUnInstall ${1:$$(yas/choose-value '(\"normal\" \"silent\"))}"
          "SpaceTexts \"${1:Space Required Test}\" ${2:$(nsis-yas-q)}${2:Space Available Text$(yas/ma \"\")}${2:$(nsis-yas-q)}"
          "SubCaption ${1:$(cond ((string= text \"License Agreement\") \"0\") ((string= text \"Installation Options\") \"1\") ((string= text \"Installation Directory\") \"2\")  ((string= text \"Installing Files\") \"3\")  ((string= text \"Completed\") \"4\") (t \"\"))} \"${2:subcaption}\" ; Changing ${1:$$(yas/choose-value '(\"License Agreement\" \"Installation Options\" \"Installation Directory\" \"Installing Files\" \"Completed\"))} Text"
          "UninstallButtonText \"${1:Text}\""
          "UninstallCaption \"${1:caption}\""
          "UninstallSubCaption ${1:$(cond ((string= text \"Confirmation\") \"0\") ((string= text \"Uninstalling Files\") \"1\") ((string= text Completed\") \"2\") (t \"\"))} \"${2:subcaption}\" ; Changing ${1:$$(yas/choose-value '(\"Confirmation\" \"Uninstalling Files\" \"Completed\"))} Text"
          "UninstallText \"${1:text}\" ${2:$(nsis-yas-q)}${2:subtext$(yas/ma \"\")}${2:$(nsis-yas-q)}"
          "WindowIcon ${1:$$(yas/choose-value '(\"on\" \"off\"))}"
          "XPStyle ${1:$$(yas/choose-value '(\"on\" \"off\"))}"
          ))
        ("Compiler Flags"
         (
          "AllowSkipFiles ${1:$$(yas/choose-value '(\"on\" \"off\"))}"
          "FileBufSize ${1:buffer_size_in_mb}"
          "SetCompress ${1:$$(yas/choose-value '(\"auto\" \"force\" \"off\"))}"
          "SetCompressor${1: /SOLID}${2: /FINAL}${3:$$(yas/choose-value '(\"zlib\" \"bzip2\" \"lzma\"))}"
          "SetCompressorDictSize ${1:dict_size_mb}"
          "SetDatablockOptimize ${1:$$(yas/choose-value '(\"on\" \"off\"))}"
          "SetDateSave ${1:$$(yas/choose-value '(\"on\" \"off\"))}"
          "SetOverwrite ${1:$$(yas/choose-value '(\"on\" \"off\" \"try\" \"ifnewer\" \"ifdiff\" \"lastused\"))}"
          ))
        ("Version Information"
         (
          "VIAddVersionKey${1: /LANG=${2:lang_id}} \"${3:$$(yas/choose-value '(\"ProductName\" \"Comments\" \"CompanyName\" \"LegalCopyright\" \"FileDescription\" \"FileVersion\" \"ProductVersion\" \"InternalName\" \"LegalTrademarks\" \"OriginalFilename\" \"PrivateBuild\" \"SpecialBuild\"))}\" \"${4:value}\""
          "VIProductVersion \"${1:`(format-time-string \"%h\")`}.${2:`(format-time-string \"%d\")`}.${3:`(format-time-string \"%m\")`}.{4:`(format-time-string \"%Y\")`}\""
          ))
        ("Basic Instructions"
         (
          "Delete ${1: /REBOOTOK} \"${2:file}\""
          "Exec '${1:command}'"
          "ExecShell \"${1:action}\" \"${1:command}\" ${2:$(nsis-yas-q)}${2:parameters$(yas/ma \"\")}${2:$(nsis-yas-q)} ${3:$$(yas/choose-value '(\"\" \"SW_SHOWDEFAULT\" \"SW_SHOWNORMAL\" \"SW_SHOWMAXIMIZED\" \"SW_SHOWMINIMIZED\" \"SW_HIDE\"))}"
          "ExpandEnvStrings \\$${1:user_var(output)} \"${2:string}\" ; Env strings coded %ENV%"
          "ReadEnvStr \\$${1:user_var(output)} ${2:environemnt name}"
          "SearchPath \\$${1:user_var(output)} '${2:filename}' ; Look for file in search %PATH%"
          
          ))
        ("File Instructions"
         (
          "File${1: /nonfatal}${2: /a}${3: /r}${4: /x '${5:excluded file or wildcard}'} ${6: /oname=${7:file.dat}} '${8:infile.dat}'"
          "Rename${1: /REBOOTOK} '${2:source_file}' '${3:dest_file}'"
          "ReserveFile${1: /nonfatal}${2: /r}${3: /x '${4:excluded file or wildcard}'} '${7:file}'"
          "RMDir${1: /r}${2: /REBOOTOK} '${3:directory_name}'"
          "SetOutPath '${1:outpath}'"
          "CopyFiles${1: /SILENT}${2: /FILESONLY} '${3:From}' '${4:To}'"
          "CreateDirectory '${3:Dir}'"
          "CreateShortCut '${1:Link Name}.lnk' '${2:target.file}'"
          "GetFileTime '${1:filename}' \\$${1:user_var(high dword output)} \\$${2:user_var(low dword output)}"
          "GetFileTimeLocal '${1:filename}' \\$${1:user_var(high dword output)} \\$${2:user_var(low dword output)}"
          "GetFullPathName${1: /SHORT} \\$${2:user_var(output)} '${3:path_or_file}"
          "GetTempFileName \\$${1:user_var(output)} ${2:base_dir}"
          "SetFileAttributes '${1:filename}' ${2:$$(yas/choose-value '(\"NORMAL\" \"ARCHIVE\" \"HIDDEN\" \"OFFLINE\" \"READONLY\" \"SYSTEM\" \"TEMPORARY\"))}"
          "FileClose \\$${1:handle}"
          "FileOpen \\$${1:handle} '${2:file}' ${3:$$(yas/choose-value '(\"r; open read-only\" \"w ; overwrite\" \"a; append or read\"))}"
          "FileRead \\$${1:handle} \\$${2:user_var(output)}"
          "FileReadByte \\$${1:handle}"
          "FileSeek \\$${1:handle} ${2:offset} ${3:$$(yas/choose-value '(\"SET\" \"CUR\" \"END\"))} ${4:$(nsis-yas-q \"$\")}${4:new position$(yas/ma \"\")}"
          "FileWrite \\$${1:handle} \"${2:string}\""
          "FileWriteByte \\$${1:handle} \"${2:Integer of Byte}\""
          "FindFirst \\$${1:user_var(handle output)} \$${2:user_var(filename output)} '${3:filespec}'"
          "FindNext \\$${1:handle} \\$${2:user_var(filename_output)}"
          "FindClose \\$${1:handle}"
          
          ))
        ("Ini Instructions"
         (
          "DeleteINISec '${1:ini_filename}' ${2:section_name}"
          "DeleteINIStr '${1:ini_filename}' ${2:section_name} ${3:entry_name}"
          "FlushINI '${1:ini_filename}'"
          "ReadINIStr '${1:ini_filename}' ${2:section_name} ${3:entry_name}"
          "WriteINIStr '${1:ini_filename}' ${2:section_name} ${3:entry_name} ${4:value}"
          ))
        ("Reistry Instructions"
         (
          "DeleteRegKey${1: /ifempty} ${2:HKLM} \"${3:Software\\\\${4:Key}}\""
          ;;EnumRegValue
          ;; ReadRegDWORD
          "ReadRegStr \\$${1:user_var(output)} ${2:HKLM} \"${3:Software\\\\${4:key}}\" ${5:name}"
          ;;WriteRegBin
          ;; WriteRegDWORD
          "WriteRegExpandStr ${1:HKLM} \"${2:Software\\\\${3:Key}}\" \"${4:Expand String Value}\" \"${5:%WINDIR%\notepad.exe}\""
          ))
        ("Flow Control Instructions"
         (
          "Call ${1:function_name, :label_name or user_var(input)}"
          ;; "ClearErrors"
          ;; GetCurrentAddress, GetFunctionAddress GetLabelAddress
          "Goto ${1:label +-# or Variable}"
          "IfAbort ${1:label_to_goto_if_abort} ${2:label_to_goto_if_no_abort}"
          "IfErrors ${1:jumpto_iferror} ${2:jumpto_ifnoerror}"
          "IfFileExists ${1:file_to_check_for} ${2:jump_if_present} ${3:jump_otherwise}"
          "IfRebootFlag ${1:jump_if_set} ${2:jump_if_not_set}"
          "IfSilent ${1:jump_if_silent} ${2:jump_if_not}"
          "IntCmp ${1:val1} ${2:val2} ${3:jump_if_equal}${4: ${5:jump_if_val1_less} ${6:jump_if_val1_more}}"
          "IntCmpU ${1:val1} ${2:val2} ${3:jump_if_equal}${4: ${5:jump_if_val1_less} ${6:jump_if_val1_more}}"
          "StrCmp ${1:val1} ${2:val2} ${3:jump_if_equal} ${4:jump if not equal}"
          "StrCmpS ${1:val1} ${2:val2} ${3:jump_if_equal} ${4:jump if not equal}"
          ;; Return
          ;; Quit
          ;; SetErrors
          ))
        ("Uninstaller Instructions"
         ( "WriteUninstaller '${1:uninstaller}'")
         )
        ("Miscellaneous Instructions"
         (
          "GetErrorLevel \\$${1:user_var(error level output)}"
          "GetInstDirError \\$${1:user_var(error level output)}"
          ;;InitPluginsDir, Nop,
          "SetErrorLevel ${1:error_level}"
          "SetShellVarContext ${1:$$(yas/choose-value '(\"current\" \"all\"))}"
          "Sleep ${1:sleeptime_in_ms}"
          "SetRebootFlag ${1:$$(yas/choose-value '(\"true\" \"false\"))}"
          "LogSet ${1:$$(yas/choose-value '(\"on\" \"off\"))}"
          "LogText '${1:Log Text}'"
          
          )
         )
        ("String Manipulation Instructions"
         (
          "StrCpy \\$${1:user_var(destination)} ${2:string}"
          "StrLen\ \$${1:user_var(length output)} ${2:string}"
          ))
        ("Stack Support"
         (
          "Exch ${1:user_var or stack_index}"
          "Pop ${1:user_var(out)}"
          "Push ${2:string}"
          ))
        ("Integer Support"
         (
          "IntFmt \\$${1:user_var(output)} ${2:format} ${3:numberstring}"
          "IntOp \\$${1:user_var(output)} ${2:va1} ${3:+} ${2:va2}"
          ))
        ("Useful Scripts"
         (
          "
 ; GetIEVersion
 ;
 ; Based on Yazno's function, http://yazno.tripod.com/powerpimpit/
 ; Returns on top of stack
 ; 1-6 (Installed IE Version)
 ; or
 ; '' (IE is not installed)
 ;
 ; Usage:
 ;   Call GetIEVersion
 ;   Pop $R0
 ;   ; at this point $R0 is \"5\" or whatnot

 Function GetIEVersion
 Push $R0
   ClearErrors
   ReadRegStr $R0 HKLM \"Software\\Microsoft\\Internet Explorer\" \"Version\"
   IfErrors lbl_123 lbl_456

   lbl_456: ; ie 4+
     Strcpy $R0 $R0 1
   Goto lbl_done

   lbl_123: ; older ie version
     ClearErrors
     ReadRegStr $R0 HKLM \"Software\\Microsoft\\Internet Explorer\" \"IVer\"
     IfErrors lbl_error

       StrCpy $R0 $R0 3
       StrCmp $R0 '100' lbl_ie1
       StrCmp $R0 '101' lbl_ie2
       StrCmp $R0 '102' lbl_ie2

       StrCpy $R0 '3' ; default to ie3 if not 100, 101, or 102.
       Goto lbl_done
         lbl_ie1:
           StrCpy $R0 '1'
         Goto lbl_done
         lbl_ie2:
           StrCpy $R0 '2'
         Goto lbl_done
     lbl_error:
       StrCpy $R0 ''
   lbl_done:
   Exch $R0
 FunctionEnd

"
          " ; IsDotNETInstalled
 ;
 ; Based on GetDotNETVersion
 ;   http://nsis.sourceforge.net/Get_.NET_Version
 ;
 ; Usage:
 ;   Call IsDotNETInstalled
 ;   Pop $0
 ;   StrCmp $0 1 found.NETFramework no.NETFramework

 Function IsDotNETInstalled
   Push $0
   Push $1

   StrCpy $0 1
   System::Call \"mscoree::GetCORVersion(w, i ${nsis_MAX_STRLEN}, *i) i .r1\"
   StrCmp $1 0 +2
     StrCpy $0 0

   Pop $1
   Exch $0
 FunctionEnd
"
          " ; IsFlashInstalled
 ;
 ; By Yazno, http://yazno.tripod.com/powerpimpit/
 ; Returns on top of stack
 ; 0 (Flash is not installed)
 ; or
 ; 1 (Flash is installed)
 ;
 ; Usage:
 ;   Call IsFlashInstalled
 ;   Pop $R0
 ;   ; $R0 at this point is \"1\" or \"0\"

 Function IsFlashInstalled
  Push $R0
  ClearErrors
  ReadRegStr $R0 HKCR \"CLSID\{D27CDB6E-AE6D-11cf-96B8-444553540000}\" \"\"
  IfErrors lbl_na
    StrCpy $R0 1
  Goto lbl_end
  lbl_na:
    StrCpy $R0 0
  lbl_end:
  Exch $R0
 FunctionEnd
"
          " ; ConnectInternet (uses Dialer plug-in)
 ; Written by Joost Verburg 
 ;
 ; This function attempts to make a connection to the internet if there is no
 ; connection available. If you are not sure that a system using the installer
 ; has an active internet connection, call this function before downloading
 ; files with nsisdl.
 ; 
 ; The function requires Internet Explorer 3, but asks to connect manually if
 ; IE3 is not installed.
 
 Function ConnectInternet
 
   Push $R0
     
     ClearErrors
     Dialer::AttemptConnect
     IfErrors noie3
     
     Pop $R0
     StrCmp $R0 \"online\" connected
       MessageBox MB_OK|MB_ICONSTOP \"Cannot connect to the internet.\"
       Quit ;This will quit the installer. You might want to add your own error handling.
     
     noie3:
   
     ; IE3 not installed
     MessageBox MB_OK|MB_ICONINFORMATION \"Please connect to the internet now.\"
     
     connected:
   
   Pop $R0
   
 FunctionEnd
" ";GetInstallerFilename
System::Call 'kernel32::GetModuleFileNameA(i 0, t .R0, i 1024) i r1'
 ;$R0 will contain the installer filename
"
"; PreventMultipleInstances, Should be in the .onInit function:


 System::Call 'kernel32::CreateMutexA(i 0, i 0, t \"${1:Text}\") i .r1 ?e'
 Pop $R0
 
 StrCmp $R0 0 +3
   MessageBox MB_OK|MB_ICONEXCLAMATION \"${2:The installer is already running.}\"
   Abort
"
))
        )
                                        ;"nsis Yas snippets that are auto-generated."
      )

(defun nsis-yas ()
  "Generates some Yasnippet templates and puts them in the appropriate directory"
  (interactive)
  (require 'yasnippet nil t)
  (when (boundp 'yas/root-directory)
    (let (
          (new-dir (if (eq (type-of 'yas/root-directory) 'symbol)
                       yas/root-directory
                     (nth 0 yas/root-directory)
                     ))
          (debug-on-error t)
          added-snippets
          file
          atfile
          snippet-list
          )
      (setq new-dir (concat new-dir "nsis-mode/"))
      (unless (file-exists-p new-dir)
        (make-directory new-dir 't)
        )
      (with-temp-file (concat new-dir "section.yasnippet")
        (insert "# -*- mode: snippet -*-\n# name: Section ... SectionEnd\n# key: section\n# key: sec\n# contributor: Matthew L. Fidler\n# --\nSection ${1:/o}${1:$(if (string= \"/o\" text) \" \" \"\")}\"$2\" sec_${2:$(nsis-yas-sec)} ${1:$(if (string= \"/o\" text) \"; Unchecked (/o)\" \"; Checked\")}${2:$(nsis-yas-hidden-bold)}\n  ; Description:\n  ; $3\n  ${4:$$(nsis-yas-desc 2 3)}$0\nSectionEnd ; sec_${2:$(nsis-yas-sec)}"))
      
      (with-temp-file (concat new-dir "sectiongroup.yasnippet")
        (insert "# -*- mode: Snippet -*-\n# name: SectionGroup ... SectionGroupEnd\n# key: sectiongroup\n# key: secg\n# contributor: Matthew L. Fidler\n# --\nSectionGroup ${1:/e}${1:$(if (string= \"/e\" text) \" \" \"\")}\"$2\" sec_${2:$(nsis-yas-sec)} ${1:$(if (string= \"/3\" text) \"; Expanded (/e)\" \"; Collapsed\")}${2:$(nsis-yas-hidden-bold)}\n  ; Description:\n  ; $3\n  ${4:$$(nsis-yas-desc 2 3)}$0\nSectionGroupEnd ; sec_${2:$(nsis-yas-sec)}"))
      (with-temp-file (concat new-dir "function.nsisppet")
        (insert "# -*- mode: snippet -*-\n# name: Function ... FunctionEnd\n# key: function\n# key: fn\n# contributor: Matthew L. Fidler\n# --\nFunction $1\n  $0\nFunctionEnd"))
      (mapc
       (lambda(x)
         (with-temp-file (concat new-dir "callback" (replace-regexp-in-string "[^A-Za-z0-9_]" "_" x) ".yasnippet")
           (insert "# -*- mode: Snippet -*-\n# name: ")
           (insert (yas-munge-callback-name x))
           (insert "\n# key: ")
           (insert (yas-munge-callback-key-1 x))
           (if (string-match "^un." x)
               (insert "\n# group: Install Callback Functions")
             (insert "\n# group: Un-install Callback Functions"))
           (insert "\n# contributor: Matthew L. Fidler\n# --\nFunction ")
           (insert x)
           (insert " ;")
           (insert (yas-munge-callback-name x))
           (insert "\n  $0\nFunctionEnd")))
       nsis-syntax-callback)
      (mapc
       (lambda(x)
         (let ((group (nth 0 x))
               (lst (nth 1 x)))
           (mapc
            (lambda(y)
              (let ((snippet y) key)
                (when (string-match "^[ \t;]*\\(\\w+\\)" snippet)
                  (setq key (match-string 1 snippet)))
                (message "Creating %s snippet" key)
                (with-temp-file (concat new-dir (replace-regexp-in-string "[^A-Za-z0-9_]" "_" key) ".yasnippet")
                  (insert "# -*- mode: Snippet -*-\n# name: ")
                  (insert key)
                  (insert "\n# key: ")
                  (insert key)
                  (insert "\n# group: ")
                  (insert group)
                  (insert "\n# contributor: Matthew L. Fidler\n# --\n")
                  (insert snippet))))
            lst)))
       nsis-yas-snippets)
      (message "Done")
      ))
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nsis helpers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nsis-nsis-menu ()
  "Open nsis menu"
  (interactive)
  (shell-command (w32-short-file-name nsis-nsis-menu-file)))

(defun nsis-nsis-user-manual ()
  "Open nsis Help (require w32-browser)"
  (interactive)
  (w32-browser nsis-nsis-manual-file))

(defun nsis-nsis-website ()
  "Goto nsis website"
  (interactive)
  (browse-url "http://nsis.sourceforge.net/"))

(defun nsis-nsis-forum ()
  "Goto nsis forum"
  (interactive)
  (browse-url "http://forums.winamp.com/forumdisplay.php?s=&forumid=65"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nsis compile functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup nsis-mode nil
  "nsis -- Yet another NSI-editing mode, www.nsi.org"
  :group 'languages)

(defcustom nsis-nsis-manual-file
  (let ((nsis (concat (getenv "ProgramFiles")
                      "/nsis/nsis.chm")))
    (if (file-exists-p nsis)
        nsis
      "nsis.chm"))
  "nsis help file"
  :type 'file
  :group 'nsis-mode)

(defcustom nsis-nsis-menu-file
  (let ((nsis (concat (getenv "ProgramFiles")
                      "/nsis/nsis.exe")))
    (if (file-exists-p nsis)
        nsis
      "nsis"))
  "nsiss Menu File"
  :type 'file
  :group 'nsis-mode)

(defcustom nsis-makensis-windows-command
  (let ((nsis (concat (getenv "ProgramFiles")
                      "/nsis/makensisw.exe")))
    (if (file-exists-p nsis)
        nsis
      "makensisw"))
  "nsis-mode windows command"
  :type 'file
  :group 'nsis-mode)

(defcustom nsis-makensis-command
  (let ((nsis (concat (getenv "ProgramFiles")
                      "/nsis/makensis.exe")))
    (if (file-exists-p nsis)
        nsis
      "makensisw"))
  "nsis-mode command prompt"
  :type 'file
  :group 'nsis-mode)
(defcustom nsis-run-nsis-async t
  "nsis-mode run Nsis asynchronously"
  :type 'boolean
  :group 'nsis-mode
  )

(defun nsis-execute-buffer (&optional async)
  "Send the contents of the buffer to a Nsi interpreter."
  (interactive)
  (let ((nsi-file-name (buffer-file-name)))
    (when nsi-file-name
      (save-buffer)
      (if nsis-run-nsis-async
          (nsis-async-execute-file nsi-file-name)
        (shell-command (concat nsis-makensis-windows-command " " nsi-file-name))))))

(defun nsis-compile-and-run ()
  "Send the contents of the buffer to a Nsi interpreter, and then run the NSI output."
  (interactive)
  (let ((nsi-file-name (buffer-file-name)))
    (when nsi-file-name
      (save-buffer)
      (nsis-async-execute-file nsi-file-name t))))

(defvar nsis-run-file-name nil
  "Constant for saving the file executed after compile")

(defun nsis-async-execute-file (file &optional run)
  "Compiles the nsis file asynchronously"
  (when run
    (setq nsis-run-file-name (nsis-get-output-file)))
  (get-buffer-create "*nsis*")
  (switch-to-buffer-other-window "*nsis*")
  (insert "================================================================================\n\"")
  (insert (w32-short-file-name nsis-makensis-command))
  (insert "\" \"")
  (insert (w32-short-file-name file))
  (insert "\"\n")
  (insert "\n================================================================================\n")
  (apply 'start-process-shell-command "*nsis*"
         "*nsis*"
         (w32-short-file-name nsis-makensis-command)
         (list (w32-short-file-name file)))
  (if run
      (set-process-sentinel (get-buffer-process (current-buffer))
                            #'nsis-finish-compile-run)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          #'nsis-finish-compile)))

(defun nsis-get-output-file ()
  (when (buffer-file-name)
    (let ((dir (file-name-directory (buffer-file-name)))
          outfile)
      (save-excursion
        (goto-char (point-min))
        (when (or (re-search-forward "^[ \t]*OutFile[ \t]*\"\\(.*?\\)\"" nil t)
                  (re-search-forward "^[ \t]*OutFile[ \t]*\\(\\[^ \t]+\\)" nil t))
          (setq outfile (w32-short-file-name (concat dir "/" (match-string 1))))))
      (symbol-value 'outfile))))

(defun nsis-run-file (&optional out-file)
  "Runs output file, possibly compiling if necessary."
  (interactive)
  (let ((out (or out-file (nsis-get-output-file))))
    (when (file-exists-p out)
      (start-process "*nsis*" "*nsis*"
                     out))))

(defun nsis-finish-compile-run (&rest ignore)
  "Finished Nsi Compilation, run output"
  (save-excursion
    (set-buffer "*nsis*")
    (forward-line -1)
    (unless (looking-at ".*abort.*")
      (goto-char (point-max))
      (insert "================================================================================\n")
      (insert "Finished Compilation, will run.\n")
      (insert "================================================================================\n")
      (nsis-run-file nsis-run-file-name))))

(defun nsis-finish-compile (&rest ignore)
  "Finished Nsi Compilation"
  (save-excursion
    (set-buffer "*nsis*")
    (goto-char (point-max))
    (insert "================================================================================\n")
    (insert "Finished Compilation\n")
    (insert "================================================================================\n")
    )
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nsis syntax table
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nsis-mode-syntax-table nil
  "Syntax table used in `nsis-mode' buffers.")
(if nsis-mode-syntax-table
    nil
  (setq nsis-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" nsis-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" nsis-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" nsis-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" nsis-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" nsis-mode-syntax-table)
  (modify-syntax-entry ?\} "){" nsis-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\% "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\& "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\* "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\- "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\< "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\= "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\> "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?\| "."  nsis-mode-syntax-table)
  (modify-syntax-entry ?. "."  nsis-mode-syntax-table)
  ;; For historical reasons, underscore is word class instead of
  ;; symbol class.  GNU conventions say it should be symbol class, but
  ;; there's a natural conflict between what major mode authors want
  ;; and what users expect from `forward-word' and `backward-word'.
  ;; Guido and I have hashed this out and have decided to keep
  ;; underscore in word class.  If you're tempted to change it, try
  ;; binding M-f and M-b to nsis-forward-into-nomenclature and
  ;; nsis-backward-into-nomenclature instead.  This doesn't help in all
  ;; situations where you'd want the different behavior
  ;; (e.g. backward-kill-word).
  (modify-syntax-entry ?\_ "w"  nsis-mode-syntax-table)
  (modify-syntax-entry ?! "w" nsis-mode-syntax-table)
  ;;(modify-syntax-entry ?. "w" nsis-mode-syntax-table)
  ;;(modify-syntax-entry ?- "w" nsis-mode-syntax-table)
  (modify-syntax-entry ?\\ "." nsis-mode-syntax-table) ; No quoting possible without $\
  (modify-syntax-entry ?/ ".")
  ;; Single quote and double quote and back-quote are string delimiters
  (modify-syntax-entry ?\' "\"" nsis-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" nsis-mode-syntax-table)
  (modify-syntax-entry ?\` "\""  nsis-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\# "<"  nsis-mode-syntax-table)
  (modify-syntax-entry ?\; "<"  nsis-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  nsis-mode-syntax-table)
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indention function
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq nsis-end-keywords
      (eval-when-compile
        (replace-regexp-in-string "@" "\\<!insertmacro[ \t]+.*?END[ \t]*$"
                                  (replace-regexp-in-string "'" "\\>"
                                                            (replace-regexp-in-string "`" "\\<"
                                                                                      (regexp-opt (append
                                                                                                   (mapcar (lambda(x)
                                                                                                             (concat "`" x "'")
                                                                                                             )
                                                                                                           (remove-if-not (lambda(x) (string-match "End$" x))
                                                                                                                          nsis-syntax-reserved-word))
                                                                                                   '("`!endif'" "`!macroend'" "${EndIf}"
                                                                                                     "${EndSelect}" "${LoopWhile}"
                                                                                                     "${LoopUntil}" "${EndUnless}"
                                                                                                     "${Loop}"
                                                                                                     "@"
                                                                                                     "${EndWhile}"
                                                                                                     "${Next}"
                                                                                                     "${EndSwitch}")) t) nil t) nil t) nil t))
      ;;"* Regular expression of nsis ending keywords"
      )
(setq nsis-start-keywords
      (eval-when-compile
        (replace-regexp-in-string "@" "\\<!insertmacro[ \t]+.*?BEGIN[ \t]*$"
                                  (replace-regexp-in-string "'" "\\>"
                                                            (replace-regexp-in-string "`" "\\<"
                                                                                      (regexp-opt
                                                                                       
                                                                                       (append (mapcar (lambda(x) (concat "`" (substring x 0 -3) "'"))
                                                                                                       (remove-if-not (lambda(x) (string-match "End$" x))
                                                                                                                      nsis-syntax-reserved-word))
                                                                                               '("`!ifdef'" "`!ifndef'"
                                                                                                 "`!ifmacrodef'"
                                                                                                 "`!ifmacrondef'"
                                                                                                 "`!macro'"
                                                                                                 "@"
                                                                                                 "${If}" "${IfNot}" "${Unless}"
                                                                                                 "${Select}" "${Switch}"
                                                                                                 "${Do}" "${DoWhile}"
                                                                                                 "${DoUntil}" "${While}"
                                                                                                 "${For}" "${ForEach}"
                                                                                                 ;;                                                                                                 "${RecFindOpen}" "${RecFindFirst}"
                                                                                                 
                                                                                                 )) t) nil t) nil t) nil t))
      ;;"* Regular expression of nsis beginning keywords"
      )
(setq nsis-indent-deindent-keywords
      (eval-when-compile
        (replace-regexp-in-string "@" "^[ \t]*[^-+!$0-9\n \t;#][^ \t\n]*?:[ \t]*\\($\\|[#;]\\|/[*].*?[*]/[ \t]*$\\|/[*].*?$\\)"
                                  (regexp-opt
                                   '( "@"
                                      "${AndIf}" "${AndIfNot}" "${AndUnless}" "${OrIf}"
                                      "${OrIfNot}" "${OrUnless}" "${ElseIf}" "${ElseIfNot}"
                                      "${ElseUnless}" "${Else}" "${CaseElse}" "${Default}" "${Case2}"
                                      "${Case3}" "${Case4}" "${Case5}" "${Case}"
                                      ;;                                      "${RecFindNext}" "${RecFindClose}"
                                      ) t) nil t))
                                        ;"Regular expression of indent-deindent keywords statements for indent-deindent-keywords "
      )

(setq nsis-indent-orphans
      (eval-when-compile
        (replace-regexp-in-string "@" "^[ \t]*[^-+!$0-9\n \t;#][^ \t\n]*?:[ \t]*\\($\\|[#;]\\|/[*].*?[*]/[ \t]*$\\|/[*].*?$\\)"
                                  (regexp-opt
                                   '( "@"
                                      "${CaseElse}" "${Default}" "${Case2}"
                                      "${Case3}" "${Case4}" "${Case5}" "${Case}"
                                      "${Case}"
                                      ) t
                                        ) nil t))
                                        ;"Regular expression of indent/deindent expression that are not used to calculate the last indentation level."
      )

(defun nsis-continuation-line-p ()
  "* Is this a nsi continuation line?"
  (save-excursion
    (beginning-of-line)
    (backward-char 1)
    (skip-chars-backward " \t")
    (eq (char-before (point)) ?\\)))

(defun nsis-last-line-indentation (&optional orphan)
  "Last line's indentation"
  (let (ret)
    (save-excursion
      (nsis-goto-last-line 'ret orphan)
      (symbol-value 'ret))))


(defmacro nsis-set-indent-count (count)
  "Subroutine to set indentation key"
  `(cond
    ((looking-at  ,(eval-when-compile (format "[ \t]*%s" nsis-indent-orphans))))
    ((looking-at ,(eval-when-compile (format "[ \t]*%s" nsis-end-keywords)))
     (unless (= ,count 0)
       (setq ,count (+ ,count 1))))
    ((looking-at ,(eval-when-compile (format "[ \t]*%s" nsis-start-keywords)))
     (unless (= ,count 0)
       (setq ,count (- ,count 1))))))

(defun nsis-goto-last-line (&optional li-q orphan)
  "Go to the last line of code -- ignore continuation lines.  Set li-q to the current-indentation when requested."
  (if orphan
      (progn
        (let (count)
          (nsis-goto-last-line li-q)
          (setq count 1)
          (nsis-set-indent-count count)
          (while (and (not (bobp)) (not (= count 0)))
            (nsis-goto-last-line li-q)
            (nsis-set-indent-count count))))
    (while (re-search-backward "^[ \t]*\n\\=" nil t))
    (forward-line -1)
    (beginning-of-line)
    (let ((mlc (nsis-in-multiline-comment-p))) ;; Ignore multi-line comments.
      (while mlc
        (goto-char mlc)
        (beginning-of-line)
        (setq mlc (nsis-in-multiline-comment-p))
        ))
    (while (re-search-backward "[\\\\][ \t]*\n\\=" nil t) ;; Ignore continuation lines.
      (beginning-of-line)
      )
    (while (looking-at "[ \t]*$")
      (forward-line -1)
      (beginning-of-line))
    (when li-q
      (set li-q (current-indentation)))))

(defun nsis-last-line-indent-line-p (&optional li-q is-id-q orphan-q)
  "* Is the last line an indentation line?"
  (let (ret)
    (save-excursion
      (when (or is-id-q orphan-q)
        (beginning-of-line))
      (when is-id-q
        (set is-id-q (or (looking-at (eval-when-compile (format "[ \t]*%s" nsis-indent-deindent-keywords)))
                         (looking-at (eval-when-compile (format "[ \t]*%s" nsis-end-keywords))))))
      (when orphan-q
        (set orphan-q (looking-at (eval-when-compile (format "[ \t]*%s" nsis-indent-orphans)))))
      (nsis-goto-last-line li-q)
      (setq ret (looking-at (eval-when-compile (format "[ \t]*%s" nsis-start-keywords))))
      (unless ret
        (setq ret (looking-at (eval-when-compile (format "[ \t]*%s" nsis-indent-deindent-keywords))))))
    (symbol-value 'ret)))

(defun nsis-current-line-deindent-p ()
  "Current line a deindent?"
  (save-excursion
    (beginning-of-line)
    (or
     (looking-at (eval-when-compile (format "[ \t]*%s" nsis-end-keywords)))
     (looking-at (eval-when-compile (format "[ \t]*%s" nsis-indent-deindent-keywords))))))

(defun nsis-comment-p ()
  "*Are we in a comment?"
  (save-match-data (or (looking-back "[;#].*") (nsis-in-multiline-comment-p))))
(defun nsis-in-multiline-comment-p ()
  "* Are we in a multi-line comment?"
  (save-match-data (nsis-is-between (regexp-quote "/*") (regexp-quote "*/") t)))

(defun nsis-is-between (first last &optional eof last-q)
  "Returns true of the carat is between the first and last .

If eof is true, then the last position is:
   (1) the position of the variable last
   (2) the position of the next variable first
   (3) the end of the buffer.
If eof is nil, then the last position is:
   (1) the position of the next variable last.
Returns first position.
"
  (let (
        (first-posB nil)
        (last-posF nil)
        (between 't)
        (case-fold-search 't))
    (save-excursion
      (if (re-search-backward first nil t)
          (setq first-posB (point))
        (setq between nil)))
    (if between
        (save-excursion
          (if (re-search-forward last nil t)
              (setq last-posF (point))
            ;; 
            ;; Last not found, look for first.
            ;;
            (if eof
                (progn
                  (if (re-search-forward first nil t)
                      (setq last-posF (point))
                    ;;
                    ;; First not found, set to end of buffer.
                    ;;
                    (setq last-posF (point-max))))
              ;;
              ;; Eof not true.  
              ;;
              (setq between nil)))))
    (if (and (not eof) between)
        (save-excursion
          (if (re-search-forward first nil t)
              (if (< (point) last-posF)
                  (setq between nil)))))
    (if between
        (save-excursion
          (if (re-search-backward last nil t)
              (if (> (point) first-posB)
                  (setq between nil)))))
    (when between
      (setq between first-posB)
      (when last-q
        (set last-q last-posF)
        ))
    between))

(defun nsis-indent-line-function ()
  "nsis indent-line function"
  (interactive)
  (let (
        (curi (current-indentation))
        li
        is-id
        orphan
        fli)
    (save-excursion
      (cond
       ((save-excursion
          (beginning-of-line)
          (bobp)) ;; First line is indented to zero.
        (setq fli 0))
       ((nsis-continuation-line-p) ;; Add 4 spaces to indentation
        ;;                (message "Continuation line.")
        (setq li (nsis-last-line-indentation))
        (unless (= (+ 4 li) curi)
          (setq fli (+ 4 li))))
       ((nsis-in-multiline-comment-p) ;; Check for multi-line comments and strings
        ;; Do nothing.
        ;;(message "Multiline comment -- No indentation support")
        )
       ((nsis-last-line-indent-line-p 'li 'is-id 'orphan) ;; Last line indicates we should indent
        ;;(message "indent: %s,%s,%s" li is-id orphan)
        
        (if (and is-id (not orphan)) ;; Actually indent/deindent line, keep same indentation.
            (unless (= li curi) ;; Change the indentation appropriately.
              (setq fli li))
          (if orphan
              (save-excursion
                (nsis-goto-last-line 'li t)
                (setq li (+ 2 li))
                (unless (= li curi) ;; Last line was an orphan too, keep the same indentation
                  (setq fli li)))
            (unless (= (+ 2 li) curi) ;; Indent
              (setq fli (+ li 2))))))
       ((nsis-current-line-deindent-p) ;; Deindentation is possible
        (setq orphan (string-match (eval-when-compile (format "^[ \t]*%s" nsis-indent-orphans)) (match-string 0)))
        (setq li (nsis-last-line-indentation t))
        (when orphan
          (setq li (+ li 2)))
        (setq li (max 0 li))
        (unless (= li curi)
          (setq fli li))
        ;;(message "deindent: %s,%s,%s,%s" orphan li curi fli)
        )
       (t ;; Keep last indentation
        ;;        (message "Keep Last Indentation")
        (setq li (nsis-last-line-indentation))
        ;;        (message "Keep last indentation: %s" li)
        (unless (= li curi)
          (setq fli li)))))
    (when fli
      (indent-line-to fli)
      (when (looking-at "[ \t]*$")
        (goto-char (match-end 0))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Folding with hideshow
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'hideshow)

;; TODO:  Add labels folding.
(setq nsis-hs-start (concat "^[ \t]*" nsis-start-keywords)
      ;;"* Esn Hide-show start regular expression"
      )

(add-to-list 'hs-special-modes-alist
             (list 'nsis-mode  nsis-hs-start nil "\\(?:[;#]\\|/[*]\\)"
                   (lambda (arg) (nsis-forward-fold)) nil))
(defun nsis-forward-fold (&rest arg)
  "* Used to go to next folded expression in NSIS"
  (interactive)
  (cond
   (
    (looking-at (concat "^[ \t]*" nsis-start-keywords))
    (let (
          (pt (point))
          (i 1)
          m)
      (save-excursion
        (forward-char (length (match-string 0)))
        (re-search-forward (format "^[ \t]*\\(%s\\|%s\\)" nsis-start-keywords nsis-end-keywords) nil t)
        (setq m (match-string 0))
        (while (or (if (not (string-match nsis-start-keywords m))
                       (progn
                         (setq i (- i 1))
                         nil)
                     (setq i (+ i 1))
                     't)
                   (> i 0))
          (if (not (re-search-forward (format "^[ \t]*\\(%s\\|%s\\)" nsis-start-keywords nsis-end-keywords) nil t))
              (setq i -2))
          (setq m (match-string 0)))
        (if (>= i 0)
            (setq pt (point))
          (error "Unbalanced expression")))
      (goto-char pt)))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode declaration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar nsis-mode-hook nil
  "*nsis mode hook")

(defun nsis-mode ()
  "Major mode for editing Nsi files."
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  
  (set (make-local-variable 'font-lock-multiline) t) ; Support multiline comments.
  (set (make-local-variable 'font-lock-defaults)
       '(nsis-font-lock-keywords nil t))
  (set (make-local-variable 'font-lock-syntactic-keywords)
       nsis-font-lock-syntactic-keywords
       )
  (make-local-variable 'font-lock-extend-region-functions)
  (add-hook 'font-lock-extend-region-functions
            'nsis-font-lock-extend-region-continue t)
  (setq imenu-generic-expression nsis-imenu-generic-expression)
  (use-local-map nsis-mode-map)
  (imenu-add-menubar-index)
  ;; (make-local-variable 'paragraph-separate)
  ;; (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  ;; (make-local-variable 'comment-indent-function)
  ;; (make-local-variable 'indent-region-function)
  (set (make-local-variable 'indent-line-function) 'nsis-indent-line-function)
  ;; (make-local-variable 'add-log-current-defun-function)
  ;;
  (set-syntax-table nsis-mode-syntax-table)
  (setq major-mode              'nsis-mode
        mode-name               "NSIS"
        paragraph-separate      "^[ \t]*$"
        paragraph-start         "^[ \t]*$"
        require-final-newline   t
        comment-start           ";"
        comment-end             ""
        comment-start-skip      "\\([;#]+\\|/\\*+\\)\\s *"
        comment-column          40)
  (nsis-construct-faces)
  (run-mode-hooks 'nsis-mode-hook))
(provide 'nsis-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nsis-mode.el ends here
