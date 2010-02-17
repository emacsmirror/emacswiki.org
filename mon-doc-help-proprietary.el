;;; mon-doc-help-proprietary.el --- extends mon-doc-help-utils with MS related docs
;; -*- mode: EMACS-LISP; -*-
;;; ================================================================
;;; DESCRIPTION:
;;; mon-doc-help-proprietary provides extension of mon-doc-help-utils 
;;; facilities for documentation of code that is proprietary or which will
;;; never fall under the scope of GPL or GFDL.
;;;
;;; FUNCTIONS:►►►
;;; `mon-help-w32-shell-execute', `mon-help-w32-cmd-commands',
;;; `mon-bind-doc-help-proprietery-vars-at-loadtime'
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*mon-compromise-my-ms-W32-virginity*', `*mon-help-w32-CMD-commands*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;; NOTES:
;;; Currently the function `mon-help-w32-shell-execute' is wrapped in a
;;; conditional which tests the variable `*mon-compromise-my-ms-W32-virginity*'
;;; is non-nil. You will need to bind that var to non-nil in order to make the
;;; function available in your environment.
;;; 
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;;
;;; THIRD PARTY CODE:
;;; :COPYRIGHT Microsoft Corp.
;;; (URL `http://msdn.microsoft.com/en-us/library/bb762153(VS.85).aspx')
;;; (URL `http://msdn.microsoft.com/en-us/cc300389.aspx#E')
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-doc-help-proprietary.el)
;;; FIRST-PUBLISHED: <Timestamp: #{2010-02-16T18:12:17-05:00Z}#{10072} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-10-19T14:21:08-04:00Z}#{09431} - by MON KEY>
;;; ================================================================
;;; Copyright © 2009, 2010 MON KEY (procedures)
;;; ===========================================
;;; CODE:

;; `mon-bind-doc-help-proprietery-vars-at-loadtime' <- `setf' 
(eval-when-compile (require 'cl)) 

(defvar *mon-compromise-my-ms-W32-virginity* (bound-and-true-p IS-MON-SYSTEM-P)
  "*Set this variable non-nil to make `mon-help-w32-shell-execute' avaiable.\n
If this var remains nil you will not be compromised by potentially proprietary
MS w32 related content.  Think of this kludge as cherry popping EULA. ;)")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-16T13:34:05-05:00Z}#{10072} - by MON KEY>
(defvar *mon-help-w32-CMD-commands* nil
"*Alphabetically keyed list of w32 CMD and CMD Network Commands.\n
The w32 command line interface can be accessed by running the CMD.exe.
This is usually located in the \"system32\" folder in the Windows folder:
 c:/WINDOWS/system32/cmd.exe\n
:SEE (URL `http://www.kapcom.com.au/Windows-CMD-Commands-CMD-Network-Commands.html')\n
:SEE (URL `http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/cmd.mspx?mfr=true')\n
:SEE-ALSO `mon-help-w32-cmd-commands', `mon-help-w32-shell-execute',
`mon-bind-doc-help-proprietery-vars-at-loadtime'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-16T14:21:34-05:00Z}#{10072} - by MON KEY>
(defun mon-help-w32-cmd-commands (cmd-search &optional intrp)
  "Return the action accomplished with the cmd command named CMD-SEARCH.\n
CMD-SEARCH is a string which names a cmd command to search.
When called interactively prompt for a command name to search.
:EXAMPLE\n\n\(mon-help-w32-cmd-commands nil t)\n
\(call-interactively 'mon-help-w32-cmd-commands\)\n
\(mon-help-w32-cmd-commands \"FIND\"\)\n
\(mon-help-w32-cmd-commands \"find\")\n
:SEE (URL `http://www.kapcom.com.au/Windows-CMD-Commands-CMD-Network-Commands.html')
:SEE (URL `http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/cmd.mspx?mfr=true')\n
:SEE-ALSO `mon-bind-doc-help-proprietery-vars-at-loadtime', 
`*mon-help-w32-CMD-commands*' `mon-help-w32-shell-execute'\n.►►►"
(interactive "i\np")
(cond (intrp (message
              (mon-help-w32-cmd-commands
               (completing-read "CMD commands (TAB completes): " 
                                *mon-help-w32-CMD-commands*))))
      (t (gethash (upcase cmd-search) *mon-help-w32-CMD-commands*))))
;;
;;; :TEST-ME (call-interactively 'mon-help-w32-cmd-commands)
;;; :TEST-ME (mon-help-w32-cmd-commands "FIND")
;;; :TEST-ME (mon-help-w32-cmd-commands "find")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-16T14:32:12-05:00Z}#{10072} - by MON KEY>
(defun mon-bind-doc-help-proprietery-vars-at-loadtime (cmd-vars-list &optional 
                                                       bind-new force-bind)
  "Hash key value pairs of sublists CMD-VARS-LIST binding to variable loadtime.\n
Bind `*mon-help-w32-CMD-commands*' to hashtable for `mon-help-w32-cmd-commands'.\n
CMD-VAR-LIST is an alphabetically keyed list containing sublists of conses which
are key value pairs of the form symbol . string e.g.:\n
 '((A (consA . \"consA string\")) (B (consB .\"consB\")) { ... } )\n
When BIND-NES is non-nil rebind the variable `*mon-help-w32-CMD-commands*' with
any new key value pairs in the sublists of CMD-VAR-LIST.\n
When FORCE-BIND is non-nil clear existing hashtable before populating the key
value pairs in the sublists of CMD-VAR-LIST.\n
:SEE-ALSO `mon-bind-cifs-vars-at-loadtime', `mon-set-register-tags-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-CL-cln-colon-swap',
`mon-bind-doc-help-proprietery-vars-at-loadtime'.\n►►►"
  ;; Not doing `clrhash' because it might not be bound.
  (cond ((not (bound-and-true-p *mon-help-w32-CMD-commands*))
         (setq *mon-help-w32-CMD-commands* (make-hash-table :test 'equal :size 217)))
        (force-bind (clrhash *mon-help-w32-CMD-commands*)))
  (let (comp-hash)
    (when (and bind-new (> (hash-table-count *mon-help-w32-CMD-commands*) 0))
      (maphash #'(lambda (kk vv)
                   (push kk comp-hash))
               *mon-help-w32-CMD-commands*))
    (mapc #'(lambda (alpha)
              (mapc #'(lambda (sub)
                        (let ((subtst 
                               (when (car sub)
                                 `(,(upcase (format "%s" (car sub)))
                                    . ,(cdr sub)))))
                          (cond ((and subtst (not bind-new))
                                 (setf (gethash (car subtst) *mon-help-w32-CMD-commands*) (cdr subtst)))
                                ((and bind-new comp-hash subtst (not (member (car subtst) comp-hash)))
                                 (setf (gethash (car subtst) *mon-help-w32-CMD-commands*) (cdr subtst))))))
                    (cdr alpha)))
          cmd-vars-list)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-19T14:27:46-04:00Z}#{09431} - by MON KEY>
(when *mon-compromise-my-ms-W32-virginity*
(defun mon-help-w32-shell-execute (&optional insrtp intrp)
  "Doc of w32-shell-execute followed by MSDN doc of ShellExecute Function.\n\n
`w32-shell-execute' is a built-in function in `w32fns.c'.\n
\(w32-shell-execute operation document &optional parameters show-flag\)\n
Get Windows to perform operation on document.
This is a wrapper around the ShellExecute system function, which
invokes the application registered to handle operation for document.\n
operation is either nil or a string that names a supported operation.
What operations can be used depends on the particular document and its
handler application, but typically it is one of the following common
operations:\n
 \"open\"    - Open document, which could be a file, a directory, or an
               executable program.  If it is an application, that
               application is launched in the current buffer's default
               directory.  Otherwise, the application associated with
               document is launched in the buffer's default directory;
 \"print\"   - Print document, which must be a file;
 \"explore\" - Start the Windows Explorer at document;
 \"edit\"    - Launch an editor and open document for editing; which
               editor is launched depends on the association for the
               specified document;
 \"find\"    - Initiate search starting from document which must specify
               a directory;
 \"nil\"     - Invoke the default operation, or \"open\" if default is
               not defined or unavailable;\n
document is typically the name of a document file or a URL, but can
also be a program executable to run, or a directory to open in the
Windows Explorer.\n
If document is a program executable, the optional third arg parameters
can be a string containing command line parameters that will be passed
to the program; otherwise, parameters should be nil or unspecified.\n
Optional fourth argument show-flag can be used to control how the
application will be displayed when it is invoked.  If show-flag is nil
or unspecified, the application is displayed normally, otherwise it is
an integer representing a ShowWindow flag:\n
  0 - start hidden
  1 - start normally
  3 - start maximized
  6 - start minimized\n\n
 ----------------------
`ShellExecute' Function
 ----------------------\n
Performs an operation on a specified file.\n
:SYNTAX\n
    HINSTANCE ShellExecute(      
        HWND hwnd,
        LPCTSTR lpOperation,
        LPCTSTR lpFile,
        LPCTSTR lpParameters,
        LPCTSTR lpDirectory,
        INT nShowCmd
    );\n
:PARAMETERS\n
hwnd
    [in] A handle to the owner window used for displaying a user interface (UI)
    or error messages. This value can be NULL if the operation is not associated
    with a window.\n
lpOperation
    [in] A pointer to a null-terminated string, referred to in this case as a
    verb, that specifies the action to be performed. The set of available verbs
    depends on the particular file or folder. Generally, the actions available
    from an object's shortcut menu are available verbs. The following verbs are
    commonly used:\n
edit
    Launches an editor and opens the document for editing. If lpFile is not a
    document file, the function will fail.\n
explore
    Explores a folder specified by lpFile.\n
find
    Initiates a search beginning in the directory specified by lpDirectory.\n
open
    Opens the item specified by the lpFile parameter. The item can be a file or
    folder.\n
print
    Prints the file specified by lpFile. If lpFile is not a document file, the
    function fails.\n
NULL
    In systems prior to Microsoft Windows 2000, the default verb is used if it
    is valid and available in the registry. If not, the \"open\" verb is used.\n
    In Windows 2000 and later, the default verb is used if available. If not,
    the \"open\" verb is used. If neither verb is available, the system uses the
    first verb listed in the registry.\n
lpFile
    [in] A pointer to a null-terminated string that specifies the file or object
    on which to execute the specified verb. To specify a Shell namespace object,
    pass the fully qualified parse name. Note that not all verbs are supported
    on all objects. For example, not all document types support the \"print\"
    verb. If a relative path is used for the lpDirectory parameter do not use a
    relative path for lpFile.\n
lpParameters
    [in] If lpFile specifies an executable file, this parameter is a pointer to
    a null-terminated string that specifies the parameters to be passed to the
    application. The format of this string is determined by the verb that is to
    be invoked. If lpFile specifies a document file, lpParameters should be
    NULL.\n
lpDirectory
    [in] A pointer to a null-terminated string that specifies the default
    (working) directory for the action. If this value is NULL, the current
    working directory is used. If a relative path is provided at lpFile, do not
    use a relative path for lpDirectory.\n
nShowCmd
    [in] The flags that specify how an application is to be displayed when it is
    opened. If lpFile specifies a document file, the flag is simply passed to
    the associated application. It is up to the application to decide how to
    handle it.\n
SW_HIDE
    Hides the window and activates another window.\n
SW_MAXIMIZE
    Maximizes the specified window.\n
SW_MINIMIZE
    Minimizes the specified window and activates the next top-level window in
    the z-order.\n
SW_RESTORE
    Activates and displays the window. If the window is minimized or maximized,
    Windows restores it to its original size and position. An application should
    specify this flag when restoring a minimized window.\n
SW_SHOW
    Activates the window and displays it in its current size and position.\n
SW_SHOWDEFAULT
    Sets the show state based on the SW_ flag specified in the STARTUPINFO
    structure passed to the CreateProcess function by the program that started
    the application. An application should call ShowWindow with this flag to set
    the initial show state of its main window.\n
SW_SHOWMAXIMIZED
    Activates the window and displays it as a maximized window.\n
SW_SHOWMINIMIZED
    Activates the window and displays it as a minimized window.\n
SW_SHOWMINNOACTIVE
    Displays the window as a minimized window. The active window remains active.\n
SW_SHOWNA
    Displays the window in its current state. The active window remains active.\n
SW_SHOWNOACTIVATE
    Displays a window in its most recent size and position. The active window
    remains active.\n
SW_SHOWNORMAL
    Activate and displays a window. If the window is minimized or maximized,
    Windows restores it to its original size and position. An application should
    specify this flag when displaying the window for the first time.\n
:RETURN-VALUE\n
If the function succeeds, it returns a value greater than 32. If the function
fails, it returns an error value that indicates the cause of the failure. The
return value is cast as an HINSTANCE for backward compatibility with 16-bit
Windows applications. It is not a true HINSTANCE, however. It can be cast only
to an int and compared to either 32 or the following error codes below.\n
0	                The operating system is out of memory or resources.\n
ERROR_FILE_NOT_FOUND    The specified file was not found.\n
ERROR_PATH_NOT_FOUND    The specified path was not found.\n
ERROR_BAD_FORMAT	The .exe file is invalid (non-Microsoft Win32 .exe
                        or error in .exe image).\n
SE_ERR_ACCESSDENIED	The operating system denied access to the specified
                        file.\n
SE_ERR_ASSOCINCOMPLETE  The file name association is incomplete or invalid.\n
SE_ERR_DDEBUSY	        The Dynamic Data Exchange (DDE) transaction
                        could not be completed because other DDE
                        transactions were being processed.\n
SE_ERR_DDEFAIL	        The DDE transaction failed.\n
SE_ERR_DDETIMEOUT       The DDE transaction could not be completed because
                        the request timed out.\n
SE_ERR_DLLNOTFOUND      The specified DLL was not found.\n
SE_ERR_FNF	        The specified file was not found.\n
SE_ERR_NOASSOC	        There is no application associated with the given file name
                        extension. This error will also be returned if you
                        attempt to print a file that is not printable. \n
SE_ERR_OOM	        Not enough memory to complete the operation.\n
SE_ERR_PNF	        The specified path was not found.\n
SE_ERR_SHARE	        A sharing violation occurred.\n
:REMARKS\n
Because ShellExecute can delegate execution to Shell extensions (data sources,
context menu handlers, verb implementations) that are activated using Component
Object Model (COM), COM should be initialized before ShellExecute is
called. Some Shell extensions require the COM single-threaded apartment (STA)
type. In that case, COM should be initialized as shown here:\n
    CoInitializeEx(NULL, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE)\n
There are certainly instances where ShellExecute does not use one of these types
of Shell extension and those instances would not require COM to be initialized
at all. Nonetheless, it is good practice to always initalize COM before using
this function.\n
This method allows you to execute any commands in a folder's shortcut menu or
stored in the registry.\n
To open a folder, use either of the following calls:\n
ShellExecute(handle, NULL, <fully_qualified_path_to_folder>, NULL, NULL, SW_SHOWNORMAL);\n
or\n
ShellExecute(handle, \"open\", <fully_qualified_path_to_folder>, NULL, NULL, SW_SHOWNORMAL);\n
To explore a folder, use the following call:\n
ShellExecute(handle, \"explore\", <fully_qualified_path_to_folder>, NULL, NULL, SW_SHOWNORMAL);\n
To launch the Shell's Find utility for a directory, use the following call.\n
ShellExecute(handle, \"find\", <fully_qualified_path_to_folder>, NULL, NULL, 0);\n
If lpOperation is NULL, the function opens the file specified by lpFile. If
lpOperation is \"open\" or \"explore\", the function attempts to open or explore the
folder.\n
To obtain information about the application that is launched as a result of
calling ShellExecute, use ShellExecuteEx.\n
:NOTE The Launch folder windows in a separate process setting in Folder Options
affects ShellExecute. If that option is disabled (the default setting),
ShellExecute uses an open Explorer window rather than launch a new one. If no
Explorer window is open, ShellExecute launches a new one.\n
:FUNCTION-INFORMATION\n
Minimum DLL Version	shell32.dll version 3.51 or later
Custom Implementation	No
Header	shellapi.h
Import library	shell32.lib
Minimum operating systems	Windows NT 3.1, Windows 95
Unicode	Implemented as ANSI and Unicode versions.\n
:COPYRIGHT MS_Corp.\n
:SEE \(URL `http://msdn.microsoft.com/en-us/library/bb762153(VS.85).aspx'\).
:SEE \(URL `http://support.microsoft.com/kb/238245'\).
:SEE \(URL `http://msdn.microsoft.com/en-us/cc300389.aspx#E'\).\n
:SEE-ALSO `mon-help-w32-cmd-commands', `mon-help-w32-env',
`*mon-help-w32-CMD-commands*', `mon-bind-doc-help-proprietery-vars-at-loadtime'.\n►►►"
  (interactive "i\nP")
  (if (or insrtp intrp)
      (mon-help-function-spit-doc 'mon-help-w32-shell-execute :insertp t)
      (message "pass non-nil for optional arg INTRP")))
) ;; :CLOSE purity test
;;
;;; :TEST-ME (mon-help-w32-shell-execute)
;;; :TEST-ME (mon-help-w32-shell-execute t)
;;; :TEST-ME (describe-function 'mon-help-w32-shell-execute)
;;; :TEST-ME (call-interactively 'mon-help-w32-shell-execute)

;;; ==============================
(provide 'mon-doc-help-proprietary)
;;; ==============================

(eval-after-load "mon-doc-help-proprietary" 
  '(mon-bind-doc-help-proprietery-vars-at-loadtime
    ;; :NOTE Below, we convert the car of each cons in the alphabetic sublist to a string.
    ;;  As we're only hashing once this lets us do `string' completion with the
    ;;  hashtable and leaves the list below suitable for porting to other
    ;;  datastructures should we wish.
    '((A  
       (ADDUSERS . "Add or list users to/from a CSV file.")
       (ARP      . "Address Resolution Protocol.")
       (ASSOC    . "Change file extension associations - CMD builtin.")
       (ASSOCIAT . "One step file association.")
       (ATTRIB   . "Change file attributes."))
      (B
       (BOOTCFG  . "Edit Windows boot settings.")
       (BROWSTAT . "Get domain, browser and PDC info."))
      (C
       (CACLS    . "Change file permissions.")
       (CALL     . "Call one batch program from another - CMD builtin.")
       (CD       . "Change Directory - move to specific Folder - CMD builtin.")
       (CHANGE   . "Change Terminal Server Session properties.")
       (CHKDSK   . "Check Disk - check and repair disk problems.")
       (CHKNTFS  . "Check an NTFS file system.")
       (CHOICE   . "Accept keyboard input to batch file.")
       (CIPHER   . "Encrypt or Decrypt files/folders.")
       (CleanMgr . "Automated cleanup of Temp files, recycle bin.")
       (CLEARMEM . "Clear memory leaks.")
       (CLIP     . "Copy STDIN to the Windows clipboard.")
       (CLS      . "Clears command screen - CMD builtin.")
       (CLUSTER  . "Windows Clustering.")
       (CMD      . "Start a new CMD shell.")
       (COLOR    . "Change colors of the CMD window - CMD builtin.")
       (COMP     . "Compare the contents of two files or sets of files.")
       (COMPACT  . "Compress files or folders on an NTFS partition.")
       (COMPRESS . "Compress individual files on an NTFS partition.")
       (CON2PRT  . "Connect or disconnect a Printer.")
       (CONVERT  . "Convert a FAT drive to NTFS..")
       (COPY     . "Copy one or more files to another location - CMD builtin.")
       (CSCcmd   . "Client-side caching (Offline Files).")
       (CSVDE    . "Import or Export Active Directory data."))
      (D
       (DATE     . "Display or set the date - CMD builtin.")
       (DEFRAG   . "Defragment hard drive.")
       (DEL      . "Delete one or more files - CMD builtin.")
       (DELPROF  . "Delete NT user profiles.")
       (DELTREE  . "Delete a folder and all subfolders.")
       (DevCon   . "Device Manager Command Line Utility.")
       (DIR      . "Display list of files and folders - CMD builtin.")
       (DIRUSE   . "Display disk usage.")
       (DISKCOMP . "Compare the contents of two floppy disks.")
       (DISKCOPY . "Copy the contents of one floppy disk to another.")
       (DISKPART . "Disk Administration.")
       (DNSSTAT  . "DNS Statistics.")
       (DOSKEY   . "Edit command line, recall commands, and create macros.")
       (DSADD    . "Add user (computer, group..) to active directory.")
       (DSQUERY  . "List items in active directory.")
       (DSMOD    . "Modify user \(computer, group, etc.\) in active directory.")
       (DSRM     . "Remove items from Active Directory."))
      (E
       (ECHO     . "Display message on screen - CMD builtin.")
       (ENDLOCAL . "End localisation of environment changes in a batch file - CMD builtin.")
       (ERASE    . "Delete one or more files - CMD builtin.")
       (EXIT     . "Quit current script/routine and set an errorlevel - CMD builtin.")
       (EXPAND   . "Uncompress files.")
       (EXTRACT  . "Uncompress CAB files."))
      (F
       (FC       . "Compare two files.")
       (FIND     . "Search text string in a file.")
       (FINDSTR  . "Search strings in files.")
       (FOR      . "Loop command: all options Files, Directory, List - CMD builtin.")
       (FORFILES . "Batch process multiple files.")
       (FORMAT   . "Format a disk.")
       (FREEDISK . "Check free disk space (in bytes).")
       (FSUTIL   . "File and Volume utilities.")
       (FTP      . "File Transfer Protocol.")
       (FTYPE    . "Display or modify file type extension associations - CMD builtin."))
      (G
       (GLOBAL   . "Display membership of global groups.")
       (GOTO     . "Direct a batch program to jump to labelled line - CMD builtin."))
      (H
       (HELP     . "Online Help."))
      (I
       (iCACLS   . "Change file and folder permissions.")
       (IF       . "Conditionally perform a command - CMD builtin.")
       (IFMEMBER . "Test if the current user is in an NT Workgroup.")
       (IPCONFIG . "Configure IP."))
      (K
       (KILL     . "Remove program from memory."))
      (L
       (LABEL    . "Edit disk label.")
       (LOCAL    . "Display membership of local groups.")
       (LOGEVENT . "Write text to the NT event viewer..")
       (LOGOFF   . "Log user off.")
       (LOGTIME  . "Log date and time in file."))
      (M
       (MAPISEND . "Send email from the command line.")
       (MBSAcli  . "Baseline Security Analyzer.")
       (MEM      . "Display memory usage.")
       (MKDIR    . "Also, MD - Create new folders.")
       (MKLINK   . "Create a symbolic link (linkd).")
       (MODE     . "Configure a system device.")
       (MORE     . "Display output, one screen at a time.")
       (MOUNTVOL . "Manage a volume mount point.")
       (MOVE     . "Move files from one folder to another - CMD builtin.")
       (MOVEUSER . "Move user from one domain to another.")
       (MSG      . "Send a message.")
       (MSIEXEC  . "Microsoft Windows Installer.")
       (MSINFO   . "Windows NT diagnostics.")
       (MSTSC    . "Terminal Server Connection \(Remote Desktop Protocol\).")
       (MUNGE    . "Find and Replace text within file\(s\).")
       (MV       . "Copy in-use files."))
      (N
       (NET        . "Manage network resources.")
       (NETDOM     . "Domain Manager.")
       (NETSH      . "Configure Network Interfaces, Windows Firewall & Remote access.")
       (NETSVC     . "Command-line Service Controller.")
       (NBTSTAT    . "Display networking statistics \(NetBIOS over TCP/IP\).")
       (NETSTAT    . "Display networking statistics \(TCP/IP\).")
       (NOW        . "Display the current Date and Time.")
       (NSLOOKUP   . "Name server lookup.")
       (NTBACKUP   . "Backup folders to tape.")
       (NTRIGHTS   . "Edit user account rights."))
      (P
       (PATH       . "Display or set search path for executable files - CMD builtin.")
       (PATHPING   . "Trace route plus network latency and packet loss.")
       (PAUSE      . "Suspend processing of batch file and display a message - CMD builtin.")
       (PERMS      . "Show permissions for user.")
       (PERFMON    . "Performance Monitor.")
       (PING       . "Test network connection.")
       (POPD       . "Restore previous value of current directory from PUSHD - CMD builtin.")
       (PORTQRY    . "Display status of ports and services.")
       (POWERCFG   . "Configure power settings.")
       (PRINT      . "Print a text file.")
       (PRNCNFG    . "Display, configure or rename a printer.")
       (PRNMNGR    . "Add, delete, list printers set the default printer.")
       (PROMPT     . "Change the command prompt - CMD builtin.")
       (PsExec     . "Execute process remotely.")
       (PsFile     . "Show files opened remotely.")
       (PsGetSid   . "Display the SID of computer or a user.")
       (PsInfo     . "List system information.")
       (PsKill     . "Kill processes by name or process ID.")
       (PsList     . "List detailed information about processes.")
       (PsLoggedOn . "Show logged on users \(locally or via resource sharing\).")
       (PsLogList  . "Event log records.")
       (PsPasswd   . "Change account password.")
       (PsService  . "View and control services.")
       (PsShutdown . "Shutdown or reboot computer.")
       (PsSuspend  . "Suspend processes.")
       (PUSHD    . "Save and change the current directory - CMD builtin."))
      (Q
       (QGREP    . "Search file\(s\) for lines matching pattern."))
      (R
       (RASDIAL  . "Manage RAS connections.")
       (RASPHONE . "Manage RAS connections.")
       (RECOVER  . "Recover damaged file from defective disk.")
       (REG      . "Registry: Read, Set, Export, Delete keys and values.")
       (REGEDIT  . "Import or export registry settings.")
       (REGSVR32 . "Register or unregister a DLL.")
       (REGINI   . "Change Registry Permissions.")
       (REM      . "Record comments \(remarks\) in batch file - CMD builtin.")
       (REN      . "Rename file or files - CMD builtin.")
       (REPLACE  . "Replace or update one file with another.")
       (RMDIR    . "Also, RD - Delete folder(s) - CMD builtin.")
       (RMTSHARE . "Share folder or printer.")
       (ROBOCOPY . "Robust File and Folder Copy.")
       (ROUTE    . "Manipulate network routing tables.")
       (RUNAS    . "Execute a program under different user account.")
       (RUNDLL32 . "Run a DLL command \(add/remove print connections\)."))
      (S
       (SC       . "Service Control.")
       (SCHTASKS . "Schedule command to run at specific time.")
       (SCLIST   . "Display NT Services.")
       (SET      . "Display, set, or remove environment variables - CMD builtin.")
       (SETLOCAL . "Control visibility of environment variables - CMD builtin.")
       (SETX     . "Set environment variables permanently.")
       (SHARE    . "List or edit file share or print share.")
       (SHIFT    . "Shift replaceable parameter position in batch file - CMD builtin.")
       (SHORTCUT . "Create windows shortcut \(.LNK file\).")
       (SHOWGRPS . "List NT Workgroups user has joined.")
       (SHOWMBRS . "List users who are members of Workgroup.")
       (SHUTDOWN . "Shutdown computer.")
       (SLEEP    . "Wait for x seconds.")
       (SLMGR    . "Software Licensing Management \(Vista/2008\).")
       (SOON     . "Schedule command to run in near future.")
       (SORT     . "Sort input.")
       (START    . "Start a program or command in a separate window - CMD builtin.")
       (SU       . "Switch User.")
       (SUBINACL . "Edit file and folder Permissions, Ownership and Domain.")
       (SUBST    . "Associate path with drive letter.")
       (SYSTEMINFO . "List system configuration."))
      (T
       (TASKLIST . "List running applications and services.")
       (TASKKILL . "Remove running process from memory.")
       (TIME     . "Display or set the system time - CMD builtin.")
       (TIMEOUT  . "Delay processing of batch file.")
       (TITLE    . "Set window title for CMD.EXE session - CMD builtin.")
       (TLIST    . "Task list with full path.")
       (TOUCH    . "Change file timestamps.")
       (TRACERT  . "Trace route to remote host.")
       (TREE     . "Graphical display of folder structure.")
       (TYPE     . "Display the contents of text file - CMD builtin."))
      (U
       (USRSTAT  . "List domain usernames and last login."))
      (V
       (VER      . "Display version information - CMD builtin.")
       (VERIFY   . "Verify files have been saved - CMD builtin.")
       (VOL      . "Display disk label - CMD builtin."))
      (W
       (WHERE    . "Locate and display files in directory tree.")
       (WHOAMI   . "Output current UserName and domain.")
       (WINDIFF  . "Compare contents of two files or sets of files.")
       (WINMSD   . "Windows system diagnostics.")
       (WINMSDP  . "Windows system diagnostics II.")
       (WMIC     . "WMI Commands."))
      (X
       (XCACLS   . "Change file and folder permissions.")
       (XCOPY    . "Copy files and folders.")))))

;;; ================================================================
;;; mon-doc-help-proprietary.el ends here
;;; EOF
