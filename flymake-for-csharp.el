;;; flymake-for-csharp.el --- C# mode derived mode
;;
;; Author:     Dino Chiesa
;; Modified:   April 2010
;; Version:    1.1
;; Keywords:   c# flymake
;;
;; This code is distributed under the MS-Public License.
;; See http://opensource.org/licenses/ms-pl.html
;;
;; last saved
;; Time-stamp: <2010-April-11 15:15:05>


;; ==================================================================
;;
;; This module provides tweaks to the flymake minor mode, to allow it to
;; work with csharp.
;;
;; Flymake is built-in to emacs.  It periodically compiles an active
;; buffer when the minor mode is enabled, and then flags or highlights
;; lines that cause errors or warnings during the compile as you
;; edit. It is analogous to the red-squigglies you get in Visual Studio,
;; highlighting syntax errors or other compile problems.
;;
;; This elisp module defines a set of tweaks of flymake, to allow it to
;; work with C# on Windows.  flymake-for-csharp depends on a makefile or
;; msbuild file of well-known name, with some well-known settings, in
;; order to work.  These are documented below.
;;
;; ==================================================================
;;
;; To use:
;;
;; 1. put flymake-for-csharp.el on your load path.
;;
;; 2. somewhere in your .emacs, place this:
;;       (require 'flymake-for-csharp)
;;
;; 3. Add this to your csharp-mode-hook function:
;;       (flymake-mode)
;;
;; 4. specify how you want to use flymake, via these variables:
;;
;;     flymake-for-csharp-netsdk-location
;;     flymake-for-csharp-dotnet-location
;;     flymake-for-csharp-buildfile-alist
;;     flymake-for-csharp-grep-pgm
;;     flymake-for-csharp-csc-arguments
;;


;; flymake-for-csharp runs the c# compiler to do syntax checking on a
;; .cs file, as you edit it.  flymake-for-csharp allows you the choice
;; of 3 ways to run the build C# files: run csc.exe directly, run
;; nmake.exe using a makefile that you specify, or run msbuild using a
;; buildfile that you specify.  To allow flymake-for-csharp to work, you
;; need to specify a particular well-known target in your makefile, or
;; you must specify instructions in your msbuild file.
;;
;;
;; Option A: use the csc.exe compiler directly
;; ============================================
;;
;; This is the simplest option, and also the most limited. With this
;; option, flymake-for-csharp compiles the existing buffer using the
;; csc.exe command-line compiler, and the arguments you provide, via the
;; `flymake-for-csharp-csc-arguments' variable.  Appended to that list
;; of arguments will be the name of a temporary .cs file, with the
;; contents of the currently-being-edited .cs file. You should not
;; include the name of the current .cs file in the
;; `flymake-for-csharp-csc-arguments' variable, but you can specify the
;; names of other .cs files that should be compiled with the current
;; one.  The `flymake-for-csharp-csc-arguments' variable is only used
;; with the direct csc.exe build option.
;;
;; In deciding which build option to use, flymake-for-csharp searches
;; the list of build files in `flymake-for-csharp-buildfile-alist' to
;; find one appropriate for use for syntax checking. Therefore, if you
;; want to use the direct csc.exe build option, set
;; `flymake-for-csharp-buildfile-alist' to nil, OR, insure that none of
;; the build files in that list, exist in the directory where the .cs
;; file resides, OR if they do exist, that the required build targets
;; (see below for an explanation) are not present in those build files.
;;
;; If you do no special setup, the direct csc.exe build will be used.
;;
;; The compile-directly-with-csc.exe option is limited because you use
;; the same arguments with all .cs buffers.  Often you want to compile
;; .cs files differently.  Each project usually requires a different set
;; of assemblies, for instance.  If you want to use flymake in a
;; scenario like that, you'll want to rely on the nmake.exe or
;; msbuild.exe options for flymake-for-csharp.
;;
;;
;; Option B: use nmake and a makefile.
;; ============================================
;; If you want to use a makefile, you must:
;;
;; 1. set `flymake-for-csharp-buildfile-alist' to a list of names of
;;    makefiles. You can use wildcards.  The name of the makefile must
;;    have the word "makefile" in it, or should end with .mk.
;;    flymake-for-csharp looks only at files with names that fit those
;;    constraints.  flymake-for-csharp searches for each named file in
;;    the same directory as the currently-being-edited .cs file. If the
;;    file exists, flymake-for-csharp searches within the file for a
;;    make target by the name of check-syntax: .  (This fixed name is
;;    consistent with the use of flymake for C language source files.).
;;    If the file exists and contains the check-syntax: make target,
;;    flymake-for-csharp runs nmake.exe on it to check the syntax of the
;;    currently-being-edited .cs file.
;;
;;
;; 2. configure the `flymake-for-csharp-netsdk-location' variable to
;;    specify the location of the bin directory that contains nmake.exe.
;;    Usually it is in one of the following places:
;;
;;    - the .NET 2.0 SDK directory
;;      example: C:\Program Files\Microsoft SDKs\Microsoft .Net\v2.0
;;    - the Visual Studio directory, if the VS install is the source of the .NET SDK
;;      example: c:\Program Files\Microsoft Visual Studio 9\SDK\v2.0
;;
;;    example:
;;      (setq flymake-for-csharp-buildfile-alist (list "makefile"))
;;      (setq flymake-for-csharp-netsdk-location "c:\\Program Files\\Microsoft Visual Studio 9\\SDK\\v2.0")
;;
;;    Notice that you should not append the bin subdir on the value of
;;    flymake-for-csharp-netsdk-location.
;;
;;
;; 3. Set your make target something like so:
;;
;;        check-syntax:
;;           %windir%\Microsoft.NET\Framework\v3.5\csc.exe /t:module $(CHK_SOURCES)
;;
;;    The target name MUST be "check-syntax". You should, of course,
;;    specify the location of your c# compiler as appropriate.  You can
;;    use a make variable like $(CSC) if one is defined in your
;;    makefile.  You SHOULD use /target:netmodule.  flymake-for-csharp
;;    will delete any temporary .netmodule files that are created as
;;    part of the syntax check.  If you don't use /target:netmodule, you
;;    will get a DLL or EXE, and flymake-for-csharp won't clean it up.
;;
;;    You MUST use $(CHK_SOURCES) as the end of the line.  The
;;    CHK_SOURCES symbol is defined on the command line when
;;    flymake-for-csharp invokes nmake.exe.  CHK_SOURCES should not be
;;    explicitly defined in your makefile.
;;
;;    If you don't want to put this make target into your main makefile,
;;    you can alternatively put it into an alternative makefile, for
;;    example "makefile.flymake". Specify that filename as one element
;;    in the variable `flymake-for-csharp-buildfile-alist'.
;;
;;
;; To insure that you use nmake to do the syntax checking, be sure that
;; on the `flymake-for-csharp-buildfile-alist' list, a matching makefile
;; file appears before any matching msbuild file.  flymake-for-csharp
;; will use the first suitable build file (either a makefile or a
;; msbuild file) that it finds.
;;
;;
;; ==================================================================
;;
;;
;; Option C: use msbuild
;; ============================================
;;
;; If you use msbuild, rather than nmake, specify the location of
;; msbuild.exe via the `flymake-for-csharp-dotnet-location' variable,
;; and insert into `flymake-for-csharp-buildfile-alist' a list of names
;; of files. Wildcards are allowed.  After wildcard expansion, for each
;; file in the list that has a name that includes "msbuild" or ends in
;; .xml, or .csproj, flymake-for-csharp will search the file, and check
;; for the well-known build target of "CheckSyntax" in the file. If that
;; target is found, then flymake-for-csharp will use that as the msbuild
;; file to do syntax checking.
;;
;; flymake-for-csharp insists on finding a build file that contains a
;; target by the name of "CheckSyntax".  Also, the target name MUST be
;; defined on the same line as the <Target> element, immediately
;; following the element name like so:
;;
;;      <Target Name="CheckSyntax"  ....
;;
;; The reason for this restriction: flymake-for-csharp does a
;; text-search in the build file for that well-known target name, in
;; that position.  If you define a target with a different name,
;; flymake-for-csharp will conclude that it is not useful for syntax
;; checking. If you define a target with a different text layout,
;; flymake-for-csharp won't find it with its naive text search.
;;
;; To insure that you use msbuild to do the syntax checking, be sure
;; that on the `flymake-for-csharp-buildfile-alist' list, a matching
;; msbuild file appears before a matching makefile.  flymake-for-csharp
;; will use the first suitable build file (either a makefile or a
;; msbuild file) that it finds.
;;
;; If the source you are editing consists of small utility programs that
;; each rely on a single source file, you can use a standard,
;; boilerplate, build file.  Define it like this:
;;
;; <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003"
;;         DefaultTargets="CompileAll"
;;         ToolsVersion="3.5"
;;       >
;;
;;  <Import Project="$(MSBuildToolsPath)\Microsoft.CSharp.targets" />
;;
;;  <!-- specify reference assemblies for all builds in this project -->
;;  <ItemGroup>
;;    <Reference Include="mscorlib" />
;;    <Reference Include="System" />
;;    <Reference Include="System.Core" />
;;    <Reference Include="System.Data" />
;;    <Reference Include="System.Data.Linq" />                   <!-- LINQ -->
;;    <!--Reference Include="System.ServiceModel" /-->           <!-- WCF -->
;;    <!--Reference Include="System.ServiceModel.Web" /-->       <!-- WCF -->
;;    <!--Reference Include="System.Runtime.Serialization" /-->  <!-- WCF -->
;;  </ItemGroup>
;;
;;  <Target Name="CheckSyntax"
;;          DependsOnTargets="ResolveAssemblyReferences"
;;        >
;;    <CSC
;;       Sources="$(SourceFileToCheck)"
;;       References="@(ReferencePath)"
;;       TargetType="module"
;;       Toolpath="$(MSBuildToolsPath)"
;;       Nologo="true"
;;       />
;;  </Target>
;;
;; </Project>
;;
;; -ends-
;;
;; (This msbuild file works only with .NET 3.5.)
;;
;; If there are additional assemblies you need to reference, add them as
;; you would normally, by adding an additional <Reference> element under
;; the <ItemGroup> element. If the assembly is inside the GAC, then use
;; <Reference Include=""/> and specify the short name of the assembly.
;; If the assembly is located in the filesystem, specify the path to the
;; DLL, relative to the local directory. For example, you can specify
;; c:\ionic\Ionic.Zip.dll if the source module references the DotNetZip
;; assembly and if that DLL is found in the c:\ionic directory.
;;
;; If your projects consist of multiple source files, then you need to
;; get fancier.  The way flymake works is, it copies the current contents
;; of the buffer to a temporary source file, then compiles it.  Therefore
;; in the case where you are compiling multiple source files together,
;; You need to compile all files, *including* the temporary copy of the
;; file being edited, but *excluding* the actual source file being
;; edited. This is easily done in msbuild with a specially-structured
;; Exclude qualifier. Your msbuild.flymake.xml file should look something
;; like this:
;;
;; <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003"
;;          DefaultTargets="CompileAll"
;;          ToolsVersion="3.5"
;;          >
;;
;;   <Import Project="$(MSBuildToolsPath)\Microsoft.CSharp.targets" />
;;
;;    <PropertyGroup>
;;       <Optimize>false</Optimize>
;;       <DebugSymbols>true</DebugSymbols>
;;       <!-- <OutputPath>.\bin\</OutputPath>  -->
;;       <OutputPath>.\</OutputPath>
;;       <OutDir>.\</OutDir>
;;       <IntermediateOutputPath>.\obj\</IntermediateOutputPath>
;;    </PropertyGroup>
;;
;;   <!-- specify reference assemblies for all builds in this project -->
;;   <ItemGroup>
;;     <Reference Include="mscorlib" />
;;     <Reference Include="System" />
;;     <Reference Include="System.Core" />
;;     <Reference Include="System.Data" />
;;     <Reference Include="System.Data.Linq" />                   <!-- LINQ -->
;;     <!--Reference Include="System.ServiceModel" /-->           <!-- WCF -->
;;     <!--Reference Include="System.ServiceModel.Web" /-->       <!-- WCF -->
;;     <!--Reference Include="System.Runtime.Serialization" /-->  <!-- WCF -->
;;   </ItemGroup>
;;
;;   <!-- This ItemGroup includes every .cs source file in the directory,           -->
;;   <!-- except for the one indicated by OriginalSourceFile.  In flymake, that     -->
;;   <!-- property indicates the currently edited file. So the result is that the   -->
;;   <!-- ItemGroup CSFile will include all files, including the _flymake.cs clone, -->
;;   <!-- but not including the original file.  Which is what we want.              -->
;;   <ItemGroup>
;;     <CSFile Include="*.cs" Exclude="$(OriginalSourceFile)" />
;;   </ItemGroup>
;;
;;   <!-- Stuff the OriginalSourceFile property into an ItemGroup.                  -->
;;   <!-- We do this so we can get at the metadata, which is available only         -->
;;   <!-- through an item within an ItemGroup.  We want the root filename, which    -->
;;   <!-- we use to name the output netmodule.                                      -->
;;   <ItemGroup>
;;     <ExcludedCSFile Include="$(OriginalSourceFile)" />
;;   </ItemGroup>
;;
;;   <Target Name="CheckSyntax"
;;           DependsOnTargets="ResolveAssemblyReferences"
;;           >
;;     <!-- Run the Visual C# compilation on the specified set of .cs files. -->
;;     <CSC
;;        Sources="@(CSFile)"
;;        References="@(ReferencePath)"
;;        TargetType="module"
;;        Toolpath="$(MSBuildToolsPath)"
;;        OutputAssembly="%(ExcludedCSFile.Filename)_flymake.netmodule"
;;        Nologo="true"
;;        />
;;   </Target>
;;
;; </Project>
;;
;; -ends-
;;
;; (The above msbuild file also works only with .NET 3.5.)
;;
;; The above assumes that every .cs file in the current directory should be
;; compiled together.  If this is not the case, then modify the msbuild file
;; accordingly, to exclude the files as appropriate.  You can also specify
;; additional references, and so on.
;;


(require 'flymake)



;;(setq flymake-log-level 3)  ;; insure flymake errors get plopped into the *Messages* buffer
;; -1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"


;; flymake-gui-warnings-enabled
;; nil = turn off? GUI warnings, eg. when no msbuild.flymake.xml file found
;; t = turn on? GUI warnings
(setq flymake-gui-warnings-enabled nil)



;; New variables for use with flymake-for-csharp
(defvar flymake-for-csharp-netsdk-location "c:\\netsdk2.0"
  "Location of .NET SDK, for finding nmake.exe.
flymake-for-csharp looks for nmake.exe in the bin subdirectory
of the given directory.  An example value is: c:\\Program
Files\\Microsoft Visual Studio 8\\SDK\\v2.0 . This variable is
referenced only if the build-file is an msbuild-compatible
file. See `flymake-for-csharp-buildfile-alist' for details.")

(defvar flymake-for-csharp-dotnet-location "c:\\.net3.5"
  "Directory containing MSBuild.exe and csc.exe. Typically, this
is c:\\Windows\\Microsoft.NET\\Framework\\v3.5 .  This variable
is referenced if the build-file is an msbuild-compatible file,
or if using csc. See `flymake-for-csharp-buildfile-alist' for
details.")

(defvar flymake-for-csharp-grep-pgm  "grep.exe"
  "The location of the grep program on your system.  This can be
a bare file if you want flymake-for-csharp to find grep.exe on
the path. ")

(defvar flymake-for-csharp-buildfile-alist
  (list "makefile.flymake" "*.flymake.xml" "makefile" "*.csproj")
  "A list of build files that flymake should look for. Wildcards
are allowed. For each filename in this list, flymake-for-csharp
will check the existence of the file in the local directory.  If
the file exists, it will determine if the file has the
appropriate target, then invoke the associated build tool.  If
the filename is 'makefile' or begins with 'makefile', it will try
to use the file with nmake.  If the filename is '*.csproj' or
'msbuild.*' or ends in '.xml', flymake will try to use the build
file with msbuild.  The build file you specify should have the
check-syntax target contained within it. If none of the build
files on the list seem appropriate, then flymake-for-csharp will
resort to directly compiling the current file by itself, using
csc.exe, into a netmodule." )


(defvar flymake-for-csharp-csc-arguments
  (list "/t:module" "/nologo")
  "A list of arguments to use with the csc.exe
compiler, when using flymake-for-csharp with a
direct csc.exe build for syntax checking purposes.")






(defun flymake-for-csharp-cleanup ()
  "Delete the temporary .netmodule file created in syntax checking,
then call through to flymake-simple-cleanup."
  (flymake-log 3 "flymake-for-csharp-cleanup")
  (if flymake-temp-source-file-name
  (let* ((netmodule-name
          (concat (file-name-sans-extension flymake-temp-source-file-name)
                              ".netmodule"))
         (expanded-netmodule-name (expand-file-name netmodule-name "."))
         )
    (if (file-exists-p expanded-netmodule-name)
        (flymake-safe-delete-file expanded-netmodule-name)
      )
    )
  )
    (flymake-simple-cleanup)

  )



(defun flymake-for-csharp-grep-target-in-build-file (target file)
  "run grep"
  (interactive)
  (save-excursion
    (let ((buf (get-buffer-create "*flymake* csharp-grep-out"))
          (beg 0))
      (set-buffer buf)  ;; switch-to-buffer
      (delete-region (point-min) (point-max))
      (setq beg (point-min))
      (goto-char (point-min))
      (call-process flymake-for-csharp-grep-pgm ;; program
                    nil        ;; infile - which file to use for input
                    t          ;; buffer - t means current
                    nil        ;; display - non-nil means redisplay buf as output arrives
                    "-F"       ;; args - "-F" means treat expression as plain text
                    target     ;;        target - what to search for.  eg, "check-syntax"
                    file       ;;        file - the to search in
                    "NUL"      ;;        NUL - make sure we print out the filename
                    )
      (goto-char (point-min))
      (if (> (point-max) (point-min))
          (flymake-log 4 "flymake-for-csharp-grep: f(%s) t(%s) output(%s)"
                       file target
                       (buffer-substring-no-properties (point-min) (1- (point-max))))
        (flymake-log 4 "flymake-for-csharp-grep f(%s) t(%s): no output" file target))

      (re-search-forward target nil t))))





(defun flymake-for-csharp-figure-build ()
"Figures the build file and type of build to run for flymake.  It
does this by examining the `flymake-for-csharp-buildfile-alist',
checking for existence of each file, then checking (best effort)
for the appropriate flymake target in the buildfile. If none of those
files exist or if they lack appropriate targets, then it backs off
to a csc.exe build."
  (let ((filelist
         ;; this does wildcard-expansion of the alist, then consolidates
         ;; to a single list
         (apply #'append
                (mapcar '(lambda (a) (file-expand-wildcards a t))
                        flymake-for-csharp-buildfile-alist)))
        (build-file nil)
        (build-type nil))

    (while filelist
      (setq build-file (car filelist))
      (flymake-log 4 "flymake-for-csharp-figure-build: file(%s)" build-file)
      (if (file-exists-p build-file)
          ;;then
          (progn
            (flymake-log 3 "flymake-for-csharp-figure-build: file(%s) exists" build-file)
            (cond
             ((or (not (null (string-match "makefile" build-file)))
                  (not (null (string-match ".*\\.mk$" build-file))))
              (if (flymake-for-csharp-grep-target-in-build-file "check-syntax:" build-file)
                  (setq build-type "nmake"
                        filelist nil)
                (flymake-log 3 "flymake-for-csharp-figure-build: file(%s) - no suitable target"
                             build-file)))

             ((or (not (null (string-match "msbuild" build-file)))
                  (not (null (string-match ".*\\.xml$" build-file)))
                  (not (null (string-match ".*\\.csproj$" build-file))))
              (if (flymake-for-csharp-grep-target-in-build-file
                   "<Target Name=\"CheckSyntax\""  build-file)
                  (setq build-type "msbuild"
                        filelist nil)
                (flymake-log 3 "flymake-for-csharp-figure-build: file(%s) - no suitable target"
                             build-file)))

             (t
                (flymake-log 3 "flymake-for-csharp-figure-build: file(%s) - not a recognized file"
                             build-file))))
        ;;else
        (flymake-log 3 "flymake-for-csharp-figure-build: file(%s) does not exist" build-file))

      (if (null build-type)
          (progn
            (setq filelist (cdr filelist))
            (setq build-file nil))))

    (if (null build-type)
      (setq build-type "csc"))

    (list build-type build-file)))





(defun flymake-for-csharp-init ()
  (flymake-for-csharp-init-impl 'flymake-create-temp-inplace t t  'flymake-for-csharp-get-flymake-cmdline))


(defun flymake-for-csharp-init-impl (create-temp-f use-relative-base-dir use-relative-source get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
        (temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))

    (setq args (flymake-get-syntax-check-program-args temp-source-file-name "."  ;; buildfile-dir
                                                      use-relative-base-dir use-relative-source
                                                      get-cmdline-f))
    (flymake-log 3 "flymake-for-csharp-init: %s" (prin1-to-string args))
    args))


;(debug-on-entry 'flymake-for-csharp-init)



; This method re-defines the defun shipped in flymake, for csharp.  Re-defining
; this function *will* definitely break flymake for all other languages.  One
; way to fix that problem is to make the "get-make-cmdline" function a
; configurable hook within flymake!

;;(defun flymake-get-make-cmdline (source base-dir)
(defun flymake-for-csharp-get-flymake-cmdline (source base-dir)
  (let* ((build (flymake-for-csharp-figure-build))
         (flavor (car build)) ;; flavor:  "msbuild" or "nmake" or "csc"
         (build-file (cadr build))
        )
    (cond
     ((string= flavor "msbuild")
      (list (concat  flymake-for-csharp-dotnet-location "\\msbuild.exe")
            (list build-file
                  ;;(concat base-dir "/" build-file)
                  "/nologo"
                  "/t:CheckSyntax"
                  "/v:quiet"  ;; normal
                  ;; use file-relative-name to remove the fully-qualified directory name
                  (concat "/property:SourceFileToCheck=" (file-relative-name source))
                  (concat "/property:OriginalSourceFile=" (file-relative-name buffer-file-name))
                  )))
     ((string= flavor "nmake")
      (list (concat  flymake-for-csharp-netsdk-location "\\bin\\nmake.exe")
            (list "/f"
                  build-file
                  ;;(concat base-dir "/" build-file)
                  "/nologo"
                  (concat "CHK_SOURCES=" source)
                  "SYNTAX_CHECK_MODE=1"
                  "check-syntax")))
     (t
      (list (concat  flymake-for-csharp-dotnet-location "\\csc.exe")
        (append flymake-for-csharp-csc-arguments (list source)))))))




; This fixup sets flymake to use a different cleanup routine for c# compiles
(defun flymake-for-csharp-fixup ()
  (let (elt
        (csharp-entry nil)
        (masks flymake-allowed-file-name-masks))

    ;; The "flymake-allowed-file-name-masks" variable stores a filename pattern as
    ;; well as the make-init function, and a cleanup function.  In the case of csharp,
    ;; the setting in flymake.el has the cleanup fn as nil, which means it gets the
    ;; standard cleanup : the *_flymake.cs cloned source file gets deleted.  But the
    ;; flymake-for-csharp compiles the .cs file into a module,
    ;; which also needs to be deleted afterwards.
    ;;

    ;; Here, we remove the C# entry in the "flymake-allowed-file-name-masks"
    ;; variable, and replace it with an entry that includes a custom csharp cleanup
    ;; routine.  That cleanup routine deletes the .netmodule file.

    ;; I could just setq the "flymake-allowed-file-name-masks" var to the C# thing I
    ;; want, but that would obliterate all the masks for all other languages, which
    ;; would be bad manners.
    ;;
    ;; You know, come to think of it, I could just delete the generated .netmodule
    ;; file in the msbuild or makefile.  That might be simpler.
    ;;
    ;; But the main point is this ought to be more easily configurable or customizable
    ;; in flymake.el.  And also, flymake ought to do something reasonable for csharp builds,
    ;; rather than completely punt.
    ;;
    ;; This fixup is really hacky, relying on the string that is used for csharp in
    ;; flymake.el.  But it will do for now...

    ;; Find the entry
    (while (consp masks)
      (setq elt (car masks))
      (if (string= "\\.cs\\'" (car elt))
          (setq csharp-entry elt)
        )
      (setq masks (cdr masks))
      )

    ;;  remove the original cleanup item entry ...
    (if csharp-entry
        (setq flymake-allowed-file-name-masks
              (delete csharp-entry flymake-allowed-file-name-masks)))

    ;; Now add a new cleanup item entry, with the custom cleanup method.
    (setq flymake-allowed-file-name-masks
          (cons
           '("\\.cs\\'" flymake-for-csharp-init flymake-for-csharp-cleanup)
           flymake-allowed-file-name-masks))
    )
  )

; need to do this only once, not every time csharp-mode is invoked
(flymake-for-csharp-fixup)


;; =======================================================
;;
;; This section attempts to workaround some anomalous display ebhavior
;; for tooltips.  It's not strictly necessary, only aesthetic.  The issue is that
;; tooltips can get clipped.  This is the topic of Emacs bug #5908, unfixed
;; in v23 and present in v22.

(defun cheeso-reverse-string (s)
  (coerce (reverse (loop for b across s
                         collect b))
          'string))

(defun cheeso-string-trim (s &rest chars)
  "Trim CHARS from the ends of S"
  (apply 'cheeso-string-trim-right
         (apply 'cheeso-string-trim-left s chars)
         chars))

(defun cheeso-string-trim-left (s &rest chars)
  (let ((idx (dotimes (i (length s))
               (unless (member (elt s i) chars)
                 (return i)))))
    (if idx
        (subseq s idx)
      "")))

(defun cheeso-string-trim-right (s &rest chars)
  (cheeso-reverse-string (apply 'cheeso-string-trim-left (cheeso-reverse-string s) chars)))


(defun cheeso-reform-string (limit arg)
  "Reforms a single-line string ARG to a multi-line string with a max
of LIMIT chars on a line.

This is intended to solve a problem with the display of tooltip text
in emacs on Win32 - which is that the tooltip is extended to be very very
long, and the final line is clipped.

The solution is to split the text into multiple lines, and to add a
trailing newline to trick the tooltip logic into doing the right thing."
  (let ((orig arg) (modified "") (curline "") word
        (words (split-string arg " ")))
    (while words
      (progn
        (setq word (car words))
        (setq words (cdr words))
        (if (stringp word)
            (progn
            (if (> (+ (length curline) (length word) 1) limit)
                ;; then
                (progn
                  (setq modified (concat modified curline "\n"))
                  (setq curline "")))
            (setq curline (concat curline " " word)))

            )))

    (setq modified (concat modified curline " \n\n")))
  )

`
(defadvice tooltip-show (before
                         flymake-for-csharp-fixup-tooltip
                         (arg &optional use-echo-area)
                         activate compile)
  (progn
    (if ;;(and (not use-echo-area) (eq major-mode 'csharp-mode))
        (not use-echo-area)
        (let ((orig (ad-get-arg 0)))
          (ad-set-arg 0 (concat " " (cheeso-string-trim (cheeso-reform-string 74 orig) ?\ )))
          ))))

;; =======================================================


(defadvice flymake-posn-at-point-as-event (before
                                           flymake-for-csharp-advice-4
                                           (&optional position window dx dy)
                                           compile activate)
  (let ((dx1 (ad-get-arg 2))
        (dy1 (ad-get-arg 3)))
    (if (not (null dx1))
        (setq dx1 (+ dx1 20))
      (setq dx1 20))
    (if (not (null dy1))
        (setq dy1 (+ dy1 14))
      (setq dy1 14))

    (ad-set-arg 2 dx1)
    (ad-set-arg 3 dy1)

    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'flymake-for-csharp)

;;; end of flymake-for-csharp.el
