;;; flymake-for-csharp.el --- C# mode derived mode

;; Author:     Dino Chiesa
;; Modified:   December 2009
;; Version:    1.0
;; Keywords:   c# flymake

;; This code is distributed under the MS-Public License.
;; See http://opensource.org/licenses/ms-pl.html


;; ==================================================================
;;
;; This module provides tweaks to the flymake minor mode, to allow it to
;; work with csharp.
;;
;; Flymake is built-in to emacs.  It periodically compiles an active
;; buffer when the minor mode is enabled, and then flags or highlights
;; broken lines in the compile as you edit. It is analogous to the
;; red-squigglies you get in Visual Studio, highlighting syntax erros.
;;
;; This elisp module defines a set of tweaks of flymake, to allow it to
;; work with C# on Windows.  flymake-for-csharp depends on a makefile or
;; msbuild file of well-known name, with some well-known settings, in
;; order to work.
;;
;; last saved Time-stamp: <2009-December-28 21:56:32>
;;
;; ==================================================================
;;
;; To use:
;; 1. Add this to your csharp-mode-hook function:
;;       (flymake-mode)
;;
;;
;; 2. specify how you want to use flymake, via these variables:
;;
;;     flymake-for-csharp-netsdk-location
;;
;;     flymake-for-csharp-nmake-buildfile
;;
;;     flymake-for-csharp-msbuild-location
;;
;;     flymake-for-csharp-msbuild-buildfile
;;
;;     flymake-for-csharp-use-msbuild


;; There are 2 common ways to build C# files: nmake or msbuild.
;; flymake-for-csharp works with either.  You need specify a particular
;; well-known target in your makefile, or specify instructions in your
;; msbuild file, to allow flymake-for-csharp to work.
;;
;; If you want to use a makefile, set `flymake-for-csharp-use-msbuild' to
;; nil, and configure the `flymake-for-csharp-netsdk-location' variable
;; to specify the location of nmake.exe.  Usually it is in one of the
;; following places:
;;
;; - the .NET 2.0 SDK directory
;;   example: C:\Program Files\Microsoft SDKs\Microsoft .Net\v2.0
;; - the Visual Studio directory, if the VS install is the source of the .NET SDK
;;   example: c:\Program Files\Microsoft Visual Studio 8\SDK\v2.0
;;
;; Provide a make target like this in your makefile:
;;
;; check-syntax:
;;      $(_CSC) /t:module $(CHK_SOURCES)
;;
;;
;; The target name must be "check-syntax". The use of the _CSC symbol is
;; an example.  If you do it this way, be sure to define _CSC within the
;; makefile as the location of the csc.exe compiler. You can also
;; hard-code the location of the C# compiler if you like.  For .NET v3.5,
;; it is %windir%\Microsoft.NET\Framework\v3.5\csc.exe.  You must use
;; /target:netmodule, and you must use $(CHK_SOURCES) as the end of the
;; line.  This symbol (CHK_SOURCES) is defined on the command line when
;; emacs invokes nmake.exe.  It need not and should not be defined in
;; your makefile.
;;
;; If you don't want to put this statement into your makefile, you can
;; alternatively put it into a specially named makefile, for example
;; "makefile.flymake". Specify that filename in the variable
;; `flymake-for-csharp-nmake-buildfile'.
;;
;; ==================================================================
;;
;; If you use msbuild, rather than nmake, specify the location of
;; msbuild.exe via the `flymake-for-csharp-msbuild-location' variable,
;; and set `flymake-for-csharp-use-msbuild' to t.  You can specify the
;; location of the msbuild file used by flymake with
;; `flymake-for-csharp-msbuild-buildfile'.  By default it is
;; msbuild.flymake.xml.
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
;; edited.  In this case, your msbuild.flymake.xml file should look like
;; this:
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
;; compiled together.  If this is not the case, the modify the msbuild file
;; accordingly, to exclude the files as appropriate.  You can also specify
;; additional references, and so on.
;;


(require 'flymake)

(setq flymake-log-level 0)  ;; insure flymake errors get plopped into the *Messages* buffer
;; -1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"


;; flymake-gui-warnings-enabled
;; nil = turn off? GUI warnings, eg. when no msbuild.flymake.xml file found
;; t = turn on? GUI warnings
(setq flymake-gui-warnings-enabled nil)




;; These variables are ones I made up for help with C#:
(defvar flymake-for-csharp-netsdk-location "c:\\netsdk2.0"
  "Location of .NET SDK, for finding nmake.exe.  The nmake is found in the bin subdir.  Example value is: c:\\Program Files\\Microsoft Visual Studio 8\\SDK\\v2.0 . " )

(defvar flymake-for-csharp-nmake-buildfile "makefile"
  "Build file if using nmake.exe.  You can name this anything you like.
The makefile should have the check-syntax target contained within it. ")

(defvar flymake-for-csharp-msbuild-location "c:\\.net3.5"
  "Directory containing MSBuild.exe. Typically, c:\\windows\\Microsoft.NET\\Framework\\v3.5 .")

(defvar flymake-for-csharp-msbuild-buildfile "msbuild.flymake.xml"
  "Build file if using MSBuild.exe.  You can name this anything you like.
The file should have a target by the name of CheckSyntax contained within it.")

(defvar flymake-for-csharp-use-msbuild t
  "If t, then flymake uses msbuild.exe and the msbuild.flymake.xml
file.  If nil, then flymake uses nmake and the makefile with a
check-status target. Keep in mind the buildfile for either msbuild or nmake
is customizable.  See the vars flymake-for-csharp-{nmake,msbuild}-buildfile .")



(defun flymake-for-csharp-cleanup ()
  "Delete the temporary .netmodule file created in syntax checking,
then call through to flymake-simple-cleanup."
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



(defun flymake-for-csharp-buildfile ()
  (if flymake-for-csharp-use-msbuild
      flymake-for-csharp-msbuild-buildfile
    flymake-for-csharp-nmake-buildfile
    )
  )


(defun flymake-for-csharp-find-csharp-buildfile (source-file-name)
  (let ((actual-build-file-name (flymake-for-csharp-buildfile)))
    (if (file-exists-p (expand-file-name actual-build-file-name "."))
        "."
      (flymake-log 1 "no buildfile (%s) for %s" actual-build-file-name source-file-name)
      (flymake-report-fatal-status
       "NOMK" (format "No buildfile (%s) found for %s"
                      actual-build-file-name source-file-name))
      nil
      )

  )
  )


(defun flymake-for-csharp-init ()
  (flymake-for-csharp-init-impl 'flymake-create-temp-inplace t t  'flymake-get-make-cmdline))


(defun flymake-for-csharp-init-impl (create-temp-f use-relative-base-dir use-relative-source get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
         (source-file-name   buffer-file-name)
         (buildfile-dir      (flymake-for-csharp-find-csharp-buildfile source-file-name)))
    (if buildfile-dir
        (let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
          (setq args (flymake-get-syntax-check-program-args temp-source-file-name buildfile-dir
                                                            use-relative-base-dir use-relative-source
                                                            get-cmdline-f))))
    args))

;(debug-on-entry 'flymake-for-csharp-init)



; This fixup sets flymake to use a different cleanup routine for c# compiles
(defun flymake-for-csharp-fixup ()
  (let (elt
        (csharp-entry nil)
        (masks flymake-allowed-file-name-masks)
        )

    ;; The "flymake-allowed-file-name-masks" variable stores a filename pattern as
    ;; well as the make-init function, and a cleanup function.  In the case of csharp,
    ;; the setting in flymake.el has the cleanup fn as nil, which means it gets the
    ;; standard cleanup : the *_flymake.cs cloned source file gets deleted.  But the
    ;; way I have done the syntax checking, I compile the .cs file into a module,
    ;; which also needs to be deleted afterwards.
    ;;

    ;; Here, we remove the C# entry in the "flymake-allowed-file-name-masks"
    ;; variable, and replace it with an entry that includes a custom csharp cleanup
    ;; routine.  In that cleanup routine, I delete the .netmodule file.

    ;; I could just setq the "flymake-allowed-file-name-masks" var to the C# thing I
    ;; want, but that would obliterate all the masks for all other languages, which
    ;; would be bad manners.

    ;; You know, come to think of it, I could just delete the generated .netmodule
    ;; file in the msbuild or makefile.  That might be simpler.

    ;; But the main point is this ought to be more easily configurable or customizable
    ;; in flymake.el.  And also, flymake ought to do something reasonable for csharp builds,
    ;; rather than completely punt.

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

    ;;  remove the original one ...
    (if csharp-entry
        (setq flymake-allowed-file-name-masks
              (delete csharp-entry flymake-allowed-file-name-masks)))

    ;; Now add a new one, with the custom cleanup method.
    (setq flymake-allowed-file-name-masks
          (cons
           '("\\.cs\\'" flymake-for-csharp-init flymake-for-csharp-cleanup)
           flymake-allowed-file-name-masks))
    )
  )

; need to do this only once, not every time csharp-mode is invoked
(flymake-for-csharp-fixup)


; This method re-defines the defun shipped in flymake, for csharp.  Re-defining
; this function *will* definitely break flymake for all other languages.  One
; way to fix that problem is to make the "get-make-cmdline" function a
; configurable hook within flymake!

(defun flymake-get-make-cmdline (source base-dir)
  (if flymake-for-csharp-use-msbuild
      (list (concat  flymake-for-csharp-msbuild-location "\\msbuild.exe")
            (list (concat base-dir "/" (flymake-for-csharp-buildfile))
                  "/nologo"
                  "/t:CheckSyntax"
                  "/v:quiet"  ;; normal
                  ;; use file-relative-name to remove the fully-qualified directory name
                  (concat "/property:SourceFileToCheck=" (file-relative-name source))
                  (concat "/property:OriginalSourceFile=" (file-relative-name buffer-file-name))
                  ))

    (list (concat  flymake-for-csharp-netsdk-location "\\bin\\nmake.exe")
        (list "/f"
              (concat base-dir "/" (flymake-for-csharp-buildfile))
              (concat "CHK_SOURCES=" source)
              "SYNTAX_CHECK_MODE=1"
              "check-syntax"))

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'flymake-for-csharp)

;;; end of flymake-for-csharp.el
