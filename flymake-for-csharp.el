;;; flymake-for-csharp.el --- C# mode derived mode
;;
;; Author:     Dino Chiesa
;; Version:    1.2
;; Modified:   2010 May 10
;; Keywords:   c# flymake
;;
;; This code is distributed under the MS-Public License.
;; See http://opensource.org/licenses/ms-pl.html
;;
;; last saved
;; Time-stamp: <2010-May-11 09:56:31>


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
;;
;;    ...and by possibly adding special check-syntax targets in
;;    your makefile or msbuild file.
;;
;; Full details are provided  below.
;;
;;
;; Intro:
;;
;; Flymake is a language-neutral mode that periodically invokes a
;; source-code compiler to check the syntax of the currently-
;; being-edited buffer.
;;
;; flymake-for-csharp is a C#-specific extension of flymake-mode, and
;; provides the logic for invoking the C# compiler to do the syntax
;; check on the currently-being-edited buffer.
;;
;;
;; When the flymake mode timer fires, it calls into the
;; flymake-for-csharp logic.  flymake-for-csharp copies the contents of
;; the current C# buffer to a temporary source file, then invokes the C#
;; compiler on that temporary source file.  flymake-for-csharp can do
;; this in one of 3 ways:
;;
;; - using the csc.exe compiler directly;
;; - using a makefile and invoking nmake
;; - using a msbuild file and invoking msbuild.exe
;;
;;
;; Which option you choose depends on your requirements.  If the .cs
;; file you're editing is a standalone module, you can compile it with a
;; simple invocation of csc.exe.  If the module being edited must be
;; compiled with other source modules in order to compile correctly, you
;; will use nmake or msbuild, as you prefer.
;;
;; To allow flymake-for-csharp to work with either the nmake or msbuild
;; options, you need to specify a particular well-known target in your
;; makefile, or in your msbuild file.
;;
;; Now, how to select the compile option:
;;
;;
;; Option A: use the csc.exe compiler directly
;; ============================================
;;
;; This is the simplest option, and also the most limited. With this
;; option, flymake-for-csharp compiles the copy of the existing buffer
;; using the csc.exe command-line compiler, and the arguments you
;; provide, via the `flymake-for-csharp-csc-arguments' variable.
;; Appended to that list of arguments will be the name of a temporary
;; .cs file, with the contents of the currently-being-edited C# buffer.
;; You should not include the name of the current .cs file in the
;; `flymake-for-csharp-csc-arguments' variable, but you can specify the
;; names of other .cs files that should be compiled *with* the current
;; one.  The `flymake-for-csharp-csc-arguments' variable is only used
;; with the direct csc.exe build option; it is not used when
;; flymake-for-csharp invokes nmake or msbuild to do the syntax check.
;;
;; If you do no special setup, the direct csc.exe build will be used.
;;
;; Here's why: in deciding which build option to use, flymake-for-csharp
;; searches the list of build files in
;; `flymake-for-csharp-buildfile-alist' to find one appropriate for use
;; for syntax checking, and uses the first one it finds. If no
;; appropriate build files are found, flymake-for-csharp falls back to
;; invoking the csc.exe compiler directly.
;;
;; To determine if a build file is appropriate, flymake-for-csharp uses
;; these criteria:
;;
;;     1. the name of the makefile or msbuild file is on the special
;;        variable, `flymake-for-csharp-buildfile-alist'
;;
;;     2. the makefile or msbuild file contains a special target
;;        by a well-known name.  check-syntax for makefiles, and
;;        CheckSyntax for msbuild files.
;;
;; If you do *nothing*, then these criteria won't be satisfied, and
;; flymake-for-csharp will try to use csc.exe to check the syntax of
;; your C# buffer.
;;
;; If you want to explicitly insure that flymake-for-csharp will use
;; csc.exe to check the syntax of your C# buffer, then set
;; `flymake-for-csharp-buildfile-alist' to nil, OR, insure that none of
;; the build files in that list, exist in the directory where the .cs
;; file resides, OR if they do exist, that the required build targets
;; are not present in those build files.
;;
;; The compile-directly-with-csc.exe option is limited because
;; flymake-for-csharp will use the same arguments with all .cs buffers.
;; Often you want to compile .cs files differently.  Each project
;; usually requires a different set of assemblies, for instance.  If you
;; want to use flymake in a scenario like that, you'll want to rely on
;; the nmake.exe or msbuild.exe options for flymake-for-csharp.
;;
;;
;; Option B: use nmake and a makefile.
;; ============================================
;; If you want to use a makefile, you must:
;;
;; 1. configure the `flymake-for-csharp-netsdk-location' variable to
;;    specify the location of the bin directory that contains nmake.exe.
;;    Usually it is in one of the following places:
;;
;;    - the .NET 2.0 SDK directory
;;      example: C:\Program Files\Microsoft SDKs\Microsoft .Net\v2.0
;;    - the Visual Studio directory, if the VS install is the source of the .NET SDK
;;      example: c:\Program Files\Microsoft Visual Studio 9\SDK\v2.0
;;
;;    example:
;;      (setq flymake-for-csharp-netsdk-location "c:\\Program Files\\Microsoft Visual Studio 9\\SDK\\v2.0")
;;
;;    Notice that you should not append the bin subdir on the value of
;;    flymake-for-csharp-netsdk-location.
;;
;;
;; 2. set `flymake-for-csharp-buildfile-alist' to a list of filespecs
;;    for names of makefiles. You can use wildcards.  The name of the
;;    makefile must have the word "makefile" in it, or should end with
;;    .mk.  flymake-for-csharp looks for files from the alist, in the
;;    same directory as the currently-being-edited .cs file, where the
;;    filename fits the constraint just described.
;;
;;    If the makefile exists, flymake-for-csharp searches within the
;;    file for a make target by the name of "check-syntax" .  (This fixed
;;    name is consistent with the use of flymake for C language source
;;    files.).  If the makefile exists and contains the check-syntax make
;;    target, flymake-for-csharp runs nmake.exe on it to check the
;;    syntax of the currently-being-edited .cs file.
;;
;;    By default, `flymake-for-csharp-buildfile-alist' contains "makefile",
;;    "flymake.mk" and "makefile.flymake".  If you put your check-syntax
;;    target in a file by one of those names, then you won't need to
;;    change the value of `flymake-for-csharp-buildfile-alist' .
;;
;;
;;
;; 3. Create the check-syntax target in the makefile. In the simplest
;;    case, you compile only a single source module at a time.  The
;;    target block to do syntax checking would look something like so:
;;
;;        check-syntax:
;;           %windir%\Microsoft.NET\Framework\v3.5\csc.exe /t:module $(FLYMAKE_CHECK)
;;
;;    This works for standalone source modules; those that do not depend
;;    on any other source modules.  If your source module depends on
;;    particular assemblies, you could insert them on the CSC command line,
;;    using the /R option, like so:
;;
;;        check-syntax:
;;           $(_CSC) /t:module /R:foo.dll $(FLYMAKE_CHECK)
;;
;;
;;    The target name MUST be "check-syntax". In the command that runs
;;    for that target, you can run whatever commands you
;;    like. Typically, you will invoke the c# compiler as appropriate.
;;    You can use a make variable like $(_CSC) if one is defined in your
;;    makefile.  If you use /target:netmodule, flymake-for-csharp will
;;    delete any temporary .netmodule files that are created as part of
;;    the syntax check.  If you don't, if for example you want to create
;;    a DLL or EXE for some reason, you'll need to do your own cleanup
;;    in the command block.
;;
;;    Flymake invokes nmake with several nmake macros defined on the
;;    command line:
;;
;;        FLYMAKE_CHECK  - the name of the file to check.  This is a source
;;                 file with a temporary name, copied from the current
;;                 state of the buffer.  If you are editing Module.cs, then
;;                 the value of this macro will be  Module_flymake.cs
;;
;;        FLYMAKE_ORIGINAL  - the name of the original file that is being
;;                 checked for syntax.  In the above example, it will be
;;                 Module.cs
;;
;;        FLYMAKE_SYNTAX_CHECK - set to 1
;;
;;    That covers the simple case. In more complex cases, there are a
;;    set of other source files in a project that get compiled to a
;;    single DLL, for example.  When editing one of those files, you'll
;;    need to *exclude* the FLYMAKE_ORIGINAL file, and to *include* the
;;    FLYMAKE_CHECK file, in the check-syntax make target. This can be
;;    done with nmake macros, and inline files.
;;
;;    For example, suppose you have a project that compiles 3 source
;;    files, Fribble,cs Zambda.cs Twoolie.cs, into a DLL. Regardless
;;    which file you are currently editing in emacs, you want to compile
;;    *the other two* along with the temporary copy of the currently-
;;    being-edited file into a netmodule. You can do that like so:
;;
;;        CS_SOURCE=Fribble.cs Zambda.cs Twoolie.cs
;;             ....
;;        check-syntax :
;;                <<flymake-build.cmd  $(CS_SOURCE)
;;            SETLOCAL ENABLEDELAYEDEXPANSION
;;            for %%I in (%*) do if NOT %%I == $(FLYMAKE_ORIGINAL) (
;;               set filesToBuild=!filesToBuild! %%I
;;            )
;;            $(_CSC) /t:module $(FLYMAKE_CHECK) !filesToBuild!
;;            ENDLOCAL
;;        <<
;;
;;
;;    Given that target in the makefile, all files EXCLUDING the
;;    currently-being-edited file, but INCLUDING the temporary copy of
;;    the currently-being-edited file, are compiled into a module.  The
;;    double-angle-bracket signifies the use of a little-known feature
;;    of nmake called in-line files.  The above nmake snippet causes
;;    nmake to create a temporary .cmd file, and invoke it.  The .cmd
;;    file includes the logic to exclude the original C# file from the
;;    build, and include the temporary C# file into the build.
;;
;;    Putting the $(FLYMAKE_CHECK) file first on the csc line causes
;;    the generated .netmodule file to have a name that allows
;;    flymake-for-csharp to find it and delete it when flymake
;;    finishes.
;;
;;    If you don't want to put a check-syntax make target into your main
;;    makefile, you can alternatively put it into an alternative
;;    makefile, for example "makefile.flymake" or "flymake.mk". Insure
;;    that the filename you use is included as one element in the
;;    variable `flymake-for-csharp-buildfile-alist'.
;;
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
;; and insure that `flymake-for-csharp-buildfile-alist' contains a spec
;; for a buildfile that contains the well-known target, CheckSyntax.
;;
;; As described above, Wildcards are allowed in the elements in the alist.
;; After wildcard expansion, for each
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
;; Flymake invokes msbuild with several properties defined on the
;; command line:
;;
;;     SourceFileToCheck - the name of the file to check.  This is a source
;;                 file with a temporary name, copied from the current
;;                 state of the buffer.  If you are editing Module.cs, then
;;                 the value of this property will be  Module_flymake.cs
;;
;;     OriginalSourceFile  - the name of the original file that is being
;;                 checked for syntax.  In the above example, it will be
;;                 Module.cs
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
;; (This msbuild file will work only with .NET 3.5 and later.)  Name
;; that build file according to the constraints described above, and put
;; the name of the file on `flymake-for-csharp-buildfile-alist' .
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
;; get fancier.  The way flymake works is, it copies the current
;; contents of the buffer to a temporary source file, then compiles it.
;; Therefore in the case where you are compiling multiple source files
;; together, you need to compile all files, *including* the temporary
;; copy of the file being edited, but *excluding* the actual source file
;; being edited. This is easily done in msbuild with a
;; specially-structured Exclude qualifier, referencing the
;; OriginalSourceFile property. Your msbuild.flymake.xml file should
;; look something like this:
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
  (list "makefile" "makefile.flymake" "flymake.mk" "*.flymake.xml"
        "*.csproj")
  "A list of build files that flymake should look for. Wildcards
are allowed. For each filename in this list, flymake-for-csharp
will check the existence of the file in the local directory.  If
the file exists, it will determine if the file has the
appropriate target, then invoke the associated build tool.  If
the filename is 'makefile' or begins with 'makefile' or ends in
.mk, flymake-for-csharp will try to use the file with nmake.  If
the filename is '*.csproj' or 'msbuild.*' or ends in '.xml',
flymake will try to use the build file with msbuild.  The build
file you specify should have the check-syntax target contained
within it. If none of the build files on the list seem
appropriate, then flymake-for-csharp will resort to directly
compiling the current file by itself, using csc.exe.
" )


(defvar flymake-for-csharp-csc-arguments
  (list "/t:module" "/nologo")
  "A list of arguments to use with the csc.exe
compiler, when using flymake-for-csharp with a
direct csc.exe build for syntax checking purposes.")



(defvar flymake-for-csharp-most-recent-cmd  nil
  "The most recent command line used to run flymake
for the current csharp buffer.

Use this to figure out what flymake-for-csharp decided
to do, given your setup, variable settings, and the value
of `flymake-for-csharp-buildfile-alist'.

The value is possily nil, if flymake
has never been run on the buffer.
")



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



(defun string-starts-with (s arg)
  "returns t if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
         (string-equal (substring s 0 (length arg)) arg))
        (t nil)))



(defun flymake-for-csharp-get-csc-arguments ()
  "gets the args for csc.exe.  You might think this could just be a variable
reference, but it's packaged as a function to allow advice to override it.
In particular, the flymake-for-csharp-ext.el package overrides this to
provide a list of /R arguments, corresponding to the using statements in
the source file.  That extension ( flymake-for-csharp-ext.el) depends on
the CSDE package, and not everybody has CSDE installed, or wants it.
So it remains an extension, and this needs to be a function.

This func also does checking to verify the /t:module is used in the arglist,
and burps if a different /t argument is found."

  (flymake-log 3 "flymake-for-csharp-get-csc-arguments: entry")
  (let ((args flymake-for-csharp-csc-arguments)
        arg
        (found nil))
    (flymake-log 3 "flymake-for-csharp-get-csc-arguments: args: %s" args)
    (while args
      (setq arg (car args))
      (cond
       ((string-equal arg "/t:module") (setq found t))
       ((string-starts-with arg "/t:")
        (setq found t)
        (message "flymake-for-csharp: WARNING /t: option present, and not /t:module; fix this.")))

      (setq args (cdr args)))
    (if found
        (progn
          (flymake-log 3 "flymake-for-csharp-get-csc-arguments: return %s" flymake-for-csharp-csc-arguments)
          flymake-for-csharp-csc-arguments)

      (flymake-log 1 "flymake-for-csharp-get-csc-arguments: appending /t:module")
      (setq args
            (append flymake-for-csharp-csc-arguments (list "/t:module"))))))



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




;;(defun flymake-get-make-cmdline (source base-dir)
(defun flymake-for-csharp-get-flymake-cmdline (source base-dir)
"Gets the cmd line for running a flymake session in a csharp buffer.
 It will invoke one of three programs: csc.exe, msbuild.exe, or nmake.exe,
 depending on the state of the filesystem.  If an appropriate build file,
 suitable for use with either nmake or msbuild, is found on the
 `flymake-for-csharp-buildfile-alist', then the appropriate build tool
 (msbuild or nmake) is invoked on that build file. If no appropriate build
 file is found, then csc.exe is invoked"
(setq flymake-for-csharp-most-recent-cmd
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
                  "FLYMAKE_SYNTAX_CHECK=1"
                  (concat "FLYMAKE_CHECK=" source)
                  (concat "FLYMAKE_ORIGINAL=" (file-relative-name buffer-file-name))
                  "check-syntax")))
     (t
      (list (concat  flymake-for-csharp-dotnet-location "\\csc.exe")
        (append (flymake-for-csharp-get-csc-arguments) (list source))))))))




; This fixup sets flymake to use a different cleanup routine for c# compiles.
; need to do this only once, not every time csharp-mode is invoked.

(progn
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
    ;; routine.  That cleanup routine deletes the .netmodule file, generated
    ;; by a successful flymake run.

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




;; =======================================================
;;
;; This section attempts to workaround some anomalous display ebhavior
;; for tooltips.  It's not strictly necessary, only aesthetic.  The issue is that
;; tooltips can get clipped.  This is the topic of Emacs bug #5908, unfixed
;; in v23 and present in v22.

(defun cheeso-reverse-string (s)
  "Reverse a string."
  (coerce (reverse (loop for b across s
                         collect b))
          'string))

(defun cheeso-string-trim (s &rest chars)
  "Trim any char in string CHARS from either end of string S.
Often this fn is called with a literal space, as with
(cheeso-string-trim my-string ?\ ) ."
  (apply 'cheeso-string-trim-right
         (apply 'cheeso-string-trim-left s chars)
         chars))

(defun cheeso-string-trim-left (s &rest chars)
  "Trim any char in string CHARS from the left of string S."
  (let ((idx (dotimes (i (length s))
               (unless (member (elt s i) chars)
                 (return i)))))
    (if idx
        (subseq s idx)
      "")))

(defun cheeso-string-trim-right (s &rest chars)
  "Trim any char in string CHARS from the right of string S."
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
