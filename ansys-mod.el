;;; ansys-mod.el --- editing Ansys log files under Emacs

;;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Tim Read <Tim.Read@fp.co.nz>
;; Author: Geoff Foster <fosterg@fp.co.nz>
;; Maintainer: Tim Read <Tim.Read@fp.co.nz>
;; Keywords: languages

;;
;; This file has used code from octave-mod.el 
;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
;; Author: John Eaton <jwe@bevo.che.wisc.edu>
;;

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides Emacs support for Ansys.
;; It defines Ansys mode, a major mode for editing Ansys log files.

;;; Code:

(defconst ansys-maintainer-address
  "Tim Read <Tim.Read@fp.co.nz> " 
  "Current maintainer of the Emacs Ansys package.")

;; Extra highlighting of matching parenthesis (or brace or bracket)
(require 'paren)

(defvar ansys-abbrev-table nil
  "Abbrev table for Ansys's reserved words.
All Ansys abbrevs start with a grave accent (`).")
(if ansys-abbrev-table
    ()
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'ansys-abbrev-table ())
    (define-abbrev ansys-abbrev-table "`i" "*IF,,LT,,THEN" nil)
    (define-abbrev ansys-abbrev-table "`eli" "*ELSEIF," nil)
    (define-abbrev ansys-abbrev-table "`el" "*ELSE\n" nil)
    (define-abbrev ansys-abbrev-table "`ei" "*ENDIF\n" nil)
    (define-abbrev ansys-abbrev-table "`cy" "*CYCLE" nil)
    (define-abbrev ansys-abbrev-table "`d" "*DO,,1,10,1\n\n*ENDDO" nil)
    (define-abbrev ansys-abbrev-table "`ex" "*EXIT" nil)
    (define-abbrev ansys-abbrev-table "`g" "*GO" nil)
    (define-abbrev ansys-abbrev-table "`cr" "*CREATE\n\n*END" nil)
    (define-abbrev ansys-abbrev-table "`e" "*END" nil)
    (define-abbrev ansys-abbrev-table "`rp" "/REPLOT" nil)
    (define-abbrev ansys-abbrev-table "`t" "/TITLE," nil)
    (setq abbrevs-changed ac)))

(defvar ansys-comment-char ?!
  "Character to start an Ansys comment.")

(defvar ansys-indent-comment
  "(concat ansys-comment-char " ")
  "if you use emacs<21 uncomment line above and comment the following line"
  (format "%c " ansys-comment-char)
  "String to insert to start a new Ansys indented comment.")

(defvar ansys-comment-start-skip "\\s<+\\s-*"
  "Regexp to match the start of an Ansys comment up to its body.")

(defvar ansys-begin-keywords
  '("*DO" "*do" "*IF" "*if" "*CREATE" "*create"
    ))
(defvar ansys-else-keywords
  '("*ELSEIF" "*elseif" "*ELSE" "*else"
    ))
(defvar ansys-end-keywords
  '("*ENDDO" "*enddo" "*ENDIF" "*endif" "*END" "*end"
    ))

(defvar ansys-command-keywords
  '(
    "A" "AADD" "AATT" "*ABBR" "ABBRES" "ABBSAV" "ABS" "ACCAT" "ACEL" "ACLEAR" "ADAPT" "ADD" "ADDAM" "ADELE" "ADGL" "ADRAG" "AFILLT" "AFLIST" "AFSURF" "*AFUN" "AGEN" "AGLUE" "AINA" "AINP" "AINV" "AL" "ALIST" "ALPFILL" "ALLSEL" "ALPHAD" "AMAP" "AMESH" "ANCNTR" "ANCUT" "ANDATA" "ANDSCL" "ANDYNA" "/ANFILE" "ANFLOW" "/ANGLE" "ANIM" "ANISOS" "ANMODE" "/ANNOT" "ANORM" "ANTIME" "ANTYPE" "/ANUM" "AOFFST" "AOVLAP" "APLOT" "APPEND" "APTN" "ARCLEN" "ARCOLLAPSE" "ARCTRM" "ARDETACH" "AREAS" "AREFINE" "AREVERSE" "ARFILL" "ARMERGE" "AROTAT" "ARSCALE" "ARSPLIT" "ARSYM" "ASBA" "ASBL" "ASBV" "ASBW" "ASEL" "*ASK" "ASKIN" "ASLL" "ASLV" "/ASSIGN" "ASUB" "ASUM" "ATAN" "ATRAN" "ATYPE" "/AUTO" "AUTOTS" "/AUX2" "/AUX12" "/AUX15" "AVPRIN" "AVRES" "/AXLAB"

    "/BATCH" "BELLOW" "BEND" "BETAD" "BF" "BFA" "BFADELE" "BFALIST" "BFCUM" "BFDELE" "BFE" "BFECUM" "BFEDELE" "BFELIST" "BFESCAL" "BFINT" "BFK" "BFKDELE" "BFKLIST" "BFL" "BFLDELE" "BFLIST" "BFLLIST" "BFSCALE" "BFTRAN" "BFUNIF" "BFV" "BFVDELE" "BFVLIST" "BIOOPT" "BIOT" "BLC4" "BLC5" "BLOCK" "BOOL" "BOPTN" "BRANCH" "BSPLIN" "BTOL" "BUCOPT"
    
    "C***" "CALC" "CBDOF" "CDREAD" "CDWRITE" "CE" "CECMOD" "CECYC" "CEDELE" "CEINTF" "CELIST" "CENTER" "CEQN" "CERIG" "CESGEN" "CFACT" "*CFCLOS" "*CFOPEN" "*CFWRITE" "CGLOC" "CGOMGA" "CHECK" "CHKMSH" "CIRCLE" "/CLABEL" "/CLEAR" "CLOCAL" "/CLOG" "CLOG" "CLRMSHLN" "CM" "/CMAP" "CMDELE" "CMEDIT" "CMGRP" "CMLIST" "CMPLOT" "CMSEL" "CNVTOL" "/COLOR" "/COM" "CON4" "CONE" "/CONFIG" "CONJUG" "/CONTOUR" "/COPY" "COUPLE" "COVAL" "CP" "CPDELE" "CPINTF" "/CPLANE" "CPLGEN" "CPLIST" "CPNGEN" "CPSGEN" "CQC" "*CREATE" "CRPLIM" "CS" "CSCIR" "CSDELE" "CSKP" "CSLIST" "CSWPLA" "CSYS" "/CTYPE" "CURR2D" "CUTCONTROL" "/CVAL" "CVAR" "CYCGEN" "*CYCLE" "CYCSOL" "CYL4" "CYL5" "CYLIND"
    
    "D" "DA" "DADELE" "DALIST" "DATA" "DATADEF" "DCGOMG" "DCUM" "DDELE" "DEACT" "DEFINE" "*DEL" "/DELETE" "DELTIM" "DERIV" "DESIZE" "DESOL" "DETAB" "/DEVDISP" "/DEVICE" "DIG" "DIGIT" "*DIM" "DISPLAY" "/DIST" "DK" "DKDELE" "DKLIST" "DL" "DLDELE" "DLIST" "DLLIST" "DMOVE" "DMPRAT" "DNSOL" "*DO" "DOF" "DOFSEL" "DOMEGA" "/DSCALE" "DSCALE" "DSET" "DSUM" "DSURF" "DSYM" "DSYS" "DTRAN" "DUMP" "/DV3D" "DYNOPT"
    
    "E" "EALIVE" "EDBOUND" "EDBVIS" "EDCDELE" "EDCGEN" "EDCLIST" "EDCONTACT" "EDCPU" "EDCRB" "EDCSC" "EDCTS" "EDCURVE" "EDDAMP" "EDDRELAX" "EDELE" "EDENERGY" "EDFPLOT" "/EDGE" "EDHGLS" "EDHIST" "EDHTIME" "EDINT" "EDIVELO" "EDLCS" "EDLDPLOT" "EDLOAD" "EDMP" "EDNDTSD" "EDNROT" "EDOPT" "EDOUT" "EDREAD" "EDRST" "EDSHELL" "EDSOLV" "EDSTART" "EDWELD" "EDWRITE" "/EFACET" "EGEN" "EINTF" "EKILL" "ELEM" "ELIST" "*ELSE" "*ELSEIF" "EMAGERR" "EMF" "EMID" "EMIS" "EMODIF" "EMORE" "EMSYM" "EMUNIT" "EN" "*END" "*ENDDO" "*ENDIF" "ENGEN" "ENORM" "ENSYM" "/EOF" "EPLOT" "EQSLV" "/ERASE" "ERASE" "EREAD" "EREFINE" "ERESX" "ERNORM" "ERRANG" "ESEL" "/ESHAPE" "ESIZE" "ESLA" "ESLL" "ESLN" "ESLV" "ESOL" "ESORT" "ESTIF" "ESURF" "ESYM" "ESYS" "ET" "ETABLE" "ETCHG" "ETDELE" "ETLIST" "ETYPE" "EUSORT" "*EVAL" "EWRITE" "*EXIT" "/EXIT" "EXP" "/EXPAND" "EXPAND" "EXPASS" "EXPSOL" "EXTOPT" "EXTREM"
    
    "F" "/FACET" "FATIGUE" "FCUM" "/FDELE" "FDELE" "FE" "FEBODY" "FECONS" "FEFOR" "FELIST" "FESURF" "FILE" "FILEAUX2" "FILEDISP" "FILL" "FILLDATA" "/FILNAME" "FINISH" "FITEM" "FK" "FKDELE" "FKLIST" "FL" "FLANGE" "FLDATA" "FLDATA1" "FLDATA2" "FLDATA3" "FLDATA4" "FLDATA4A" "FLDATA5" "FLDATA6" "FLDATA7" "FLDATA8" "FLDATA9" "FLDATA10" "FLDATA11" "FLDATA12" "FLDATA13" "FLDATA14" "FLDATA15" "FLDATA16" "FLDATA17" "FLDATA18" "FLDATA19" "FLDATA20" "FLDATA20A" "FLDATA21" "FLDATA22" "FLDATA23" "FLDATA24" "FLDATA24A" "FLDATA24B" "FLDATA24C" "FLDATA24D" "FLDATA25" "FLDATA26" "FLDATA27" "FLDATA28" "FLDATA29" "FLDATA30" "FLDATA31" "FLDATA32" "FLDATA33" "FLIST" "FLLIST" "FLOCHECK" "FLOTRAN" "FLREAD" "FLST" "FLUXV" "FMAGBC" "FMAGSUM" "/FOCUS" "FOR2D" "FORCE" "FORM" "/FORMAT" "FP" "FPLIST" "FREQ" "FS" "FSCALE" "FSDELE" "FSLIST" "FSNODE" "FSPLOT" "FSSECT" "FSUM" "FTCALC" "FTRAN" "FTSIZE" "FTWRITE" "/FTYPE" "FVMESH"
    
    "GAP" "GAPF" "GAPFINISH" "GAPLIST" "GAPMERGE" "GAPOPT" "GAPPLOT" "GAUGE" "GCGEN" "/GCMD" "/GCOLUMN" "GENOPT" "GEOM" "GEOMETRY" "*GET" "/GFILE" "/GFORMAT" "/GLINE" "/GMARKER" "*GO" "/GO" "/GOLIST" "/GOPR" "GP" "GPDELE" "GPLIST" "GPLOT" "/GRAPHICS" "/GRESUME" "/GRID" "/GROPT" "GRP" "/GRTYP" "/GSAVE" "/GST" "GSUM" "/GTHK" "/GTYPE"
    
    "HARFRQ" "/HEADER" "HELP" "HELPDISP" "HFSWEEP" "HMAGSOLV" "HPGL" "HPTCREATE" "HPTDELETE" "HREXP" "HROPT" "HROUT"
    
    "IC" "ICDELE" "ICLIST" "*IF" "IGESIN" "IGESOUT" "IMAGIN" "IMMED" "IMPD" "/INPUT" "INRES" "INRTIA" "INT1" "INTSRF" "IOPTN" "IRLF" "IRLIST"
    
    "K" "KATT" "KBC" "KBETW" "KCALC" "KCENTER" "KCLEAR" "KDELE" "KDIST" "KESIZE" "KEYOPT" "KEYPTS" "KEYW" "KFILL" "KGEN" "KL" "KLIST" "KMESH" "KMODIF" "KMOVE" "KNODE" "KPLOT" "KPSCALE" "KREFINE" "KSCALE" "KSCON" "KSEL" "KSLL" "KSLN" "KSUM" "KSYMM" "KTRAN" "KUSE" "KWPAVE" "KWPLAN"
    
    "L" "L2ANG" "L2TAN" "LANG" "/LARC" "LARC" "LAREA" "LARGE" "LATT" "LAYER" "LAYERP26" "LAYLIST" "LAYPLOT" "LCABS" "LCASE" "LCCALC" "LCCAT" "LCDEF" "LCFACT" "LCFILE" "LCLEAR" "LCOMB" "LCOPER" "LCSEL" "LCSL" "LCWRITE" "LCZERO" "LDELE" "LDIV" "LDRAG" "LDREAD" "LESIZE" "LEXTND" "LFILLT" "LFSURF" "LGEN" "LGLUE" "LGWRITE" "/LIGHT" "LINA" "/LINE" "LINE" "LINES" "LINL" "LINP" "LINV" "*LIST" "LLIST" "LMATRIX" "LMESH" "LNCOLLAPSE" "LNDETACH" "LNFILL" "LNMERGE" "LNSPLIT" "LNSRCH" "LOCAL" "LOVLAP" "LPLOT" "LPTN" "LREFINE" "LREVERSE" "LROTAT" "LSBA" "LSBL" "LSBV" "LSBW" "LSCLEAR" "LSDELE" "LSEL" "LSLA" "LSLK" "LSOPER" "/LSPEC" "LSREAD" "LSSCALE" "LSSOLVE" "LSTR" "LSUM" "LSWRITE" "/LSYMBOL" "LSYMM" "LTAN" "LTRAN" "LUMPM" "LVSCALE" "LWPLAN"
    
    "M" "MAGOPT" "MAGSOLV" "MASTER" "MAT" "MATER" "MDAMP" "MDELE" "/MENU" "MESHING" "*MFOURI" "*MFUN" "MGEN" "MITER" "MLIST" "MMF" "MODE" "MODMSH" "MODOPT" "MONITOR" "*MOONEY" "*MOPER" "MOPT" "MOVE" "MP" "MPAMOD" "MPCHG" "MPDATA" "MPDELE" "MPDRES" "/MPLIB" "MPLIST" "MPMOD" "MPUNDO" "MPPLOT" "MPREAD" "MPRINT" "MPTEMP" "MPTGEN" "MPTRES" "MPWRITE" "/MREP" "MSADV" "MSCAP" "MSDATA" "*MSG" "MSHAPE" "MSHKEY" "MSHMID" "MSHPATTERN" "MSMETH" "MSNOMF" "MSPROP" "MSQUAD" "MSRELAX" "MSSOLU" "MSSPEC" "/MSTART" "MSTERM" "MSVARY" "MXPAND"
    
    "N" "NANG" "NCNV" "NDELE" "NDIST" "NEQIT" "/NERR" "NFORCE" "NGEN" "NKPT" "NLGEOM" "NLIST" "NLOG" "NLOPT" "NMODIF" "NOCOLOR" "NODES" "/NOERASE" "/NOLIST" "NOORDER" "/NOPR" "/NORMAL" "NPLOT" "NPRINT" "NREAD" "NREFINE" "NRLSUM" "NROPT" "NROTAT" "NRRANG" "NSCALE" "NSEL" "NSLA" "NSLE" "NSLK" "NSLL" "NSLV" "NSOL" "NSORT" "NSTORE" "NSUBST" "NSVR" "NSYM" "/NUMBER" "NUMCMP" "NUMEXP" "NUMMRG" "NUMOFF" "NUMSTR" "NUMVAR" "NUSORT" "NWPAVE" "NWPLAN" "NWRITE"
    
    "OMEGA" "OPADD" "OPANL" "OPCLR" "OPDATA" "OPDEL" "OPEQN" "OPERATE" "OPEXE" "OPFACT" "OPFRST" "OPGRAD" "OPKEEP" "OPLFA" "OPLGR" "OPLIST" "OPLOOP" "OPLSW" "OPMAKE" "OPNCONTROL" "OPPRNT" "OPRAND" "OPRESU" "OPRFA" "OPRGR" "OPRSW" "OPSAVE" "OPSEL" "OPSUBP" "OPSWEEP" "/OPT" "OPTYPE" "OPUSER" "OPVAR" "OUTOPT" "OUTPR" "/OUTPUT" "OUTRES"
    
    "PADELE" "/PAGE" "PAGET" "PAPUT" "PARESU" "PARRES" "PARSAV" "PASAVE" "PATH" "/PBC" "/PBF" "PCALC" "PCIRC" "/PCIRCLE" "PCONV" "/PCOPY" "PCORRO" "PCROSS" "PDEF" "PDOT" "PDRAG" "PERBC2D" "PEXCLUDE" "PFACT" "PFLUID" "PGAP" "PHYSICS" "PINCLUDE" "PINSUL" "PIPE" "PLANEWAVE" "PLCONV" "PLCPLX" "PLCRACK" "PLDISP" "PLESOL" "PLETAB" "PLF2D" "PLLS" "PLNSOL" "/PLOPTS" "PLOT" "PLOTTING" "PLPAGM" "PLPATH" "PLSECT" "PLTIME" "PLTRAC" "PLVAR" "PLVAROPT" "PLVECT" "/PMACRO" "PMAP" "/PMETH" "PMETH" "PMGTRAN" "PMOPTS" "/PMORE" "/PNUM" "POINT" "POLY" "/POLYGON" "POPT" "PORTOPT" "/POST1" "/POST26" "POWERH" "PPATH" "PPLOT" "PPRANGE" "PPRES" "PRANGE" "PRCONV" "PRCPLX" "PRECISION" "PRED" "/PREP7" "PRERR" "PRESOL" "PRETAB" "PRI2" "PRIM" "PRINT" "PRISM" "PRITER" "PRNLD" "PRNSOL" "PROD" "PRPATH" "PRRFOR" "PRRSOL" "PRSECT" "PRSSOL" "PRTIME" "PRVAR" "PRVAROPT" "PRVECT" "PSCR" "PSDCOM" "PSDFRQ" "PSDRES" "PSDSPL" "PSDUNIT" "PSDVAL" "PSDWAV" "/PSEARCH" "PSEL" "/PSF" "PSOLVE" "/PSPEC" "PSPEC" "PSPRNG" "/PSTATUS" "PSTRES" "/PSYMB" "PTEMP" "PTXY" "PUNIT" "PVECT" "/PWEDGE"
    
    "QDVAL" "QFACT" "QUAD" "/QUIT" "QUOT" "R" "RACE" "RALL" "RAPPND" "/RATIO" "RBE3" "RCON" "RDELE" "REAL" "REALVAR" "RECTNG" "REDUCE" "REFLCOEF" "/RENAME" "REORDER" "*REPEAT" "/REPLOT" "/RESET" "RESET" "RESP" "RESUME" "REXPORT" "RFILSZ" "RFORCE" "/RGB" "RIGID" "RIMPORT" "RITER" "RLIST" "RMEMRY" "RMODIF" "RMORE" "ROCK" "RPOLY" "RPR4" "RPRISM" "RPSD" "RSPEED" "RSTAT" "RSYS" "RTIMST" "RUN" "/RUNST" "RWFRNT"
    
    "SABS" "SADD" "SALLOW" "SARPLOT" "SAVE" "SBCLIST" "SBCTRAN" "SDELETE" "SE" "SECDATA" "/SECLIB" "SECNUM" "SECOFFSET" "SECPLOT" "SECREAD" "SECTYPE" "SECWRITE" "SED" "SEDLIST" "SEEXP" "/SEG" "SELIST" "SELM" "SENERGY" "SEOPT" "SESYMM" "*SET" "SET" "SETRAN" "SEXP" "SF" "SFA" "SFACT" "SFADELE" "SFALIST" "SFBEAM" "SFCALC" "SFCUM" "SFDELE" "SFE" "SFEDELE" "SFELIST" "SFFUN" "SFGRAD" "SFL" "SFLDELE" "SFLIST" "SFLLIST" "SFSCALE" "SFTRAN" "/SHADE" "SHELL" "/SHOW" "/SHOWDISP" "SHPP" "/SHRINK" "SLIST" "SLPPLOT" "SLSPLOT" "SMALL" "SMAX" "SMBODY" "SMCONS" "SMFOR" "SMIN" "SMRTSIZE" "SMSURF" "SMULT" "SOLCONTROL" "/SOLU" "SOLU" "SOLUOPT" "SOLVE" "SORT" "SOURCE" "SPACE" "SPARM" "SPEC" "SPH4" "SPH5" "SPHERE" "SPLINE" "SPOINT" "SPOPT" "SPREAD" "SPTOPT" "SQRT" "SRCS" "SRSS" "/SSCALE" "SSLN" "SSTIF" "SSUM" "STAT" "*STATUS" "/STATUS" "STEF" "/STITLE" "STORE" "SUBOPT" "SUBSET" "SUMTYPE" "SV" "SVTYP" "/SYP" "/SYS"
    
    "TALLOW" "TB" "TBCOPY" "TBDATA" "TBDELE" "TBLE" "TBLIST" "TBMODIF" "TBPLOT" "TBPT" "TBTEMP" "TCHG" "TEE" "TERM" "TIME" "TIMERANGE" "TIMINT" "TIMP" "TINTP" "/TITLE" "/TLABEL" "TOFFST" "TOPDEF" "TOPEXE" "TOPITER" "TORQ2D" "TORQC2D" "TORQSUM" "TORUS" "TOTAL" "TRANS" "TRANSFER" "*TREAD" "TREF" "/TRIAD" "/TRLCY" "TRNOPT" "TRPDEL" "TRPLIS" "TRPOIN" "TRTIME" "TSHAP" "/TSPEC" "TSRES" "TUNIF" "TVAR" "/TYPE" "TYPE"
    
    "/UCMD" "/UI" "UIMP" "/UIS" "*ULIB" "/UNITS" "UPCOORD" "UPGEOM" "*USE" "/USER" "USRCAL"
    
    "V" "VA" "*VABS" "VADD" "VALVE" "VARDEL" "VARNAM" "VATT" "VCLEAR" "*VCOL" "/VCONE" "VCROSS" "*VCUM" "VCVFILL" "VDDAM" "VDELE" "VDGL" "VDOT" "VDRAG" "*VEDIT" "VEXT" "*VFACT" "*VFILL" "*VFUN" "VGEN" "*VGET" "VGET" "VGLUE" "/VIEW" "VIMP" "VINP" "VINV" "*VITRP" "*VLEN" "VLIST" "VLSCALE" "*VMASK" "VMESH" "VOFFST" "VOLUMES" "*VOPER" "VOVLAP" "*VPLOT" "VPLOT" "VPTN" "*VPUT" "VPUT" "*VREAD" "VROTAT" "VSBA" "VSBV" "VSBW" "/VSCALE" "*VSCFUN" "VSEL" "VSLA" "*VSTAT" "VSUM" "VSWEEP" "VSYMM" "VTRAN" "VTYPE" "/VUP" "*VWRITE"
    
    "/WAIT" "WAVES" "WERASE" "WFRONT" "/WINDOW" "WMORE" "WPAVE" "WPCSYS" "WPLANE" "WPOFFS" "WPROTA" "WPSTYL" "WRITE" "WSORT" "WSTART" "/XRANGE" "XVAR" "XVAROPT"
    
    "/YRANGE"
    
    "/ZOOM"
   )
  "Commands in Ansys")

(defvar ansys-downcase-comand-keywords
  '(
    "a" "aadd" "aatt" "*abbr" "abbres" "abbsav" "abs" "accat" "acel" "aclear" "adapt" "add" "addam" "adele" "adgl" "adrag" "afillt" "aflist" "afsurf" "*afun" "agen" "aglue" "aina" "ainp" "ainv" "al" "alist" "alpfill" "allsel" "alphad" "amap" "amesh" "ancntr" "ancut" "andata" "andscl" "andyna" "/anfile" "anflow" "/angle" "anim" "anisos" "anmode" "/annot" "anorm" "antime" "antype" "/anum" "aoffst" "aovlap" "aplot" "append" "aptn" "arclen" "arcollapse" "arctrm" "ardetach" "areas" "arefine" "areverse" "arfill" "armerge" "arotat" "arscale" "arsplit" "arsym" "asba" "asbl" "asbv" "asbw" "asel" "*ask" "askin" "asll" "aslv" "/assign" "asub" "asum" "atan" "atran" "atype" "/auto" "autots" "/aux2" "/aux12" "/aux15" "avprin" "avres" "/axlab"

    "/batch" "bellow" "bend" "betad" "bf" "bfa" "bfadele" "bfalist" "bfcum" "bfdele" "bfe" "bfecum" "bfedele" "bfelist" "bfescal" "bfint" "bfk" "bfkdele" "bfklist" "bfl" "bfldele" "bflist" "bfllist" "bfscale" "bftran" "bfunif" "bfv" "bfvdele" "bfvlist" "bioopt" "biot" "blc4" "blc5" "block" "bool" "boptn" "branch" "bsplin" "btol" "bucopt"
    
    "c***" "calc" "cbdof" "cdread" "cdwrite" "ce" "cecmod" "cecyc" "cedele" "ceintf" "celist" "center" "ceqn" "cerig" "cesgen" "cfact" "*cfclos" "*cfopen" "*cfwrite" "cgloc" "cgomga" "check" "chkmsh" "circle" "/clabel" "/clear" "clocal" "/clog" "clog" "clrmshln" "cm" "/cmap" "cmdele" "cmedit" "cmgrp" "cmlist" "cmplot" "cmsel" "cnvtol" "/color" "/com" "con4" "cone" "/config" "conjug" "/contour" "/copy" "couple" "coval" "cp" "cpdele" "cpintf" "/cplane" "cplgen" "cplist" "cpngen" "cpsgen" "cqc" "*create" "crplim" "cs" "cscir" "csdele" "cskp" "cslist" "cswpla" "csys" "/ctype" "curr2d" "cutcontrol" "/cval" "cvar" "cycgen" "*cycle" "cycsol" "cyl4" "cyl5" "cylind"
    
    "d" "da" "dadele" "dalist" "data" "datadef" "dcgomg" "dcum" "ddele" "deact" "define" "*del" "/delete" "deltim" "deriv" "desize" "desol" "detab" "/devdisp" "/device" "dig" "digit" "*dim" "display" "/dist" "dk" "dkdele" "dklist" "dl" "dldele" "dlist" "dllist" "dmove" "dmprat" "dnsol" "*do" "dof" "dofsel" "domega" "/dscale" "dscale" "dset" "dsum" "dsurf" "dsym" "dsys" "dtran" "dump" "/dv3d" "dynopt"
    
    "e" "ealive" "edbound" "edbvis" "edcdele" "edcgen" "edclist" "edcontact" "edcpu" "edcrb" "edcsc" "edcts" "edcurve" "eddamp" "eddrelax" "edele" "edenergy" "edfplot" "/edge" "edhgls" "edhist" "edhtime" "edint" "edivelo" "edlcs" "edldplot" "edload" "edmp" "edndtsd" "ednrot" "edopt" "edout" "edread" "edrst" "edshell" "edsolv" "edstart" "edweld" "edwrite" "/efacet" "egen" "eintf" "ekill" "elem" "elist" "*else" "*elseif" "emagerr" "emf" "emid" "emis" "emodif" "emore" "emsym" "emunit" "en" "*end" "*enddo" "*endif" "engen" "enorm" "ensym" "/eof" "eplot" "eqslv" "/erase" "erase" "eread" "erefine" "eresx" "ernorm" "errang" "esel" "/eshape" "esize" "esla" "esll" "esln" "eslv" "esol" "esort" "estif" "esurf" "esym" "esys" "et" "etable" "etchg" "etdele" "etlist" "etype" "eusort" "*eval" "ewrite" "*exit" "/exit" "exp" "/expand" "expand" "expass" "expsol" "extopt" "extrem"
    
    "f" "/facet" "fatigue" "fcum" "/fdele" "fdele" "fe" "febody" "fecons" "fefor" "felist" "fesurf" "file" "fileaux2" "filedisp" "fill" "filldata" "/filname" "finish" "fitem" "fk" "fkdele" "fklist" "fl" "flange" "fldata" "fldata1" "fldata2" "fldata3" "fldata4" "fldata4a" "fldata5" "fldata6" "fldata7" "fldata8" "fldata9" "fldata10" "fldata11" "fldata12" "fldata13" "fldata14" "fldata15" "fldata16" "fldata17" "fldata18" "fldata19" "fldata20" "fldata20a" "fldata21" "fldata22" "fldata23" "fldata24" "fldata24a" "fldata24b" "fldata24c" "fldata24d" "fldata25" "fldata26" "fldata27" "fldata28" "fldata29" "fldata30" "fldata31" "fldata32" "fldata33" "flist" "fllist" "flocheck" "flotran" "flread" "flst" "fluxv" "fmagbc" "fmagsum" "/focus" "for2d" "force" "form" "/format" "fp" "fplist" "freq" "fs" "fscale" "fsdele" "fslist" "fsnode" "fsplot" "fssect" "fsum" "ftcalc" "ftran" "ftsize" "ftwrite" "/ftype" "fvmesh"
    
    "gap" "gapf" "gapfinish" "gaplist" "gapmerge" "gapopt" "gapplot" "gauge" "gcgen" "/gcmd" "/gcolumn" "genopt" "geom" "geometry" "*get" "/gfile" "/gformat" "/gline" "/gmarker" "*go" "/go" "/golist" "/gopr" "gp" "gpdele" "gplist" "gplot" "/graphics" "/gresume" "/grid" "/gropt" "grp" "/grtyp" "/gsave" "/gst" "gsum" "/gthk" "/gtype"
    
    "harfrq" "/header" "help" "helpdisp" "hfsweep" "hmagsolv" "hpgl" "hptcreate" "hptdelete" "hrexp" "hropt" "hrout"
    
    "ic" "icdele" "iclist" "*if" "igesin" "igesout" "imagin" "immed" "impd" "/input" "inres" "inrtia" "int1" "intsrf" "ioptn" "irlf" "irlist"
    
    "k" "katt" "kbc" "kbetw" "kcalc" "kcenter" "kclear" "kdele" "kdist" "kesize" "keyopt" "keypts" "keyw" "kfill" "kgen" "kl" "klist" "kmesh" "kmodif" "kmove" "knode" "kplot" "kpscale" "krefine" "kscale" "kscon" "ksel" "ksll" "ksln" "ksum" "ksymm" "ktran" "kuse" "kwpave" "kwplan"
    
    "l" "l2ang" "l2tan" "lang" "/larc" "larc" "larea" "large" "latt" "layer" "layerp26" "laylist" "layplot" "lcabs" "lcase" "lccalc" "lccat" "lcdef" "lcfact" "lcfile" "lclear" "lcomb" "lcoper" "lcsel" "lcsl" "lcwrite" "lczero" "ldele" "ldiv" "ldrag" "ldread" "lesize" "lextnd" "lfillt" "lfsurf" "lgen" "lglue" "lgwrite" "/light" "lina" "/line" "line" "lines" "linl" "linp" "linv" "*list" "llist" "lmatrix" "lmesh" "lncollapse" "lndetach" "lnfill" "lnmerge" "lnsplit" "lnsrch" "local" "lovlap" "lplot" "lptn" "lrefine" "lreverse" "lrotat" "lsba" "lsbl" "lsbv" "lsbw" "lsclear" "lsdele" "lsel" "lsla" "lslk" "lsoper" "/lspec" "lsread" "lsscale" "lssolve" "lstr" "lsum" "lswrite" "/lsymbol" "lsymm" "ltan" "ltran" "lumpm" "lvscale" "lwplan"
    
    "m" "magopt" "magsolv" "master" "mat" "mater" "mdamp" "mdele" "/menu" "meshing" "*mfouri" "*mfun" "mgen" "miter" "mlist" "mmf" "mode" "modmsh" "modopt" "monitor" "*mooney" "*moper" "mopt" "move" "mp" "mpamod" "mpchg" "mpdata" "mpdele" "mpdres" "/mplib" "mplist" "mpmod" "mpundo" "mpplot" "mpread" "mprint" "mptemp" "mptgen" "mptres" "mpwrite" "/mrep" "msadv" "mscap" "msdata" "*msg" "mshape" "mshkey" "mshmid" "mshpattern" "msmeth" "msnomf" "msprop" "msquad" "msrelax" "mssolu" "msspec" "/mstart" "msterm" "msvary" "mxpand"
    
    "n" "nang" "ncnv" "ndele" "ndist" "neqit" "/nerr" "nforce" "ngen" "nkpt" "nlgeom" "nlist" "nlog" "nlopt" "nmodif" "nocolor" "nodes" "/noerase" "/nolist" "noorder" "/nopr" "/normal" "nplot" "nprint" "nread" "nrefine" "nrlsum" "nropt" "nrotat" "nrrang" "nscale" "nsel" "nsla" "nsle" "nslk" "nsll" "nslv" "nsol" "nsort" "nstore" "nsubst" "nsvr" "nsym" "/number" "numcmp" "numexp" "nummrg" "numoff" "numstr" "numvar" "nusort" "nwpave" "nwplan" "nwrite"
    
    "omega" "opadd" "opanl" "opclr" "opdata" "opdel" "opeqn" "operate" "opexe" "opfact" "opfrst" "opgrad" "opkeep" "oplfa" "oplgr" "oplist" "oploop" "oplsw" "opmake" "opncontrol" "opprnt" "oprand" "opresu" "oprfa" "oprgr" "oprsw" "opsave" "opsel" "opsubp" "opsweep" "/opt" "optype" "opuser" "opvar" "outopt" "outpr" "/output" "outres"
    
    "padele" "/page" "paget" "paput" "paresu" "parres" "parsav" "pasave" "path" "/pbc" "/pbf" "pcalc" "pcirc" "/pcircle" "pconv" "/pcopy" "pcorro" "pcross" "pdef" "pdot" "pdrag" "perbc2d" "pexclude" "pfact" "pfluid" "pgap" "physics" "pinclude" "pinsul" "pipe" "planewave" "plconv" "plcplx" "plcrack" "pldisp" "plesol" "pletab" "plf2d" "plls" "plnsol" "/plopts" "plot" "plotting" "plpagm" "plpath" "plsect" "pltime" "pltrac" "plvar" "plvaropt" "plvect" "/pmacro" "pmap" "/pmeth" "pmeth" "pmgtran" "pmopts" "/pmore" "/pnum" "point" "poly" "/polygon" "popt" "portopt" "/post1" "/post26" "powerh" "ppath" "pplot" "pprange" "ppres" "prange" "prconv" "prcplx" "precision" "pred" "/prep7" "prerr" "presol" "pretab" "pri2" "prim" "print" "prism" "priter" "prnld" "prnsol" "prod" "prpath" "prrfor" "prrsol" "prsect" "prssol" "prtime" "prvar" "prvaropt" "prvect" "pscr" "psdcom" "psdfrq" "psdres" "psdspl" "psdunit" "psdval" "psdwav" "/psearch" "psel" "/psf" "psolve" "/pspec" "pspec" "psprng" "/pstatus" "pstres" "/psymb" "ptemp" "ptxy" "punit" "pvect" "/pwedge"
    
    "qdval" "qfact" "quad" "/quit" "quot" "r" "race" "rall" "rappnd" "/ratio" "rbe3" "rcon" "rdele" "real" "realvar" "rectng" "reduce" "reflcoef" "/rename" "reorder" "*repeat" "/replot" "/reset" "reset" "resp" "resume" "rexport" "rfilsz" "rforce" "/rgb" "rigid" "rimport" "riter" "rlist" "rmemry" "rmodif" "rmore" "rock" "rpoly" "rpr4" "rprism" "rpsd" "rspeed" "rstat" "rsys" "rtimst" "run" "/runst" "rwfrnt"
    
    "sabs" "sadd" "sallow" "sarplot" "save" "sbclist" "sbctran" "sdelete" "se" "secdata" "/seclib" "secnum" "secoffset" "secplot" "secread" "sectype" "secwrite" "sed" "sedlist" "seexp" "/seg" "selist" "selm" "senergy" "seopt" "sesymm" "*set" "set" "setran" "sexp" "sf" "sfa" "sfact" "sfadele" "sfalist" "sfbeam" "sfcalc" "sfcum" "sfdele" "sfe" "sfedele" "sfelist" "sffun" "sfgrad" "sfl" "sfldele" "sflist" "sfllist" "sfscale" "sftran" "/shade" "shell" "/show" "/showdisp" "shpp" "/shrink" "slist" "slpplot" "slsplot" "small" "smax" "smbody" "smcons" "smfor" "smin" "smrtsize" "smsurf" "smult" "solcontrol" "/solu" "solu" "soluopt" "solve" "sort" "source" "space" "sparm" "spec" "sph4" "sph5" "sphere" "spline" "spoint" "spopt" "spread" "sptopt" "sqrt" "srcs" "srss" "/sscale" "ssln" "sstif" "ssum" "stat" "*status" "/status" "stef" "/stitle" "store" "subopt" "subset" "sumtype" "sv" "svtyp" "/syp" "/sys"
    
    "tallow" "tb" "tbcopy" "tbdata" "tbdele" "tble" "tblist" "tbmodif" "tbplot" "tbpt" "tbtemp" "tchg" "tee" "term" "time" "timerange" "timint" "timp" "tintp" "/title" "/tlabel" "toffst" "topdef" "topexe" "topiter" "torq2d" "torqc2d" "torqsum" "torus" "total" "trans" "transfer" "*tread" "tref" "/triad" "/trlcy" "trnopt" "trpdel" "trplis" "trpoin" "trtime" "tshap" "/tspec" "tsres" "tunif" "tvar" "/type" "type"
    
    "/ucmd" "/ui" "uimp" "/uis" "*ulib" "/units" "upcoord" "upgeom" "*use" "/user" "usrcal"
    
    "v" "va" "*vabs" "vadd" "valve" "vardel" "varnam" "vatt" "vclear" "*vcol" "/vcone" "vcross" "*vcum" "vcvfill" "vddam" "vdele" "vdgl" "vdot" "vdrag" "*vedit" "vext" "*vfact" "*vfill" "*vfun" "vgen" "*vget" "vget" "vglue" "/view" "vimp" "vinp" "vinv" "*vitrp" "*vlen" "vlist" "vlscale" "*vmask" "vmesh" "voffst" "volumes" "*voper" "vovlap" "*vplot" "vplot" "vptn" "*vput" "vput" "*vread" "vrotat" "vsba" "vsbv" "vsbw" "/vscale" "*vscfun" "vsel" "vsla" "*vstat" "vsum" "vsweep" "vsymm" "vtran" "vtype" "/vup" "*vwrite"
    
    "/wait" "waves" "werase" "wfront" "/window" "wmore" "wpave" "wpcsys" "wplane" "wpoffs" "wprota" "wpstyl" "write" "wsort" "wstart" "/xrange" "xvar" "xvaropt"
    
    "/yrange"
    
    "/zoom"
    )
  "Downcase of Commands in Ansys (these names are also reserved).")

(defvar ansys-reserved-words
  (append ansys-begin-keywords
	  ansys-else-keywords
	  ansys-end-keywords
	  ansys-command-keywords
	  ansys-downcase-comand-keywords
	'("AMESH4"
	    "amesh4"
	    ))
  "Reserved words in Ansys.")



(defvar ansys-command-options
  '("EX" "DENS" "ALPX" "REFT" "NUXY" "PRXY" "GXY" "MU" "DAMP" "VISC" "SONC"
    "ex" "dens" "alpx" "reft" "nuxy" "prxy" "gxy" "mu" "damp" "visc" "sonc"
    "VOLU" "AREA" "LINE" "KP" "ELEM" "NODE"
    "volu" "area" "line" "kp" "elem" "node"
    "STATIC" "MODAL" "TRANS"
    "static" "modal" "trans"
    "FRONT" "PCG" "JCG" "SPARSE"
    "front" "pcg" "jcg" "sparse"
    "SUBSP" "FULL"
    "subsp" "full"
    "UX" "UY" "UZ" "ROTX" "ROTY" "ROTZ" "PRES"
    "ux" "uy" "uz" "rotx" "roty" "rotz" "pres"
    "ALL" "YES" "NO" "ON" "OFF" "AUTO"
    "all" "yes" "no" "on" "off" "auto"
    "THEN" "*EXIT" "CYCLE" "STOP"
    "then" "*exit" "cycle" "stop"
    )
  "Builtin Command options in Ansys.")

(defvar ansys-font-lock-keywords
  (list
   ;; Fontify all builtin keywords.
   (cons (concat "\\<\\("
		 (mapconcat 'identity ansys-reserved-words "\\|")
		 "\\)\\>")
	 'font-lock-function-name-face)
   ;; Fontify all builtin operators.
   (cons "\\(&\\||\\|<=\\|>=\\|==\\|<\\|>\\|!=\\|!\\)"
	 'font-lock-reference-face)
   ;; Fontify all builtin variables.
   (cons (concat "\\<\\("
		 (mapconcat 'identity ansys-command-options "\\|")
		 "\\)\\>")
	 'font-lock-variable-name-face))
  "Additional Ansys expressions to highlight.")

(defvar ansys-mode-map nil
  "Keymap used in Ansys mode.")
(if ansys-mode-map
    ()
  (let ((map (make-sparse-keymap)))
    (define-key map "`" 'ansys-abbrev-start)
    (define-key map " " 'ansys-electric-space)
    (define-key map "\n" 'ansys-reindent-then-newline-and-indent)
    (define-key map "\t" 'indent-according-to-mode)
    (define-key map "\e\n" 'ansys-indent-new-comment-line)  
    (define-key map "\e\t" 'ansys-complete-symbol)
    (define-key map "\C-c;" 'ansys-comment-region)
    (define-key map "\C-c:" 'ansys-uncomment-region)  
    (define-key map "\C-c\C-b" 'ansys-submit-bug-report)
    (define-key map "\C-c\C-p" 'ansys-previous-code-line)
    (define-key map "\C-c\C-n" 'ansys-next-code-line)
    (define-key map "\C-c\C-a" 'ansys-beginning-of-line)
    (define-key map "\C-c\C-e" 'ansys-end-of-line)  
    (define-key map "\C-c\M-\C-n" 'ansys-forward-block)
    (define-key map "\C-c\M-\C-p" 'ansys-backward-block)
    (define-key map "\C-c\M-\C-u" 'ansys-backward-up-block)
    (define-key map "\C-c\M-\C-d" 'ansys-down-block)
    (define-key map "\C-c]" 'ansys-close-block)
    (define-key map "\C-c\C-r" 'ansys-column-ruler)
    (define-key map "\C-c\C-i" 'ansys-if)
    (define-key map "\C-c\C-t" 'ansys-if-then)
    (define-key map "\C-c\C-d" 'ansys-do)
    (define-key map "\C-c\C-m" 'ansys-mp)
    (setq ansys-mode-map map)))

(defvar ansys-mode-menu
  (list "Ansys"
	["Comment   Region"             ansys-comment-region t]
	["Uncomment Region"             ansys-uncomment-region t]
	(list "Insert Function"
	      [" *IF "	                ansys-if t]
	      [" *IF THEN *ENDIF "	ansys-if-then t]
	      [" *DO *ENDDO"	        ansys-do t]
	      [" MP "	                ansys-mp t])
	(list "Lines"
	      ["Previous Code Line"	ansys-previous-code-line t]
	      ["Next Code Line"		ansys-next-code-line t]
	      ["Begin of Continuation"	ansys-beginning-of-line t]
	      ["End of Continuation"	ansys-end-of-line t]
	      ["Split Line at Point"	ansys-indent-new-comment-line t])
	(list "Blocks"
	      ["Next Block"		ansys-forward-block t]
	      ["Previous Block"		ansys-backward-block t]
	      ["Down Block"		ansys-down-block t]
	      ["Up Block"		ansys-backward-up-block t]
	      ["Close Block"		ansys-close-block t])
	"-"
        ["Toggle Abbrev Mode"           abbrev-mode t]
	["Describe Ansys Mode"		ansys-describe-major-mode t])
  "Menu for Ansys mode.")

(defvar ansys-mode-syntax-table nil
  "Syntax table in use in ansys-mode buffers.")
(if ansys-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\r " "  table)
    (modify-syntax-entry ?+ "."   table)
    (modify-syntax-entry ?- "."   table)
    (modify-syntax-entry ?= "."   table)
    (modify-syntax-entry ?> "."   table)
    (modify-syntax-entry ?< "."   table)
    (modify-syntax-entry ?. "."   table)
    (modify-syntax-entry ?\% "."  table)
    (modify-syntax-entry ?| "."   table)
    (modify-syntax-entry ?\' "."  table)
    (modify-syntax-entry ?\` "w"  table)
    (modify-syntax-entry ?_ "w"   table)
    (modify-syntax-entry ?: "w"  table)

    (modify-syntax-entry ?* "w"   table)
    (modify-syntax-entry ?/ "w"   table)
    (modify-syntax-entry ?\! "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    (setq ansys-mode-syntax-table table)))


(defvar ansys-auto-indent t
  "*Non-nil means indent line after a space in Ansys mode.")

(defvar ansys-blink-matching-block t
  "*Control the blinking of matching Ansys block keywords.
Non-nil means show matching begin of block when inserting a space,
newline or semicolon after an else or end keyword.")

(defvar ansys-block-offset 3
  "*Extra indentation applied to statements in Ansys block structures.")

(defvar ansys-block-begin-regexp
  (concat "\\<\\("
	  (mapconcat 'identity ansys-begin-keywords "\\|")
	  "\\)\\>"))
(defvar ansys-block-else-regexp
  (concat "\\<\\("
	  (mapconcat 'identity ansys-else-keywords "\\|")
	  "\\)\\>"))
(defvar ansys-block-end-regexp
  (concat "\\<\\("
	  (mapconcat 'identity ansys-end-keywords "\\|")
	  "\\)\\>"))
(defvar ansys-block-begin-or-end-regexp
  (concat ansys-block-begin-regexp "\\|" ansys-block-end-regexp))

(defvar ansys-block-else-or-end-regexp
  (concat ansys-block-else-regexp "\\|" ansys-block-end-regexp))

(defvar ansys-block-match-alist
  '(("*IF" . ("THEN" "*ELSE" "*ELSEIF" "*ENDIF"))
    ("*DO" . ("*ENDDO" "*enddo"))
    ("*do" . ("*ENDDO" "*enddo"))
    ("*CREATE" . ("*END" "*end"))
    ("*create" . ("*END" "*end")))
  "Alist with Ansys's matching block keywords.
Has Ansys's begin keywords as keys and a list of the matching else or
end keywords as associated values.")

(defvar ansys-continuation-offset 0
  "*Extra indentation applied to Ansys continuation lines.")

(defvar ansys-continuation-regexp
  "[^#%\n]*\\(\\\\\\|\\.\\.\\.\\)\\s-*\\(\\s<.*\\)?$")

(defvar ansys-continuation-string "\&"
  "*Character string used for Ansys continuation lines.  Normally \\.")

(defvar ansys-completion-alist nil
  "Alist of Ansys symbols for completion in Ansys mode.
Each element looks like (VAR . VAR), where the car and cdr are the same
symbol (an Ansys command or variable name).
Currently, only builtin variables can be completed.") 

(defvar ansys-mode-startup-message t
  "*Nil means do not display the Ansys mode startup message.")

(defvar ansys-mode-hook nil
  "*Hook to be run when Ansys mode is started.")

(defvar ansys-column-ruler-wide
  "0        10        20        30        40        50\
\        60        70        80\n\
\|    |    |    |    |    |    |    |    |    |    \
\|    |    |    |    |    |    |\n"
  "*String displayed above current line by \\[ansys-column-ruler].")

(defvar ansys-column-ruler-narrow
  "0        10        20        30        40        50\
\        60        70\n\
\|    |    |    |    |    |    |    |    |    |    \
\|    |    |    |    |\n"
  "*String displayed above current line by \\[ansys-column-ruler].")

(defvar ansys-format nil
  "*String representing the ansys format.")

;;;###autoload
(defun ansys-mode ()
  "Major mode for editing Ansys log files.

This mode makes it easier to write Ansys log files by helping with
indentation, doing some of the typing for you (with Abbrev mode) and by
showing keywords, comments, strings, etc. in different faces (with
Font Lock mode on terminals that support it).

Type \\[list-abbrevs] to display the built-in abbrevs for Ansys keywords.

Keybindings
===========

\\{ansys-mode-map}

Variables you can use to customize Ansys mode
==============================================

ansys-auto-indent
  Non-nil means indent current line after a  space.
  Default is nil.

ansys-blink-matching-block
  Non-nil means show matching begin of block when inserting a space,
  newline or semicolon after an else or end keyword.  Default is t.

ansys-mode-startup-message
  Nil means do not display the Ansys mode startup message.
  Default is t.

Turning on Ansys mode runs the hook `ansys-mode-hook'.

To begin using this mode for all `.log' files that you edit, add the
following lines to your `.emacs' file:

  (autoload 'ansys-mode \"ansys-mod\" nil t)

  (setq auto-mode-alist
         (cons '(\"\\.log\\'\" . ansys-mode) auto-mode-alist))

To automatically turn on the abbrev, auto-fill and font-lock features,
add the following lines to your `.emacs' file as well:

  (add-hook 'ansys-mode-hook
	    (lambda ()
	      (abbrev-mode 1)
	      (auto-fill-mode 1)
	      (if (eq window-system 'x)
		  (font-lock-mode 1))))

To submit a problem report, enter \\[ansys-submit-bug-report] from
an Ansys mode buffer.
This automatically sets up a mail buffer with version information
already added.  You just need to add a description of the problem,
including a reproducible test case and send the message."
  (interactive)
  (kill-all-local-variables)

  (use-local-map ansys-mode-map)
  (setq major-mode 'ansys-mode)
  (setq mode-name "Ansys")
  (setq local-abbrev-table ansys-abbrev-table)
  (set-syntax-table ansys-mode-syntax-table)
  
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ansys-indent-line)

  (make-local-variable 'comment-start)  
  (setq comment-start ansys-indent-comment)
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 30)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\s<+\\s-*")

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ansys-font-lock-keywords nil nil))

  (make-local-variable 'wide-ansys-ruler-mode)
  (setq wide-ansys-ruler-mode nil)
	"set to  nil for narrow, t for wide."

  (make-local-variable 'ansys-column-ruler-wide)
  (make-local-variable 'ansys-column-ruler-narrow)

  (make-local-variable 'ansys-format)
  (setq ansys-format (intern "log"))

  (ansys-add-ansys-menu)
  (ansys-initialize-completions)
  (run-hooks 'ansys-mode-hook))

;;; Miscellaneous useful functions
(defun ansys-describe-major-mode ()
  "Describe the current major mode."
  (interactive)
  (describe-function major-mode))

(defun ansys-point (position)
  "Returns the value of point at certain positions." 
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     (t (error "unknown buffer position requested: %s" position)))
    (point)))

(defsubst ansys-in-comment-p ()
  "Returns t if point is inside an Ansys comment, nil otherwise."
  (interactive)
  (save-excursion
    (nth 4 (parse-partial-sexp (ansys-point 'bol) (point)))))

(defsubst ansys-in-string-p ()
  "Returns t if point is inside an Ansys string, nil otherwise."
  (interactive)
  (save-excursion
    (nth 3 (parse-partial-sexp (ansys-point 'bol) (point)))))

(defsubst ansys-not-in-string-or-comment-p ()
  "Returns t if point is not inside an Ansys string or comment."
  (let ((pps (parse-partial-sexp (ansys-point 'bol) (point))))
    (not (or (nth 3 pps) (nth 4 pps)))))

(defun ansys-in-block-p ()
  "Returns t if point is inside an Ansys block, nil otherwise.
The block is taken to start at the first letter of the begin keyword and
to end after the end keyword."
  (let ((pos (point)))
    (save-excursion
      (condition-case nil
	  (progn
	    (skip-syntax-forward "w")
	    (ansys-up-block -1)
	    (ansys-forward-block)
	    t)
	(error nil))
      (< pos (point)))))

(defun ansys-maybe-insert-continuation-string ()
  (if (or (ansys-in-comment-p)
	  (save-excursion
	    (beginning-of-line)
	    (looking-at ansys-continuation-regexp)))
      nil
    (delete-horizontal-space)
    (insert (concat " " ansys-continuation-string))))

(defun ansys-column-ruler ()
  "Inserts a column ruler momentarily above current line, till next keystroke.
The key typed is executed unless it is SPC."
  (interactive)
  (momentary-string-display 
   (if wide-ansys-ruler-mode 
       ansys-column-ruler-wide
     ansys-column-ruler-narrow)
   (save-excursion
     (beginning-of-line) 
     (if (eq (window-start (selected-window))
             (window-point (selected-window)))
         (progn (forward-line) (point))
       (point)))
   nil "Type SPC or any command to erase ruler."))

;;; Comments
(defun ansys-comment-region (beg end &optional arg)
  "Comment or uncomment each line in the region as Ansys code.
See `comment-region'."
  (interactive "r\nP")
  (let ((comment-start (char-to-string ansys-comment-char)))
    (comment-region beg end arg)))
  
(defun ansys-uncomment-region (beg end &optional arg)
  "Uncomment each line in the region as Ansys code."
  (interactive "r\nP")
  (or arg (setq arg 1))
  (ansys-comment-region beg end (- arg)))

;;; Indentation
(defun calculate-ansys-indent ()
  "Return appropriate indentation for current line as Ansys code.
Returns an integer (the column to indent to) unless the line is a
comment line with fixed goal golumn.  In that case, returns a list whose
car is the column to indent to, and whose cdr is the current indentation
level."
  (let ((is-continuation-line
	 (save-excursion
	   (if (zerop (ansys-previous-code-line))
	       (looking-at ansys-continuation-regexp))))
	(icol 0))
    (save-excursion
      (beginning-of-line)
      ;; If we can move backward out one level of parentheses, take 1
      ;; plus the indentation of that parenthesis.  Otherwise, go back
      ;; to the beginning of the previous code line, and compute the
      ;; offset this line gives.
      (if (condition-case nil
	      (progn
		(up-list -1)
		t)
	    (error nil))
	  (setq icol (+ 1 (current-column)))
	(if (zerop (ansys-previous-code-line))
	    (progn
	      (ansys-beginning-of-line)
	      (back-to-indentation)
	      (setq icol (current-column))
	      (let ((bot (point))
		    (eol (ansys-point 'eol)))
		(while (< (point) eol)
		  (if (ansys-not-in-string-or-comment-p)
		      (cond
		       ((looking-at "\\<switch\\>")
			(setq icol (+ icol (* 2 ansys-block-offset))))
		       ((looking-at ansys-block-begin-regexp)
			(setq icol (+ icol ansys-block-offset)))
		       ((looking-at ansys-block-else-regexp)
			(if (= bot (point))
			    (setq icol (+ icol ansys-block-offset))))
		       ((looking-at ansys-block-end-regexp)
			(if (not (= bot (point)))
			    (setq icol (- icol
					  (ansys-block-end-offset)))))))
		  (forward-char)))
	      (if is-continuation-line
		  (setq icol (+ icol ansys-continuation-offset)))))))
    (save-excursion
      (back-to-indentation)
      (cond
       ((and (looking-at ansys-block-else-regexp)
	     (ansys-not-in-string-or-comment-p))
	(setq icol (- icol ansys-block-offset)))
       ((and (looking-at ansys-block-end-regexp)
	     (ansys-not-in-string-or-comment-p))
	(setq icol (- icol (ansys-block-end-offset))))
       ((and (looking-at "\\s<\\s-\\S<")
	     (not (looking-at "\\s<\\s-\\s-\\S<"))
	(setq icol (list comment-column icol))))
       ((or (looking-at "\\s<\\S<")
	(setq icol (+ 0 icol))))))
    icol))

(defun ansys-block-end-offset ()
  (save-excursion
    (ansys-backward-up-block 1)
    (* ansys-block-offset
       (if (string-match (match-string 0) "switch") 2 1))))

(defun ansys-indent-line (&optional arg)
  "Indent current line as Ansys code.
With optional ARG, use this as offset unless this line is a comment with
fixed goal column."
  (interactive)
  (or arg (setq arg 0))
  (let ((icol (calculate-ansys-indent))
	(relpos (- (current-column) (current-indentation))))
    (if (listp icol)
	(setq icol (car icol))
      (setq icol (+ icol arg)))
    (if (< icol 0)
	(error "Unmatched end keyword")
      (indent-line-to icol)
      (if (> relpos 0)
	  (move-to-column (+ icol relpos))))))

(defun ansys-indent-new-comment-line ()
  "Break Ansys line at point, continuing comment if within one.
If within code, insert `ansys-continuation-string' before breaking the
line.  If within a string, signal an error.   
The new line is properly indented." 
  (interactive)
  (delete-horizontal-space)
  (cond
   ((ansys-in-comment-p)
    (indent-new-comment-line))
   ((ansys-in-string-p)
    (error "Cannot split a code line inside a string"))
   (t
    (insert (concat " " ansys-continuation-string))
    (ansys-reindent-then-newline-and-indent))))

;;; Motion
(defun ansys-next-code-line (&optional arg)
  "Move ARG lines of Ansys code forward (backward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc))
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))
      
(defun ansys-previous-code-line (&optional arg)
  "Move ARG lines of Ansys code backward (forward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (ansys-next-code-line (- arg)))

(defun ansys-beginning-of-line ()
  "Move point to beginning of current Ansys line.
If on an empty or comment line, go to the beginning of that line.
Otherwise, move backward to the beginning of the first Ansys code line
which is not inside a continuation statement, i.e., which does not
follow a code line ending in `...' or `\\', or is inside an open
parenthesis list."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\s-*\\($\\|\\s<\\)"))
      (while (or (condition-case nil
		     (progn
		       (up-list -1)
		       (beginning-of-line)
		       t)
		   (error nil))
		 (and (or (looking-at "\\s-*\\($\\|\\s<\\)")
			  (save-excursion
			    (if (zerop (ansys-previous-code-line))
				(looking-at ansys-continuation-regexp))))
		      (zerop (forward-line -1)))))))

(defun ansys-scan-blocks (from count depth)
  "Scan from character number FROM by COUNT Ansys begin-end blocks.
Returns the character number of the position thus found.

If DEPTH is nonzero, block depth begins counting from that value.
Only places where the depth in blocks becomes zero are candidates for
stopping; COUNT such places are counted.

If the beginning or end of the buffer is reached and the depth is wrong,
an error is signaled."
  (let ((min-depth (if (> depth 0) 0 depth))
	(inc (if (> count 0) 1 -1)))
    (save-excursion
      (while (/= count 0)
	(catch 'foo
	  (while (or (re-search-forward
		      ansys-block-begin-or-end-regexp nil 'move inc)
		     (if (/= depth 0)
			 (error "Unbalanced block")))
	    (if (ansys-not-in-string-or-comment-p)
		(progn
		  (cond
		   ((match-end 1)
		    (setq depth (+ depth inc)))
		   ((match-end 2)
		    (setq depth (- depth inc))))
		  (if (< depth min-depth)
		      (error "Containing expression ends prematurely"))
		  (if (= depth 0)
		      (throw 'foo nil))))))
	(setq count (- count inc)))
      (point))))

(defun ansys-forward-block (&optional arg)
  "Move forward across one balanced Ansys begin-end block.
With argument, do it that many times.
Negative arg -N means move backward across N blocks."
  (interactive "p")
  (or arg (setq arg 1))
  (goto-char (or (ansys-scan-blocks (point) arg 0) (buffer-end arg))))

(defun ansys-backward-block (&optional arg)
  "Move backward across one balanced Ansys begin-end block.
With argument, do it that many times.
Negative arg -N means move forward across N blocks."
  (interactive "p")
  (or arg (setq arg 1))
  (ansys-forward-block (- arg)))

(defun ansys-down-block (arg)
  "Move forward down one begin-end block level of Ansys code.
With argument, do this that many times.
A negative argument means move backward but still go down a level.
In Lisp programs, an argument is required."
  (interactive "p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (ansys-scan-blocks (point) inc -1)
		     (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun ansys-backward-up-block (arg)
  "Move backward out of one begin-end block level of Ansys code.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot.
In Lisp programs, an argument is required."
  (interactive "p")
  (ansys-up-block (- arg)))

(defun ansys-up-block (arg)
  "Move forward out of one begin-end block level of Ansys code.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot.
In Lisp programs, an argument is required."
  (interactive "p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (ansys-scan-blocks (point) inc 1)
		     (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun ansys-close-block ()
  "Close the current Ansys block on a separate line.
An error is signaled if no block to close is found."
  (interactive)
  (let (bb-keyword)
    (condition-case nil
	(progn
	  (save-excursion
	    (ansys-backward-up-block 1)
	    (setq bb-keyword (buffer-substring-no-properties
			      (match-beginning 1) (match-end 1))))
	  (if (save-excursion
		(beginning-of-line)
		(looking-at "^\\s-*$"))
	      (indent-according-to-mode)
	    (ansys-reindent-then-newline-and-indent))
	  (insert (car (reverse
			(assoc bb-keyword
			       ansys-block-match-alist))))
	  (ansys-reindent-then-newline-and-indent)
	  t)
      (error (message "No block to close found")))))

(defun ansys-blink-matching-block-open ()
  "Blink the matching Ansys begin block keyword.
If point is right after an Ansys else or end type block keyword, move
cursor momentarily to the corresponding begin keyword.
Signal an error if the keywords are incompatible."
  (interactive)
  (let (bb-keyword bb-arg eb-keyword pos eol)
    (if (and (ansys-not-in-string-or-comment-p)
	     (looking-at "\\>")
	     (save-excursion
	       (skip-syntax-backward "w")
	       (looking-at ansys-block-else-or-end-regexp)))
	(save-excursion
	  (cond
	   ((match-end 1)
	    (setq eb-keyword
		  (buffer-substring-no-properties
		   (match-beginning 1) (match-end 1)))
	    (ansys-backward-up-block 1))
	   ((match-end 2)
	    (setq eb-keyword
		  (buffer-substring-no-properties
		   (match-beginning 2) (match-end 2)))
	    (ansys-backward-block)))
	  (setq pos (match-end 0)
		bb-keyword
		(buffer-substring-no-properties
		 (match-beginning 0) pos)
		pos (+ pos 1)
		eol (ansys-point 'eol)
		bb-arg
		(save-excursion
		  (save-restriction
		    (goto-char pos)
		    (while (and (skip-syntax-forward "^<" eol)
				(ansys-in-string-p)
				(not (forward-char 1))))
		    (skip-syntax-backward " ")
		    (buffer-substring-no-properties pos (point)))))
	  (if (member eb-keyword
		      (cdr (assoc bb-keyword ansys-block-match-alist)))
	      (progn
		(message "Matches `%s %s'" bb-keyword bb-arg)
		(if (pos-visible-in-window-p)
		    (sit-for blink-matching-delay)))
	    (error "Block keywords `%s' and `%s' do not match"
		   bb-keyword eb-keyword))))))

(defun ansys-end-of-line ()
  "Move point to end of current Ansys line.
If on an empty or comment line, go to the end of that line.
Otherwise, move forward to the end of the first Ansys code line which
does not end in `...' or `\\' or is inside an open parenthesis list."
  (interactive)
  (end-of-line)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "\\s-*\\($\\|\\s<\\)"))
      ()
    (while (or (condition-case nil
		   (progn
		     (up-list 1)
		     (end-of-line)
		     t)
		 (error nil))
	       (and (save-excursion
		      (beginning-of-line)
		      (or (looking-at "\\s-*\\($\\|\\s<\\)")
			  (looking-at ansys-continuation-regexp)))
		    (zerop (forward-line 1)))))
    (end-of-line)))

;;; Completions
(defun ansys-initialize-completions ()
  "Create an alist for Ansys completions."
  (if ansys-completion-alist
      ()
    (setq ansys-completion-alist
	  (mapcar '(lambda (var) (cons var var))
		  (append ansys-reserved-words
			  ansys-command-options)))))

(defun ansys-complete-symbol ()
  "Perform completion on Ansys symbol preceding point.
Compare that symbol against Ansys's reserved words and builtin
variables."
  ;; This code taken from lisp-complete-symbol
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion (backward-sexp 1) (point)))
	 (string (buffer-substring-no-properties beg end))
	 (completion (try-completion string ansys-completion-alist)))
    (cond ((eq completion t))		; ???
	  ((null completion)
	   (message "Can't find completion for \"%s\"" string)
	   (ding))
	  ((not (string= string completion))
           (delete-region beg end)
           (insert completion))
	  (t
	   (let ((list (all-completions string ansys-completion-alist))
		 (conf (current-window-configuration)))
	     ;; Taken from comint.el
	     (message "Making completion list...")	       
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list))
	     (message "Hit space to flush")
	     (let (key first)
	       (if (save-excursion
		     (set-buffer (get-buffer "*Completions*"))
		     (setq key (read-key-sequence nil)
			   first (aref key 0))
		     (and (consp first) (consp (event-start first))
			  (eq (window-buffer (posn-window (event-start
							   first)))
			      (get-buffer "*Completions*"))
			  (eq (key-binding key) 'mouse-choose-completion)))
		   (progn
		     (mouse-choose-completion first)
		     (set-window-configuration conf))
		 (if (eq first ?\ )
		     (set-window-configuration conf)
		   (setq unread-command-events
			 (listify-key-sequence key))))))))))
	       

;;;; Electric characters && friends
(defun ansys-reindent-then-newline-and-indent ()
  "Reindent current Ansys line, insert newline, and indent the new line.
If Abbrev mode is on, expand abbrevs first."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (if ansys-blink-matching-block
      (ansys-blink-matching-block-open))
  (save-excursion
    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
    (indent-according-to-mode))
  (insert "\n")
  (indent-according-to-mode))

(defun ansys-electric-space ()
  "Insert a space in Ansys mode.
Maybe expand abbrevs and blink matching block open keywords.
Reindent the line of `ansys-auto-indent' is non-nil."
  (interactive)
  (setq last-command-char ? )
  (if (not (ansys-not-in-string-or-comment-p))
      (progn
	(indent-according-to-mode)
	(self-insert-command 1))
    (if abbrev-mode (expand-abbrev))
    (if ansys-blink-matching-block
	(ansys-blink-matching-block-open))
    (if (and ansys-auto-indent
	     (save-excursion
	       (skip-syntax-backward \" \")
	       (not (bolp))))
	(indent-according-to-mode))
    (self-insert-command 1)))

(defun ansys-abbrev-start ()
  "Start entering an Ansys abbreviation.
If Abbrev mode is turned on, typing ` (grave accent) followed by ? or
\\[help-command] lists all Ansys abbrevs.  Any other key combination is
executed normally.
Note that all Ansys mode abbrevs start with a grave accent."
  (interactive)
  (if (not abbrev-mode)
      (self-insert-command 1)
    (let (c)
      (insert last-command-char)
      (if (or (eq (setq c (read-event)) ??)
	      (eq c help-char))
	  (let ((abbrev-table-name-list '(ansys-mode-abbrev-table)))
	    (list-abbrevs))
	(setq unread-command-events (list c))))))

;;; Menu
(defun ansys-add-ansys-menu ()
  "Adds the `Ansys' menu to the menu bar in Ansys mode."
  (require 'easymenu)  
  (easy-menu-define ansys-mode-menu-map ansys-mode-map
		    "Menu keymap for Ansys mode." ansys-mode-menu)
  (easy-menu-add ansys-mode-menu-map ansys-mode-map))

;;; Bug reporting
(defun ansys-submit-bug-report ()
  "Submit a bug report on the Emacs Ansys package via mail."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a bug report? ")
   (reporter-submit-bug-report
    ansys-maintainer-address
    (concat "Emacs version " emacs-version)
    (list
     'ansys-blink-matching-block
     'ansys-block-offset
     'ansys-comment-char
     'ansys-continuation-offset
     'ansys-continuation-string
     'ansys-mode-startup-message))))

(defmacro define-ansys-skeleton (command documentation &rest definitions)
  "Define COMMAND with [DOCSTRING] to insert statements as in DEFINITION ...
Prior definitions (e.g. from ~/.emacs) are maintained.
Each definition is built up as (FORMAT PROMPT ELEMENT ...).  Alternately
a synonym definition can be (FORMAT . PREVIOUSLY-DEFINED-FORMAT).

For the meaning of (PROMPT ELEMENT ...) see `skeleton-insert'.
Each DEFINITION is actually stored as
	(put COMMAND FORMAT (PROMPT ELEMENT ...)),
which you can also do yourself."
  (or (stringp documentation)
      (setq definitions (cons documentation definitions)
	    documentation ""))
  ;; The compiled version doesn't.
  (require 'backquote)
  (`(progn
      (let ((definitions '(, definitions)))
	(while definitions
	  ;; skeleton need not be loaded to define these
	  (or (get '(, command) (car (car definitions)))
	      (put '(, command) (car (car definitions))
		   (if (symbolp (cdr (car definitions)))
		       (get '(, command) (cdr (car definitions)))
		     (cdr (car definitions)))))
	  (setq definitions (cdr definitions))))
      (defun (, command) ()
	(, documentation)
	(interactive)
	(skeleton-insert
	 (or (get '(, command) ansys-format)
	     (error "%s statement syntax not defined for ansys format %s."
		    '(, command) ansys-format)))))))


(define-ansys-skeleton ansys-if
  "Insert an if statement in the current format's syntax."
  (format "Value/Parameter 1: "
      "*IF," str ","
      (read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
       "," 
      (read-string "Value/Parameter 2: ")
       "," 
      (read-string "Action: (:label,STOP,EXIT,CYCLE,THEN) ")
      \n)
  (log . format))

(define-ansys-skeleton ansys-if-then
  "Insert an if statement in the current format's syntax."
  (format "Value/Parameter 1: "
      "*IF," str ","
      (read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
       "," 
      (read-string "Value/Parameter 2: ")
       ",THEN" \n
       > _ \n
       ("*ELSEIF? %s: "
	< "*ELSEIF," str ","
	(read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
	"," 
	(read-string "Next Value/Parameter: ")
	",THEN" \n
	> \n)
	< "*ELSE" \n
	> \n
       < "*ENDIF"
       \n)
  (log . format))

(define-ansys-skeleton ansys-do
  "Insert an if statement in the current format's syntax."
  (format "Parameter: "
      "*DO," str ","
      (read-string "Start Value/Parameter: ")
       "," 
      (read-string "Finish Value/Parameter: ")
       "," 
      (read-string "Increment Value/Parameter: ") \n
      > _ \n
      < "*ENDDO"
      \n)
  (log . format))

(define-ansys-skeleton ansys-mp
  "Insert an if statement in the current format's syntax."
  (format "Material Property: (EX,ALPX,PRXY,NUXY,GXY,DAMP,MU,DENS,KXX) "
      "MP," str ","
      (read-string "Material Number: ")
       "," 
      (read-string "Constant Value: ")
      ","
      (read-string "Linear Coefficient? : ")
      ","
      (read-string "Quadratic Coefficient? : ")
      ","
      (read-string "Cubic Coefficient? : ")
      ","
      (read-string "Quartic Coefficient? : ")
      \n)
  (log . format))


;;; provide ourself

(provide 'ansys-mod)

;;; ansys-mod.el ends here
