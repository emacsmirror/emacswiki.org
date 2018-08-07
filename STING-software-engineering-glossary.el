;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- mode: EMACS-LISP; no-byte-compile: t; -*-
;;; this is STING-software-engineering-glossary.el
;;; ================================================================
;;; DESCRIPTION: 
;;; STING-Software-Engineering-Glossary presents 
;;; Philip Hausler's STING Software Engineering Glossary as an 
;;; alist/hash with a lookup function to evaluate from within Emacs.
;;; Should eventually be incorporated with a current version of FOLDOC.
;;; \(URL `http://www.apl.jhu.edu/Notes/Hausler/web/glossary.html')
;;; Hausler indicates the glossary is Copyright:
;;; CERN, European Laboratory for Particle Physics with a proviso that 
;;; third party use acknowledge its source and contributors. So done.
;;;
;;; FUNCTIONS:
;;; `mon-lookup-string-se'
;;;
;;; CONSTANTS or VARIABLES:
;;; `*sting-se-glossary*', `*sting-se-glossary-terms*'
;;;
;;; MACROS:
;;;
;;; SUBST or ALIASES:
;;;
;;; DEPRECATED, RENAMED, OR MOVED:
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;; Both `*sting-se-glossary*', and `*sting-se-glossary-terms*'
;;; need to be hashed esp. once incorporated with a current version of FOLDOC.
;;;
;;; NOTES:
;;; Since assembling this list I've encountered
;;; *V.E.R.A. -- Virtual Entity of Relevant Acronyms*
;;; COURTESY: Oliver Heidelbach
;;; His Texinfo version of V.E.R.A is a special contribution to the GNU project
;;; contains approximately 11373 acronyms. Is available here:
;;; (URL `http://home.snafu.de/ohei/FTP/')
;;; V.E.R.A. is also avaialbe on the World Wide Web The WWW version contains an
;;; additional 1013 links to manufacturers and organizations.
;;; Its URL is: (URL `http://home.snafu.de/ohei/')
;;;
;;; A tarball of V.E.R.A 1.19 texi files is also avaiable on the emacswiki at:
;;; (URL `http://www.emacswiki.org/emacs/V_E_R_A')
;;;
;;; COURTESY: Ralf Wachinger's <rwnewsmampfer <#at#> geekmail.de>
;;; An Emacs lisp extension to V.E.R.A. rw-acronyms.el is available here:
;;; (URL `http://article.gmane.org/gmane.emacs.sources/3244') 
;;;
;;; See also; (URL `http://www.emacswiki.org/emacs/download/org-pua.el')
;;; Which is influenced by rw-acronyms.el
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;; ==============================
;;; Philip Hausler is a program manager of software technology in IBM's Worldwide
;;; Industry Solutions organization. Currently, he is responsible for the Enterprise
;;; Solution Structure, a business and framework architecture in support of This is
;;; a thumbnail picture of Philip Hauslerenterprise solutions across multiple
;;; industries. With IBM he has extensive experience in management, software product
;;; development, services consulting, and project management. He has strong
;;; interests in management and technology for software processes, project
;;; management, and advanced software engineering practices, including Cleanroom
;;; software engineering for developing ultra-high quality software with certified
;;; reliability.
;;;
;;; Since 1985, Philip has served on the faculties of the Computer Science
;;; departments at the University of Maryland and Johns Hopkins University, where he
;;; teaches undergraduate and graduate courses on software engineering, programming
;;; languages and compiler theory. He is a member of IEEE, IEEE Software Engineering
;;; Technical Committee, and was a panel member for the Department of Defense's
;;; Software Acquisition Best Practices Initiative.
;;;
;;; hausler@vnet.ibm.com
;;; OFFICE: (301) 803-2684
;;; HOME: (301) 725-1578
;;; FAX: (301) 803-3260
;;;
;;; (URL `http://www.apl.jhu.edu/Notes/Hausler/web/')
;;; (URL `http://www.apl.jhu.edu/Notes/Hausler/web/instructor.html')
;;; ==============================
;;; \(URL `http://www.apl.jhu.edu/Notes/Hausler/web/glossary.html')
;;; 
;;; Warning This is a long HTML file with a lot of links! It is not really intended
;;; to be read as a whole, which will take a long time. Instead, you can search for
;;; entries using the STING WWW information server.
;;;
;;; Copyright CERN, European Laboratory for Particle Physics.
;;;
;;; This compilation is derived from many sources, including public postings to
;;; Usenet newsgroups and contributions from individuals. If you make use of this
;;; material in any way other than by the STING information service, please
;;; acknowledge its source and contributors.
;;;
;;; 1996 September 13 -MS
;;; ==============================
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; FILE-CREATED:
;;; <Timestamp: Sunday August 02, 2009 @ 07:16.12 AM - by MON KEY>
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T18:15:58-04:00Z}#{09327} - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; ©opyright (C) MON KEY - 2009
;;; ================================================================
;;; CODE:


;;; ==============================
;;; regexp to turn file into an alist
;;;
;;; 1) set the region then call `mon-escape-lisp-string-region'
;;;
;;; 2)
;;;     (while (search-forward-regexp  " - More information" nil t)
;;;     	       (replace-match  ""))
;;;
;;; 3) 
;;;    (while (search-forward-regexp 
;;;    	    ;;..1...2.......3...........4.......5.........
;;;	     "\\(^\\(.*\\)\\(\n    \\)\\(.*\\)\\( $\\)\\)" nil t)
;;;       (replace-match  "(\"\\2\" \"\\4\")\n"))
;;;
;;; 4)
;;;  (while  (search-forward-regexp "^\\((\".*\" \\)") (replace-match "\\1\n "))

;;; ==============================
;;; TODO: Both `*sting-se-glossary*', and `*sting-se-glossary-terms*'
;;; need to be hashed after FOLDOC incorporation.
;;; CREATED: <Timestamp: 2009-08-09-W32-7T07:20:31-0400Z - by MON KEY>
(defvar *sting-se-glossary* 'nil
  "alist of the Sting Software Engineering Glossary.
Indexed by term where term is a keyed string -> alist val.
SOURCE: Philip Hausler:
\(URL `http://www.apl.jhu.edu/Notes/Hausler/web/glossary.html')
Hausler indicates the material is Copyright: 
CERN, European Laboratory for Particle Physics.\n
See also; `*sting-se-glossary-terms*', `mon-lookup-string-se'.")
;; 
(when (not (bound-and-true-p *sting-se-glossary*))
  (setq *sting-se-glossary* '(

("4GL" 
 "Fourth generation language.")

("88open" 
  "A consortium with the aim of creating a multivendor open computing environment based on the Motorola 88000 RISC processor family.")

("AAP DTD" 
 "A DTD for a standard SGML document type for scientific documents, defined by the AAP.")

("AAP" 
 "The Association of American Publishers: engaged in standardisation efforts in document preparation.")

("ABI" 
 "Application Binary Interface: the interface by which an application program gains access to operating system and other services, designed to permit porting of compiled binary applications between systems with the same ABI.")

("Abstract Class" "In object-oriented programming, a class designed only as a parent from which sub-classes may be derived, but which is not itself suitable for instantiation. Often used to \"abstract out\" incomplete sets of features which may then be shared by a group of sibling sub-classes which add different variations of the missing pieces.")

("ACA" 
 "Application Control Architecture: DEC's implementation of ORB")

("ACE" 
 "Advanced Computing Environment: a consortium to agree on an open architecture based on the MIPS R4000 chip. A computer architecture ARCS will be defined, on which either OS/2 or Open Desktop can be run.")

("ACE" 
 "Adaptive Communication Environment, a C++ Wrapper Library for communications from the University of California at Irvine.")

("ACM" 
 "Association for Computing Machinery.")

("Acrobat" 
 "A platform-independent text and image formatter/viewer from Adobe Systems.")

("Actis" 
 "An approach to integrated CASE by Apollo.")

("Active object" 
 "An object that encompasses its own thread of control.")

("Active DBMS" 
 "A conventional or passive DBMS combined with a means of event detection and condition monitoring. Event handling is often rule-based, as with an expert system.")

("ActiveX" 
 "A software development kit from Microsoft for develpment of Internet applications and content.")

("Actor" 
 "In object-oriented programming, an object which exists as a concurrent process.")

("Actor" 
 "A term in Chorus denoting the unit of resource allocation.")

("Actra" 
 "A multiprocessor Smalltalk project.")

("AD/Cycle" 
 "\(AD = Application Development\): a set of SAA-compatible IBM-sponsored products for program development, running on workstations accessing a central repository on a mainframe. The stages cover requirements, analysis and design,production of the application, building and testing, and maintenance. Technologies used include code generators and knowledge based systems, as well as languages and debuggers.")

("Ada" 
 "A high-level computer language sponsored by the US Department of Defense. It has a multitasking mechanism, and a number of features useful for software engineering.")

("AdaIC" 
 "Ada Information Clearinghouse.")

("Adaline" 
 "Name given by Widrow to ADAptive LInear NEurons, that is neurons \(see McCulloch-Pitts\) which learn using the Widrow-Huff Delta Rule \(see also Madaline\).")

("ADAMO" 
 "A data management system written at CERN based on the Entity-Relationship model.")

("Adaptable User Interface" "A toolkit from Oracle allowing applications to be written portably for different windowing systems. It provides one call level interface along with a resource manager and editor across a range of \"standard\" GUIs, including Macintosh, Windows and the X Window System.")

("Adaptive learning" 
 "Learning in which a system programs itself by adjusting weights or strengths until it produces the desired output. Same as Hebbian.")

("ADDD" 
 "A Depository of Development Documents. A public domain Software Engineering Environment from GMD developed as part of the STONE project.")

("ADL" 
 "Assertion \(or API\) Definition Language. A project for Automatic Interface Test Generation.")

("ADT" 
 "Abstract Data Type: a class of data structures described by means of a set of operations rather than by physical representation, such as a class in object-oriented programming..")

("Aegis" 
 "A CASE tool for project change management, part of the GNU software.")

("AENOR" 
 "Asociacion Espanola de Normalizacion y Certificacion. The Spanish standards organisation.")

("AEP" 
 "Application environment profile.")

("AES" 
 "Application environment specification: a set of specifications from OSF for programming and user interfaces, aimed at providing a consistent application environment on different hardware platforms. It includes O/S for the operating system \(user commands and program interfaces\), U/E for the User Environment \(Motif\), and N/S for Network services.")

("AFIPS" 
 "American Federation of Information Processing Societies.")

("AFNOR" 
 "Association Francaise pour la Normalisation: the French national standards institute, a member of ISO.")

("AFS" 
 "Andrew File System.")

("AGOCG" 
 "Advisory Group on Computer Graphics. Advising UK Higher Education on Computer Graphics, Visualization and Multimedia.")

("AGL" 
 "Atelier de Genie Logiciel: French for IPSE.")

("AI" 
 "Artificial Intelligence.")

("AIA" "Application Integration Architecture: DEC's \"open standards\" 
 specifications.")

("AICA" 
 "Associazione Italiana di Calcolo Automatico.")

("AIFF" 
 "Audio IFF. A format developed by Apple for storing high-quality sampled sound and musical instrument info; also used by SGI and several professional audio packages.")

("AIS" 
 "Advanced Informatics Support project for administrative work at CERN.")

("AIX" 
 "Advanced Interactive eXecutive: IBM's version of UNIX, taken as the basis for the OSF standard.")

("Algol" 
 "A high-level programming language developed in the 1950s .")

("Algorithm" 
 "A systematic procedure guaranteed to produce a result after a finite number of steps.")

("Alvey" 
 "A funding programme for collaborative research in the UK.")

("ami" 
 "Applications of Metrics in Industry \(Assess, Analyze, Metricate, Improve\). A method for software project management and process improvement.")

("Amoeba" 
 "A distributed operating system developed by A.Tanenbaum and others at Amsterdam.")

("AMS" 
 "Andrew Message System.")

("AMADEUS" 
 "A PC client for Hyper-G.")

("Analysis" 
 "The part of the software development process concerned with defining the requirements for the product.")

("ANDF" 
 "Architecture-Neutral Distribution Format: an emerging OSF standard for software distribution. Programs are compiled into ANDF before distribution, and executables are produced from it for the local target system.")

("Andrew File System" 
 "The distributed file system of the Andrew project, adopted by the OSF as part of their DCE.")

("Andrew Message System" 
 "A multimedia interface to electronic mail and bulletin boards, developed as part of the Andrew project")

("Andrew Project" 
 "A distributed system project for support of educational and research computing at Carnegie Mellon University.")

("Andrew Toolkit" 
 "A portable user interface toolkit developed as part of the Andrew project, running on the X Window System and distributed with X11R5.")

("ANL" 
 "Argonne National Laboratory, USA.")

("Anna" 
 "A specification language from Stanford University for formally specifying Ada programs. It has a Specification Analyzer and a Consistency Checking System.")

("Annealing" 
 "A technique which can be applied to any minimization or learning process based on successive update steps \(either random or deterministic\) where the update step length is proportional to an arbitrarily set parameter which can play the role of a temperature. Then, in analogy with the annealing of metals, the temperature is made high in the early stages of the process for faster minimization or learning, then is reduced for greater stability.")

("ANSA" 
 "Advanced Network Systems Architecture: an architecture for distributed computer systems based on a model developed as an Esprit project.")

("ANSI Z39.50" 
 "See Z39.50.")

("ANSI/SPARC Architecture" 
 "A layered model of database architecture comprising a physical schema, a conceptual schema, and user views.")

("ANSI" 
 "American National Standards Institute, responsible for approving U.S. standards in many areas, including computers and communications. ANSI is a member of ISO.")

("AnswerGarden" 
 "A help desk software package from MIT.")

("AOCE" 
 "Apple Open Collaboration Environment. A set of software for e-mail, directory services etc.")

("APA" 
 "Application Portability Architecture: DEC's plan for portable applications software.")

("apE" 
 "A graphics package from the Ohio Supercomputer Centre.")

("Apertos" 
 "An object-oriented operating system from Sony Computer Science Laboratory.")

("API" 
 "Application Program Interface: a term for the interface by which an application program gains access to operating system and other services, defined at source-code level.")

("APL" 
 "A Programming Language developed by Iverson for mathematical applications.")

("Apollo" 
 "Apollo Computer, now a division of Hewlett-Packard, also the name of a range of workstations manufactured by this company.")

("AppKit" 
 "A set of objects used by the application builder for the NeXTstep environment.")

("Applet" 
 "A small application, often downloaded from a remote server and run in a controlled environment. Typically written in a language such as Java for execution by a WWW browser.")

("Apple" 
 "Apple Computer Inc, manufacturers of the Macintosh range of Personal Computers.")

("Appletalk" 
 "The proprietary local area network protocol developed by Apple for their Macintosh range of processors. Current implementations exist on Localtalk and Ethertalk.")

("APSE" 
 "Ada Programming Support Environment.")

("ARC" 
 "\(Previously ARCS\) Advanced RISC Computing Specification: the standard hardware architecture of ACE., specifying the baseline hardware requirements to create ACE-compatible systems.")

("Arcadia" 
 "A software engineering research project by a consortium of US universities.")

("Archie" 
 "An archive server database and query system operated by the McGill University School of Computer Science. Services remote requests for information on software kept on archives worldwide and available via ftp.")

("ARCS" 
 "see ARC.")

("Arjuna" 
 "A system for reliable distributed computing from the Computing Laboratory, University of Newcastle upon Tyne. It supports atomic transactions on persistent objects.")

("ARL" 
 "ASSET Reuse Library.")

("ARL" 
 "Association of Research Libraries \(North America\).")

("ARPANET" 
 "U.S. Department of Defense \(DARPA\) wide area network. It became operational in 1968 and was the forerunner of the Internet.")

("Artifex" 
 "A CASE environment from ARTIS of Turin for the development of large event-driven distributed systems. It has code-generation and rapid prototyping features.")

("Artificial Intelligence" 
 "The subfield of computer science concerned with the concepts and methods of symbolic inference by computer, and the symbolic representation of the knowledge to be used in making inferences.")

("ASCII" 
 "American Standard Code for Information Interchange.")

("ASDL" 
 "Abstract-Type and Scheme-Definition Language: developed as part of Esprit project GRASPIN, as a basis for generating language-based editors and environments. It combines an object-oriented type system, syntax-directed translation schemes and a target-language interface.")

("ASE" 
 "Advanced Software Environment: an object-oriented application support system from Nixdorf.")

("ASIC" 
 "Application-Specific Integrated Circuit: an integrated circuit designed to perform a particular function by defining the interconnection of a set of basic circuit building blocks drawn from a library provided by the circuit manufacturer.")

("ASIS" 
 "Ada Semantic Interface Specification. An interface between an Ada library and any tool requiring information in it.")

("ASIS" 
 "Application Software Installation Server at CERN.")

("ASME" 
 "American Society of Mechanical Engineers: involved in CAD standardisation.")

("ASN.1" 
 "Abstract Syntax Notation 1: an ISO/CCITT standard for the description of data. It is intended to facilitate the exchange of data between application programs.")

("ASPECT" 
 "An IPSE developed by an Alvey project, using Z to specify the object-management system and tool interface.")

("ASQ" 
 "Automated Software Quality. The use of software tools, such as automated testing tools, to improve software quality.")

("ASQC" 
 "American Society for Quality Control.")

("ASSET" 
 "Asset Source for Software Engineering Technology. A programme to promote software reuse by the DoD.")

("AtFS" 
 "Attributed File System: the basis of the Shape_VC toolkit. Cooperative work within projects is supported by a status model controlling visibility of version objects, locking, and \"long transactions\" for synchronizing concurrent updates. The concept of object attributes provides a basis for storing management information with versions and passing this information between individual tools. This mechanism is useful for building integrated environments from a set of unrelated tools.")

("Athena" 
 "Project Athena: a distributed system project for support of educational and research computing at MIT. Much of the software developed is now in wider use, especially the X Window System.")

("Atherton" 
 "Atherton Technology developed the Software BackPlane CASE framework. Their Atherton Tool Integration Services were the basis for the ATIS standard.")

("ATIS" 
 "A Tools Integration Standard: an object-oriented interface to a set of services that allows the saving, accessing, and managing of information in a common repository. Developed by Atherton Technology and DEC, based on an extended version of the Software BackPlane, now proposed as an industry standard.")

("ATK" 
 "The Andrew Toolkit")

("ATM" 
 "Asynchronous Transfer Mode. A transmission system for telecommunications.")

("ATM" 
 "Adobe Type Manager.")

("AUE" 
 "Andrew User Environment. Part of the Andrew project")

("AUI" 
 "Adaptable User Interface from Oracle.")

("AUIS" 
 "Andrew user Interface System.")

("AutoCAD" 
 "A CAD software package for mechanical engineering marketed by Autodesk Inc.")

("AVL" 
 "Abstract Visualization Language in the Tecate project.")

("AVS" 
 "Application Visualisation System: a portable modular UNIX-based graphics package supported by a consortium of vendors including Convex, DEC, IBM, HP, SET Technologies, Stardent and WaveTracer.")

("AWK" 
 "A pattern scanning and processing language named after its authors: Aho, Weinberger and Kernighan.")

("aXe" 
 "A text editor for the X-Window-System.")

("B" 
 "A Formal method of program design.")

("Bachman" 
 "Proposed a style of Entity-Relationship modeling which differs from the original Chen proposals.")

("Back-propagation" 
 "An important algorithm for learning in feed-forward networks which makes use of a mathematical trick when the network is simulated on a digital computer, yielding in just two traversals of the network \(once forward, and once back\) both the difference between the desired and actual output, and the derivatives of this difference with respect to the connection weights.")

("Backus Naur" 
 "A formal language for syntax specification.")

("Bamboo" 
 "A trusted third-party authentication system from the University of Iowa, similar to Kerberos.")

("Baseline" 
 "See Released version.")

("BASIC" 
 "Beginners All-purpose Symbolic Instruction Code: a programming language, usually interpreted, suitable for simple applications.")

("BBN" 
 "Bolt Beranek and Newman Inc.,of Cambridge, Massachusetts, was awarded the original contract to build the ARPANET and has been extensively involved in Internet development. It is responsible for managing NNSC, CSNET, and NEARnet.")

("BCS" 
 "Binary Compatibility Standard: the ABI of 88open.")

("BCS" 
 "British Computer Society.")

("BEA" 
 "Basic programming Environment for interactive-graphical Applications, from Siemens-Nixdorf.")

("Bedrock" 
 "A C++ class library for Macintosh user interface portability.")

("Benchmark" 
 "A standard set of programs which can be run on different platforms to compare performance.")

("Bento" 
 "A multi-vendor initiative allowing files to contain typed parts, to allow standard access between parts of a compound document independent of the file system.")

("Berkeley UNIX" 
 "see BSD.")

("BETA" 
 "An object-oriented language and associated programming environment from Mjolner Informatics, Aarhus.")

("BHT" 
 "Budget Holder's Toolkit \(at CERN\).")

("BITNET" 
 "An academic and research network connecting approximately 2500 computers, often IBM mainframes. It provides interactive electronic mail, and file transfer services via a store-and-forward technique based on IBM NJE protocols. BITNET and Internet traffic are exchanged via several gateway hosts. It is now operated by CREN.")

("BMP" 
 "Bitmap format \(for Windows\).")

("BNF" 
 "Backus-Naur Form.")

("BOCS" 
 "Berard Object and Class Specifier, an Object-oriented CASE tool from Berard Software Engineering.")

("Boehm B." 
 "Proposed the COCOMO technique for evaluating the cost of a software project.")

("BoM" 
 "Bill of Materials.")

("BON" 
 "Better Object Notation. Used in the Esprit Business Classes project.")

("Bookreader" 
 "DEC's CD-ROM-based online documentation browser.")

("Bookviewer" 
 "A hypertext documentation system from Oracle based on Oracle Toolkit. It allows the user to create private links and bookmarks, and to make multimedia annotations.")

("BOOM" 
 "Berard Object-Oriented Methodology.")

("BOS" 
 "A data management system written at DESY and used in some HEP programs.")

("Bourne shell" 
 "A common UNIX shell.")

("BPM" 
 "Business Process Modelling.")

("BPR" 
 "Business Process Reengineering.")

("Browser" 
 "A tool for navigating around hypertext documents.")

("BSD" 
 "Berkeley Source Distribution: the versions of UNIX developed and distributed by the University of California at Berkeley. Many commercial UNIX implementations such as SunOS and Dynix are derived from it.")

("BSI" 
 "British Standards Institution: a member of ISO.")

("BSP method" 
 "A CASE method from IBM .")

("Byte" 
 "A data unit of several bits smaller than a computer word: usually 8 bits.")

("C++" 
 "An extension to the C language developed primarily by B.Stroustrup at AT&T Bell Laboratories: it supports object-oriented programming among other enhancements.")

("C Beautifier" 
 "A tool for tidying the syntax of C source code.")

("c shell" 
 "A common UNIX shell originating on Berkeley UNIX.")

("C" 
 "A language developed in conjunction with the UNIX operating system at AT&T Bell Laboratories by D.Ritchie and now an ANSI standard. It has grown popular due to its simplicity, efficiency, and flexibility. C programs are often easily adapted to new environments.")

("Cache" 
 "A small fast memory holding recently-accessed data, designed to speed up further access.")

("CACI" 
 "A company marketing SIMSCRIPT, MODSIM, and other simulation software products.")

("CACM" 
 "Communications of the ACM.")

("CAD/CAM" 
 "Computer Aided Design/Computer Aided Manufacturing \(see CAD\)")

("CAD" 
 "Computer Aided Design: usually applied to that part of CAE which has to do with the drawing or physical layout steps of engineering design.")

("CADD" 
 "Computer Aided Detector Design: a project to develop standards and methods to allow cooperation between HEP detector designers working in different institutes.")

("CADRE" 
 "A software engineering vendor in the US.")

("CAE" 
 "Common Applications Environment of X/Open, based on POSIX and C.")

("CAE" 
 "Computer Aided Engineering: a technique for using computers to help with all phases of engineering design work. As CAD, but also involving the conceptual and analytical design steps.")

("CAI" 
 "Computer Aided Instruction.")

("CAIS-A" 
 "Common APSE Interface Set: DoD-STD-1838A.")

("CAIS" 
 "Common APSE Interface Specification.")

("CAiSE" 
 "Conference on Advanced Information Systems Engineering.")

("CAJUN" 
 "CD-ROM Acrobat Journals Using Networks. A project at Nottigham University More information.")

("CALS" 
 "Computer-Aided Acquisition and Logistics Support: a DoD standard for electronic exchange of data with commercial suppliers.")

("Caml" 
 "A functional programming language in the style of ML.")

("CApH" 
 "Conventions for the Application of HyTime. An activity of the GCA")

("CAQ" 
 "Computer Aided Quality.")

("CARDS" 
 "Central Archive for Reusable Defense Software of the DoD .")

("CASE*Method" 
 "An analysis and design method from Oracle, targeted at information management applications.")

("CASE framework" 
 "A set of products and conventions that allow CASE tools to be integrated into a coherent environment.")

("CASE tools" 
 "Software tools to help in the application of CASE methods to a software project.")

("CASE" 
 "Computer Aided Software Engineering: a technique for using computers to help with the systematic analysis, design, implementation and maintenance of software. Adopting the CASE approach to building and maintaining systems involves software tools and training for the developers who will use them.")

("CAST" 
 "Computer Aided Software Testing.")

("CATE" 
 "Computer Aided Test Engineering: CASE methods applied to electronics testing and linked to CAE")

("CAUSE" 
 "An international \(mainly North American\) nonprofit association for managing and using information technology in higher education.")

("cb" 
 "C Beautifier.")

("CBT" 
 "Computer-Based Training.")

("CCI" 
 "Common Client Interface for Mosaic")

("CCITT" 
 "A committee of the ITU responsible for making technical recommendations about telephone and data communication systems for PTTs and suppliers. Plenary sessions are held every four years to adopt new standards.")

("CCL" 
 "Common Command Language. A standard for bibliographic information retrieval systems.")

("CCS" 
 "Common Communication Services: the standard program interface to networks in SAA.")

("CDA" 
 "Compound Document Architecture: DEC's set of standards for compound document creation, storage, retrieval, interchange and manipulation.")

("CDC" 
 "Control Data Corporation")

("CDD/Plus" 
 "DEC's CASE repository.")

("CDE" 
 "C Development environment from IDE")

("CDE" 
 "Common Desktop Environment. A Desktop manager from COSE.")

("CDF" 
 "Common Data Format. A library and toolkit for multi-dimensional data sets.")

("CDM" 
 "Content Data Model. An SGML-based DoD specification for interactive manuals.")

("CDIF" 
 "CASE Data Interchange Format: an emerging standard.for interchange of data between CASE tools.")

("CE" 
 "Concurrent Engineering.")

("CEBAF" 
 "Continuous Electron Beam Facility in Newport News, VA USA.")

("Cecil" 
 "An object-oriented language from Washington University intended to support rapid construction of high-quality, extensible software.")

("CEN" 
 "Conseil Europeen pour la Normalisation: a body coordinating standardisation activities in the EEC and EFTA. countries.")

("CERA" 
 "Concurrent Engineering: Research and Applications. An international journal.")

("CERC" 
 "Concurrent Engineering Research Center, West Virginia University.")

("CERN" 
 "The European Laboratory for Particle Physics.")

("CERNLIB" 
 "The CERN Program Library.")

("CERT" 
 "Computer Emergency Response Team. Now CERT Coordination Center, works with the Internet community on security problems.")

("CENELEC" 
 "CEN-electricite.")

("CFI" 
 "CAD Framework Initiative. A consortium working on interface standards for integrating CAD tools and data.")

("CFOOT" 
 "Corporate Facilitators of Object-Oriented Technology.")

("CGI" 
 "Common Gateway Interface. A standard for running external programs under a WWW or similar information server.")

("CGI" 
 "A \(French\) software engineering vendor in the US.")

("CGM" 
 "Computer Graphics Metafile: a standard file format for storage and communication of graphical information, widely used on personal computers and accepted by desktop publishing systems. \(ANSI/ISO 8632-1987\).")

("Change Management" 
 "A consistent set of techniques that aid in evolution, composition and policy management of the design and implementation of an object or system.")

("Charm" 
 "A portable object-oriented parallel programming system from University of Illinois.")

("Chen" 
 "Peter Chen developed the Entity-Relationship model.")

("CHEOPS" 
 "A satellite-based batch data dissemination project between CERN and member state institutes.")

("Child version" 
 "A version of a version. See change management.")

("CHILL" 
 "CCITT High-Level Language. A real-time language used in telecommunications.")

("Choices" 
 "An object-oriented operating system from University of Illinois.")

("Chorus" 
 "A distributed operating system developed at INRIA.")

("CIAC" 
 "Computer Incident Advisory Capability of the US DoE.")

("CIC" 
 "Committee on Institutional Cooperation. An academic consortium of American Universities.")

("CICERO" 
 "Control Information system Concepts based on Encapsulated Real-time Objects. A CERN DRDC proposal.")

("CIDR" 
 "Classless Inter-Domain Routing \(on the Internet\)")

("CIL" 
 "Component Integration Laboratories. An effort to create a common framework for interoperability between applications on desktop platforms, formed by Apple, IBM, Novell, Oracle, Taligent, WordPerfect, and Xerox.")

("CIM" 
 "Computer Integrated Manufacturing.")

("CIS" 
 "Case Integration Services: a committee formed to discuss CASE tool integration standards related to ATIS.")

("CISC" 
 "Complex Instruction Set Computer.")

("CISI" 
 "A French software house.")

("CIX" 
 "Commercial Internet eXchange. A non-profit trade association of Public Data Internetwork service providers.")

("CL" 
 "See Common Lisp.")

("Class-Relation Method" 
 "A design technique based on the concepts of object-oriented programming and the Entity-Relationship model from the French company Softeam.")

("Class" 
 "A language developed by the Andrew Project: one of the first attempts to add object-oriented features to C.")

("Class" 
 "The prototype for an object in an object-oriented language; analogous to a derived type in a procedural language.")

("Class library" 
 "A library of reusable classes for use with an object-oriented programming system.")

("Cleanroom" 
 "A software development approach aimed at producing software with the minimum number of errors.")

("Client" 
 "A system or process that requests a service from another system or process.")

("CLHEP" 
 "A C++ class library for high energy physics applications.")

("CLOS" 
 "Common Lisp Object System: an object-oriented language derived from Common Lisp.")

("CLP" 
 "Constraint Logic Programming.")

("CLU" 
 "An object-oriented programming language developed at MIT by Liskov et al.")

("CLX" 
 "The Common Lisp interface to the X Window System, equivalent to Xlib.")

("CM" 
 "Configuration Management.")

("CMA" 
 "Concert Multithread Architecture from DEC .")

("CML" 
 "Chemical Markup Language. A means for interchanging chemical information, based on SGML.")

("CMM" 
 "Capability Maturity Model for software development organisations, from SEI.")

("CMS" 
 "A code management system from DEC.")

("CMVC" 
 "Configuration Management Version Control from IBM.")

("CMZ" 
 "A portable interactive code management system from CodeME S.A.R.L in use in the high-energy physics community.")

("CNET" 
 "Centre national d'Etudes des Telecommunications: the French national telecommunications research centre at Lannion.")

("CNI" 
 "Coalition for Networked Information. Promotes the creation of and access to information resources in networked environments in order to enrich scholarship and enhance intellectual productivity.")

("CNRI" 
 "Corporation for National Research Initiatives, Reston, VA. A US research and development organisation in information processing technology.")

("COBOL" 
 "COmmon Business Oriented Language: an early and widely-used programming language for business applications.")

("COCOMO" "Constructive Cost Model: a method for evaluating the cost of a software package proposed by B.Boehm, \"Software Engineering Economics\" 
 Prentice-Hall 1987.")

("CODA" 
 "An object-oriented data-acquisition system at CEBAF.")

("Codd's First Normal Form" 
 "see Normal Form.")

("Code Management" 
 "A source code management system helps program developers keep track of version history, releases, parallel versions etc. There are several in popular use.")

("CodeCenter" 
 "A proprietary software development environment for C programs, offering an integrated toolkit for developing, testing, debugging and maintainance \(formerly Saber-C\)")

("Cognitech" 
 "A French software house specialising in Artificial Intelligence.")

("COHESION" 
 "DEC's CASE environment.")

("Collage" 
 "A synchronous collaborative data analysis tool for use over the Internet, from NCSA.")

("COM" 
 "Common Object Model. An open architecture from DEC and Microsoft, allowing interoperation between ObjectBroker and OLE.")

("COMIS" 
 "a COMpilation and Interpretation System. A FORTRAN interpreter use by the PAW system.")

("COMMA" 
 "Common Object-oriented Methodology Metamodel Architecture from OPEN.")

("Common Lisp" 
 "An ANSI standard version of Lisp.")

("COMNET" 
 "A simulation tool from CACI for analysing wide-area voice or data networks, based on SIMSCRIPT..")

("Compaq" 
 "A US manufacturer of IBM PC-compatibles.")

("Compression" 
 "Data files are often compressed to take up less network bandwidth, memory etc. Common examples are program executables and visual images. Many algorithms and utilities exist for this.")

("COMSOFT" 
 "Consortium for the Management of Emerging Software Technologies.")

("Concrete Class" 
 "In object-oriented programming, a class suitable to be instantiated.\(as opposed to an abstract class\).")

("Concurrent Clean" 
 "A functional language for the Macintosh from the University of Nijmegen.")

("Concurrent Engineering" 
 "An approach where all aspects of a product's life-cycle are considered as early as possible in the design, manufacturing and maintenance process.")

("Configuration management" 
 "The process of identifying, defining, recording and reporting the configuration items in a system and the change requests. Controlling the releases and change of the items throughout the life-cycle See also code management.")

("Constructor" 
 "A function provided by a class in C++ to instantiate an object.")

("Container class" 
 "A class whose instances are collections of other objects. Examples include stacks, queues, lists and arrays.")

("CooL" 
 "Combined object-oriented Language from the ITHACA Esprit project, which combines C-based languages with database technology.")

("COOL" 
 "A class library for C++ from Texas Instruments.")

("COOTS" 
 "Conference on Object-Oriented Technologies and Systems.")

("CORBA" 
 "Common Object Request Broker Architecture: an OMG specification.")

("CORDIS" 
 "The European Community R&D information service.")

("CORE" 
 "Chemistry Online Retrieval Experiment. A project to publish American Chemical Society journals electronically.")

("Cortex" 
 "An experimental slow controls project at CERN.")

("COS" 
 "Corporation for Open Systems: an international consortium of computer users and vendors, set up to provide ways of testing OSI implementations.")

("COSE" 
 "Common Open Software Environment. An initiative by Hewlett-Packard, Sun, IBM, Novell, Univel and SCO to move towards consistency and interopability between Unix suppliers.")

("COSS" 
 "Common Object Services Specification in CORBA.")

("COSINE" 
 "Cooperation for Open Systems Interconnection Networking in Europe. A EUREKA project.")

("CoST" 
 "A set of software tools for SGML documents.")

("COTS" 
 "Commercial Off The Shelf solution.")

("CPAN" 
 "Comprehensive Perl Archive Network.")

("CPI" 
 "Common Program Interface: the API of SAA.")

("CPSR" 
 "Computer Professionals for Social Responsibility. A US non-profit organisation concerned with the effects of computers on society.")

("CPU" 
 "Central Processing Unit, usually applied to that part of a computer which carries out the arithmetic and controls the instruction flow.")

("CRAY" 
 "Cray Research Inc.: manufacturers of a range of large powerful mainframes.")

("CRC" 
 "Class-Responsibility-Collaboration. A technique described in Object-Oriented Software by Wirfs-Brock.")

("CREASE" 
 "Catalog of Resources for Education in Ada and Software Engineering. A database maintained by AdaIC.")

("CREN" 
 "Corporation for Research and Educational Networking: responsible for providing networking service to BITNET and CSNET users.")

("cron" 
 "The clock daemon in UNIX that executes commands at specified dates and times according to instructions in a file.")

("Cross software" 
 "Software developed on one kind of computer for use on another \(usually because the other computer does not have itself adequate facilities for software development\).")

("CRS4" 
 "Centro di Ricerca, Sviluppo e Studi Superiori in Sardegna. \( Center for Advanced Studies, Research and Development in Sardinia\). A high performance computing centre with an interesting information server.")

("CSCW" 
 "Computer Supported Cooperative Work \(also known as Groupware\): software tools and technology to support groups of people working together on a project, often at different sites.")

("csh" 
 "See c shell")

("cshell" 
 "See c shell")

("CSL" 
 "Caml Special Light. An implementation of Caml.")

("CSMA/CD" 
 "Carrier Sense Multiple Access with Collision Detection: a network arbitration scheme used on Ethernet. A station with a message to send starts sending if there is no carrier detected on the transmission medium. If a collision occurs, transmission is abandoned and retried after a delay.")

("CSNET" 
 "Computers and Science Network, operated by CREN for US computer science institutes. It provides electronic mail service via dial-up lines, plus X.25 and Internet services.")

("CSP" 
 "Communicating Sequential Processes. A programming model developed by T. Hoare at Oxford University.")

("CSS" 
 "Cascading Style Sheets. A simple mechanism for adding style to WWW documents.")

("CSTC" 
 "Computer Security Technology Center of the US DoE.")

("CTAN" 
 "Comprehensive TeX Archive Network.")

("CTI" 
 "Computer Telephony Integration.")

("CUA" 
 "Common User Access: the User Interface standard of SAA.")

("curses" 
 "A set of subroutines in UNIX for handling navigation on a terminal screen using the cursor.")

("CVS" 
 "A code management system based on RCS.")

("CWI" 
 "Dutch Centre for Mathematics and Computer Science, Amsterdam.")

("CWIS" 
 "Campus-Wide Information System. Many universities and other institutes have computerised information systems, often based on WWW or gopher")

("DAA" 
 "Distributed Application Architecture: under design by Hewlett-Packard and Sun. A distributed object management environment that will allow applications to be developed independent of operating system, network or windowing system.")

("DACNOS" 
 "A prototype network operating system for multivendor environments, from IBM European Networking Centre Heidelberg and University of Karlsruhe.")

("DAD" 
 "Distributed Adamo Database. An extension to ADAMO.")

("daemon" 
 "A process running in the background performing some service \(such as handling print queues\) in UNIX or other operating systems.")

("DANTE" 
 "A company established by the national research networks in Europe to provide international network services.")

("DARPA" 
 "Defense Advanced Research Project Agency of the US Department of Defense,.responsible for the development of new technology, including ARPANET.")

("DASE" 
 "Distributed Application Support Environment .")

("Data base" 
 "See DBMS")

("Data Definition Language" 
 "A language enabling the structure and instances of a database to be defined in a human- and machine-readable form.")

("Data dictionary" 
 "A set of data descriptions that can be shared by several applications.")

("Data Flow Diagram" 
 "A graphical notation used to describe how data flows between processes in a system. An important tool of most structured analysis techniques.")

("Data Model" 
 "A set of data structures with manipulation and validation operators for general purpose usage. Examples are the Entity-Relationship model and NIAM")

("Data Warehouse" 
 "A database of information intended for use as part of a decision support system. The data is typically extracted from an organisation's operational databases.")

("Database" 
 "See DBMS.")

("Datacom" 
 "A DBMS from Computer Associates International..")

("DATATRIEVE" 
 "A query and report system for use with DEC's VMS system \(RMS, VAX Rdb/VMS or VAX DBMS\).")

("DataViews" 
 "Graphical user interface development software from V.I. Corporation, aimed at constructing platform-independent interactive views of dynamic data.")

("DAZIX" 
 "Daisy/Cadnetix Corporation: a supplier of digital electronic CAE systems.")

("DB2" 
 "A DBMS from IBM.")

("DB" 
 "Database.")

("DBA" 
 "DataBase Administrator.")

("dBASE III" 
 "A DBMS from Ashton-Tate Corporation.")

("DBMS" 
 "Database management system: such systems typically manage large structured sets of persistent data, offering ad hoc query facilities to many users. They are widely used in business applications: commercial examples include Ingres, Oracle, Sybase etc.")

("DCA" 
 "Document Content Architecture.from IBM")

("DCE" 
 "Distributed Computing Environment from OSF.")

("DCF" 
 "Document Composition Facility.")

("DCOM" 
 "Distributed Component Object Model Protocol.")

("DCSA" 
 "Distributed Component Software Architecture.")

("DD" 
 "Data Dictionary.")

("DDE Manager" 
 "An Oracle product that lets Windows applications that support the DDE protocol act as front end tools for Oracle. It allows applications like Excel, Word, Ami Professional, WingZ, and ToolBook to query, update, graph, and report information stored in Oracle.")

("DDE protocol" 
 "Dynamic Data Exchange: a Microsoft protocol that allows Windows applications to communicate using a client/server model.")

("DDIF" 
 "Digital Document Interchange Format. A CDA specification for representing compound documents in revisable format; a DEC standard for document encoding.")

("DDL" 
 "Data definition language.")

("DDL" 
 "Document Description Language.")

("DDTS" 
 "Distributed Defect Tracking System.")

("DEC" 
 "Digital Equipment Corporation: a computer manufacturer and software vendor.")

("DECdesign" 
 "A software analysis and design tool from DEC supporting several methodologies.")

("DECdns" 
 "Distributed Naming Service: adopted by OSF as the naming service for DCE.")

("DECnet" 
 "The network marketed by DEC to connect its computers together.")

("DECstation" 
 "A range of RISC based workstations manufactured by DEC.")

("DECwindows" 
 "DEC's windowing environment based on the X Window System.")

("DECwrite" 
 "DEC's CDA-based, WYSIWYG document processing application. It can generate and import SGML marked-up documents.")

("Delphi" 
 "An object-oriented development system from Borland.")

("Delta" 
 "The information which differentiates a version from members of its immediate family. See change management")

("Delta-4" 
 "Definition and Design of an open Dependable Distributed system architecture. An Esprit project investigating the achievement of dependability in open distributed systems, including real-time systems.")

("DELTASE" 
 "A distributed processing environment concerned with fault-tolerant and process-control applications from the Esprit Delta-4 project.")

("DEM" 
 "Digital Elevation Model. A format for map files.")

("DeMarco" 
 "Tom DeMarco proposed a form of Structured Analysis.")

("Demeter" 
 "A CASE tool developed mainly by Karl Lieberherr \(see Aug/Sep 1988 issue of JOOP, OOPSLA '89 Proceedings \"Contributions to Teaching Object-Oriented Design and Programming\"\)")

("DES" 
 "Data Encryption Standard. A NIST encryption standard.")

("Design" 
 "Design is usually considered to be the phase of software development following analysis, and concerned with how the problem is to be solved.")

("Design recovery" 
 "A subset of reverse engineering in which domain knowledge, external information, and deduction of fuzzy reasoning are added to the observations of the subject system to identify meaningful higher level abstraction beyond those obtained directly by examining the system itself.")

("Desktop manager" 
 "A user interface to system services, usually icon and menu based like the Macintosh Finder, enabling the user to run applications and use a filing system without directly using the command language of the operating system.")

("DESQview" 
 "A system from Quarterdeck Office Systems implementing multitasking under MS-DOS.")

("Destructor" 
 "A function provided by a class in C++ to delete an object.")

("DESY" 
 "Deutsches Electronen Synchrotron Laboratory, Hamburg, Germany.")

("Development" 
 "The process of analysis, design, coding and testing software.")

("DFD" 
 "Data Flow Diagram.")

("DGL" 
 "Data Generation Language: a tool for generating test data for hardware or software systems.")

("DGL" 
 "The distributed version of GL .")

("Dhrystone" 
 "A benchmark program in C and Ada.")

("DIALOG" 
 "A commercial bibliographic database and retrieval service from DIALOG Information Services.")

("DIB" 
 "Device Independent Bitmap, a format for portable images.")

("Dienst" 
 "A protocol for a distributed digital document library built on http.")

("DII" 
 "Dynamic Invocation Interface. An OMG specification.")

("DIIG" 
 "Digital Information Infrastructure Guide. A resource to facilitate the development of the NII.")

("DIN" 
 "Deutsche Institut fuer Normung: the German standardisation body, a member of ISO.")

("DIP" 
 "Document Image Processing: storage, management and retrieval of images.")

("Dirt" 
 "Design In Real Time: a user interface builder for the X Window System by R.Hesketh")

("DISA" 
 "Data Interchange Standards Association \(USA\)")

("DISA" 
 "Defense Information Systems Agency \(USA\).")

("Display PostScript" 
 "An extended form of PostScript permitting its interactive use with bitmap displays.")

("DL/I" 
 "The data manipulation language of IMS.")

("DLG" 
 "Digital Line Graph. A format for map files.")

("DLM" 
 "Distributed Lock Manager on distributed VMS systems.")

("DME" 
 "Distributed Management Environment: an OSF standard presently at the RFT stage.")

("DMS" 
 "Document Management System.")

("DNS" 
 "Distributed Name Service: see DECdns.")

("DOC" 
 "Distributed Object Computing.")

("Document Examiner" 
 "A high-performance hypertext system by Symbolics that provides on-line access to their user documentation.")

("Document Style Semantics and Specification Language" 
 "An ISO standard under preparation, addressing the semantics of high-quality composition in a manner independent of particular formatting systems or processes. DSSSL is intended as a complementary standard to SGML for the specification of semantics.")

("DoD-STD-2167A" 
 "A DoD standard specifying the overall process of development and documentation for mission-critical software.")

("DoD-STD-2168" 
 "A DoD standard for software quality assurance procedures.")

("DoD" 
 "The US Department of Defense, responsible for sponsoring many standards in the software engineering field.")

("DoE" 
 "The US Department of Energy.")

("DOE" 
 "Distributed Object Environment: a distributed object-oriented application framework from SunSoft.")

("Domain" 
 "Distributed Operating Multi Access Interactive Network:the proprietary network protocol used by Apollo workstations.")

("DOMF" 
 "Distributed Object Management Facility: an OMG-compliant object management system; part of DOE. from SunSoft.")

("DOORS" 
 "Dynamic Object Oriented Requirements System.")

("DORIS" 
 "3-10 GeV center of mass electron-positron storage ring/collider at DESY.")

("DPS" 
 "Display PostScript.")

("DQO" 
 "Data Quality Objectives.")

("DRAGON" 
 "An Esprit project aimed at providing effective support to reuse in real-time distributed Ada applications..")

("DRAGOON" 
 "A distributed concurrent object-oriented Ada-based language from the Esprit DRAGON project.")

("DSE" 
 "Data Structure Editor.")

("DSDM" 
 "Dynamic Systems Development Method. A non-proprietary Rapid Application Development method.")

("DSEE" 
 "Domain Software Engineering Environment: a proprietary CASE framework and configuration management system from Apollo.")

("DSOM" 
 "Distributed SOM")

("DSP" 
 "Digital Signal Processing.")

("DS" 
 "Dansk Standard. The Danish standards association.")

("DSS" 
 "Decision Support Systems. Software tools to help with management tasks.")

("DSSSL" 
 "Document Style Semantics and Specification Language. An ISO standard under preparation, addressing the semantics of high-quality composition in a manner independent of particular formatting systems or processes. DSSSL is intended as a complementary standard to SGML for the specification of semantics.")

("DTD" 
 "Document Type Definition: the definition of a document type in SGML, consisting of a set of markup tags and their interpretation.")

("DTI" 
 "UK Department of Trade and Industry.")

("DTIC" 
 "Defense Technical Information Center of the US Dept. of Defense.")

("DTL" 
 "DVI Text Language. An ASCII DVI format.")

("DTLS" 
 "Descriptive Top-Level Specification language: used in POSIX and TRUSIX.")

("DTP" 
 "Desktop publishing.")

("DTS" 
 "Distributed Time Service .")

("DVI" 
 "Device independent file format. A dvi file containing a description of the formatted document is the usual output of TeX .")

("Dylan" 
 "An object-oriented dynamic language.")

("DWARF" 
 "A debugging information format for UNIX System V")

("E" 
 "A database progamming language developed for the EXODUS project.")

("E-mail" 
 "See Electronic mail .")

("EAPLS" 
 "European Association for Programming Languages and Systems.")

("EARN" 
 "European Academic and Research Network. A self-managing network in the research community originally sponsored by IBM. It uses BITNET protocols and connects to BITNET in the US.")

("EAST" 
 "A Eureka project developing a software engineering platform.")

("EC" 
 "Electronic Commerce. Managing business transactions using networking and electronic means.")

("ECFA" 
 "European Committee for Future Accelerators. This body, whose principal role is to take care of Europe's requirements for future particle accelerators, has also looked at particle physics data handling on a European-wide basis.")

("ECHO" 
 "A public database service of the European Community.")

("ECHT" 
 "European Conference on Hypertext.")

("ECIP2" 
 "An Esprit Project on the definition of a specification language at the requirement level.")

("ECIS" 
 "European Committee for Interoperable Systems.")

("ECM" 
 "Enterprise Component Modelling.")

("ECMA" 
 "European Computer Manufacturers Association.")

("ECO" 
 "Engineering Change Order.")

("ECOOP" 
 "European Conference on Object-oriented Programming.")

("ECRC" 
 "Electronic Commerce Resource Centers. A network of US government sponsored centers that provide support to government and industry in developing and implementing strategies for business process improvement, implementing enabling technologies, and migrating to electronic commerce.")

("EDA" 
 "Product line from Dazix.")

("Eden" 
 "An object-oriented distributed operating system based on an RPC mechanism .")

("EDH" 
 "Electronic Document Handling \(at CERN\).")

("EDI" 
 "Electronic Data Interchange: a set of standards for exchanging orders and other business transactions by electronic mail.")

("EDIF" 
 "Electronic Design Interchange Format .")

("EDM" 
 "Engineering Data Management.")

("EDMS" 
 "Electronic Document Management System.")

("EDUCOM" 
 "A nonprofit consortium of US higher education institutions promoting access to and use of information resources and technology.")

("EEMA" 
 "European Electronic Messaging Association.")

("EER" 
 "An extended entity-relationship model .")

("EFF" 
 "Electronic Frontier Foundation. An organisation working on civil rights issues in networking.")

("EHTS" 
 "Emacs HyperText System: an experimental multiuser hypertext system from the University of Aalborg. It consists of a text editor \(based on Epoch and GNU Emacs and written in elisp\) and a graphical browser \(based on XView and written in C\) running under the X Window System and OpenWindows Both tools use HyperBase as database.")

("EIA" 
 "Electronic Industries Association.")

("Eiffel" 
 "An object-oriented programming language developed by B.Meyer et al. and commercialised by ISE.")

("Eiffel shelf" 
 "A set of user-contributed classes available with the Eiffel system.")

("EIS" 
 "Executive Information System.")

("EJO" 
 "Electronic Journals Online. A service of the OCLC.")

("ELOT" 
 "The Greek standards association.")

("Electronic Mail" 
 "A system allowing computer users to exchange messages via a network.")

("Ellemtel" 
 "A C++ style guide originated by Ellemtel Telecom Systems, Stockholm.")

("ELSA" 
 "Electronic Library Services and Applications. A library of reusable public domain software supported by NASA.")

("emacs" 
 "A popular editor and associated utilities for UNIX from the FSF")

("email" 
 "See Electronic mail .")

("EMDIR" 
 "The CERN Electronic Mail DIRectory utility.")

("Encapsulation" 
 "The ability to provide users with a well-defined interface to a set of functions in a way which hides their internal workings. In object-oriented programming, the technique of keeping together data structures and the methods \(procedures\) which act on them.")

("Entity-Relationship diagram" 
 "A type of diagram used in the Entity-Relationship model.")

("Entity-Relationship" 
 "An approach to data modelling proposed by P.Chen in 1976.")

("EOQ" 
 "European Organization for Quality.")

("EOUG" 
 "European ORACLE Users Group.")

("EPCS" 
 "Experimental Physics Control Systems: a group of the European Physical Society, focussing on all aspects of controls, especially informatics, in experimental physics, including accelerators and experiments.")

("EPIC" 
 "Electronic Privacy Information Center. A US center working on privacy issues relating to the National Information Infrastructure.")

("EPICS" 
 "Experimental Physics and Industrial Control System. Software for accelerator, experiment, and process control from ANL and LANL.")

("EPO" 
 "European Patent Office.")

("Epoch" 
 "A version of GNU Emacs for the X Window system from NCSA.")

("EPS" 
 "Encapsulated PostScript.")

("EQA" 
 "European Quality Award for process improvement.")

("ER" 
 "Entity-Relationship.")

("ERA" 
 "Entity-Relationship-Attribute.")

("ERC" 
 "An extended entity-relationship model .")

("ERCIM" 
 "European Research Consortium on Informatics and Mathematics. An association of European research organizations promoting cooperative research on key issues in information technology.")

("ERCS" 
 "Extended Reference Concrete Syntaxes for SGML, to support East Asian and other non-English languages.")

("ERD" 
 "Entity-relationship diagram.")

("ESA" 
 "European Space Agency on ESA software standards.")

("ESF" 
 "Eureka Software Factory.")

("ESI" 
 "European Software Institute. A network of organisations co-operating in strategic planning of process improvement.")

("ESIS" 
 "Element Structure Information Set produced by SGML parsers.")

("ESML" 
 "Extended Systems Modelling Language: a real-time software engineering methodology based on RTSA.")

("ESPIF" 
 "European Software Process Improvement Foundation.")

("Esprit" 
 "A funding programme to develop Informatics in the EEC..")

("Estelle" 
 "A formal description technique developed for OSI protocol specification.")

("ESUG" 
 "European Smalltalk Users' Group.")

("Ethernet" 
 "A 10-megabit/second local area network developed by Xerox and now widely adopted. Hosts are connected to a coaxial cable, and transmission conflicts are avoided by backing off and re-sending later. IEEE standard 802.3 defines the hardware and transport layers of the network.")

("ETLA" 
 "Extended Three Letter Acronym.")

("ETM" 
 "An active DBMS from the University of Karlsruhe.")

("ETSI" 
 "European Telecommunications Standards Institute.")

("EUnet" 
 "The European UNIX network: an Internet service provider. More information.")

("Eureka" 
 "A European technological development programme.")

("EuropaNET" 
 "A combination of pan-European backbone services run by DANTE.")

("EUSIDIC" 
 "European Association of Information Services.")

("EUUG" 
 "European UNIX User Group.")

("EWOS" 
 "European Workshop for Open Systems.")

("Excelerator" 
 "A set of CASE tools from Index Technology Corp.")

("eXodus" 
 "A package from White Pines allowing the Macintosh to be used as an X server.")

("EXODUS" 
 "An extensible database project developed at the University of Wisconsin.")

("Expert system" 
 "An intelligent computer program that contains a knowledge base, specialized software, and a set of algorithms or rules that infer new facts from knowledge and from incoming data.")

("Express" 
 "A data modelling language adopted by the ISO working group on STEP.")

("Extensible database" 
 "A DBMS that allows access to data from remote sources as if it were part of the database.")

("EXUG" 
 "European X User Group.")

("FATMEN" 
 "A distributed file and tape management system for HEP data.")

("FDDI" 
 "Fiber Distributed Data Interface: a new ANSI standard for a 100 megabits/second fibre optic token ring local area network")

("FEA" 
 "Finite Element Analysis.")

("Feature" 
 "An attribute or function of a class in Eiffel.")

("Feed-forward" 
 "A multilayer perceptron network in which the outputs from all neurons \(see McCulloch-Pitts\) go to following but not preceding layers, so there are no feedback loops.")

("FFT" 
 "Fast Fourier Transform")

("FIMS" 
 "Form Interface Management System.")

("FIPS" 
 "Federal Information Processing Standard: U.S. Government standards.")

("FITS" 
 "Flexible Image Transport System. The standard data interchange and archive format of the astronomy community.")

("FNAL" 
 "Fermi National Accelerator Laboratory \(Illinois, USA\).")

("Floppy" 
 "A Fortran coding convention checker. The latest version has a feature for generating HTML..")

("FOOM" 
 "Formal Object Oriented Method.")

("FOOT" 
 "Forum for Object Oriented Technology at CERN.")

("Foresight" 
 "A software product from Nu Thena providing graphical modelling tools for high level system design and simulation.")

("Formal methods" 
 "Several formal approaches to program specification have been developed, such as those based on VDM or Z. They can be used to develop software with high reliability, for safety-critical or high-volume applications.")

("FORML" 
 "Formal Object Role Modeling Language.")

("FORTH" 
 "Greek FOundation for Research and Technology.")

("FORTRAN" 
 "FORmula TRANslating system: a programming language widely used for many years in scientific applications.")

("Forward delta" 
 "The delta which, when combined with a version, creates a child version. See change management")

("Forward engineering" 
 "The traditional process of moving from high-level abstractions and logical, implementation-independent designs to the physical implementation of a system.")

("FORWISS" 
 "Bayerische Forschungszentrum fuer Wissensbasierte Systeme \(Bavarian research centre for knowledge-based systems\) in Passau \(in German\).")

("FOSI" 
 "Formatted Output Specification Instance template for SGML")

("FPA" 
 "Function Point Analysis.")

("FPM" 
 "Function Point Metric.")

("Fourth generation language" 
 "A high-level language, usually non-procedural, to allow users inexperienced in programming to develop database applications.")

("Framework" 
 "In object-oriented systems, a set of classes that embodies an abstract design for solutions to a number of related problems.")

("FrameMaker" 
 "Commercial publishing software available on a wide variety of workstations and addressing technical and scientific needs.")

("FreeHEP" 
 "An organisation offering a repository of software and related information for high energy physics applications.")

("Fresco" 
 "An object-oriented API for graphical user interfaces, under development by the X consortium as an open, multi-vendor standard.")

("Friend" 
 "Relationship between classes in the language C++.")

("FSF" 
 "Free Software Foundation \(675 Massachusetts Ave., Cambridge, MA 02139, USA\): dedicated to promoting the development and use of free software, especially the GNU system.")

("FSM" 
 "Finite State Machine.")

("FTAM" 
 "File Transfer, Access, and Management: an application layer protocol for file transfer and remote manipulation \(ISO 8571\).")

("FTP" 
 "File Transfer Protocol \(based on TCP/IP\). Also the name of a utility program available on several operating systems which makes use of this protocol to access and transfer files on remote computers.")

("FTR" 
 "Formal Technical Review. A software engineering technique.")

("Full-custom" 
 "A technique used for the design of integrated circuits that involves the manipulation of circuit designs at the semiconductor device level.")

("Function point" 
 "A unit for estimating the functionality of a program.")

("Functional language" 
 "A general purpose, high-level programming language based on the mathematical notion of functions. A functional program consists of a set of \(possibly recursive\) function definitions. Its execution consists of the evaluation of a function. Programs written in a functional language are generally compact and elegant, but tend to run slowly and consume a lot of memory.")

("Functional programming" 
 "See Functional language")

("FUSE" 
 "A DEC software development environment for ULTRIX, offering an integrated toolkit for developing, testing, debugging and maintainance.")

("Fusion" 
 "An object oriented analysis and design method developed by Hewlett Packard.")

("Futurebus+" 
 "A high performance bus system specified by IEEE Std.896.2")

("Fuzzy logic" 
 "An alternative to traditional logic where truth values range between 0.0 and 1.0, with 0.0 representing absolute Falseness and 1.0 representing absolute Truth.")

("FVWM" 
 "A window manager for the X Window System derived from twm.")

("FWEB" 
 "See Literate Programming")

("FWF" 
 "Free Widget Foundation.")

("G2" 
 "A real-time expert system from Gensym Corporation.")

("GAIA" 
 "GUI Application Interoperability Architecture project of OSF")

("GAMS" 
 "Guide to Available Mathematical Software at NIST.")

("GANDALF" 
 "A software development environment from Carnegie Mellon University.")

("Garbage collection" 
 "The process of reclaiming storage which is no longer in use.")

("Garnet" 
 "A user interface development environment for Common Lisp and X or Macintosh from Carnegie Mellon.")

("GBIP" 
 "General Purpose Interface Bus \(IEEE 488\).")

("GCA" 
 "Graphic Communications Association.")

("GCC" 
 "Gnu C Compiler.")

("GDB" 
 "Gnu DeBugger.")

("GDMO" 
 "Guidelines for the Definition of Managed Objects. A standard \(ISO/IEC 10165-4 / ITU-T Rec. X.722\) for defining data models on ASN.1")

("GEANT" 
 "A simulation, tracking and drawing package for HEP.")

("GEI" 
 "A German software engineering company.")

("GEN-X" 
 "An expert system developed by General Electric.")

("Generic Markup" 
 "In computerised document preparation, a method of adding information to the text indicating the logical components of a document, such as paragraphs, headers or footnotes: SGML is an example of such a system. Specific instructions for layout of the text on the page do not appear in the markup.")

("Genericity" 
 "The possibility for a language to provided parameterized modules or types. e.g. List\(of:Integer\) or List\(of:People\).")

("Genesia" 
 "An expert system developed by Electricite de France and commercialised by STERIA \(Paris\).")

("GEOS" 
 "An object-oriented operating system project.")

("ghostscript" 
 "The gnu PostScript interpreter.")

("ghostview" 
 "An X window interface to the ghostscript interpreter.")

("GIF" 
 "Graphics Interchange Format: a standard for digitised images compressed with the LZW algorithm.")

("GILS" 
 "Government Information Locator Service. A plan for a decentralised collection of information locators and associated public services to find information throughout the US government.")

("GINA" 
 "Generic INteractive Application. A toolkit of useful classes and functions for authoring GUIs built on CLM, CLX and CLOS, from GMD")

("GKS-3D" 
 "The three-dimensional version of GKS, a standard for graphics I/O \(ISO 8805\).")

("GKS" 
 "Graphical Kernel System: a standard for graphics I/O \(ANSI X3.124\).")

("GL" 
 "A graphics package from Silicon Graphics.")

("GLUT" 
 "OpenGL Utility Toolkit.")

("GMD" 
 "Gesellschaft fuer Mathematik und Datenverarbeitung \(German Institute for Mathematics and Data Processing\), D-53754 Sankt Augustin.")

("GNAT" 
 "The GNU NYU Ada 95 compiler.")

("GNU" 
 "GNU 's Not UNIX: a popular range of portable software from FSF, upwardly compatible with UNIX.")

("GOOD" 
 "An object-oriented framework for graphical applications from TU Ilmenau running under X Windows with special support to IRIS GL, OpenGL, VOGL, etc..")

("Gopher" 
 "A Campus Wide Information System designed at the University of Minnesota.")

("GPIB" 
 "General Purpose Interface Bus: an 8-bit parallel bus \(IEEE 488\).")

("GPM" 
 "General Purpose Macrogenerator written by C. Strachey around 1965. The author said \"It contains in itself all the undesirable features of every possible machine code... It can also be almost impenetrably opaque\".")

("GQM" 
 "Goal/Question/Metrics. A software engineering assessment method by V. Basili.")

("Grapevine" 
 "A distributed system project .")

("Grammar" 
 "A grammar is a mathematical system for defining a language, as well as a device for giving the sentences in the language a useful structure.")

("GRAS" 
 "A public domain graph-oriented database system for software engineering applications from RWTH Aachen")

("GRASPIN" 
 "An Esprit project to develop a personal software engineering environment to support the construction and verification of distributed and non-sequential software systems.")

("Grasshopper" 
 "An experimental operating system for persistent systems.")

("GRIB" 
 "GRid In Binary. World Meteorological Organization data format.")

("Groupware" 
 "see CSCW.")

("GROW" 
 "GNU Remote Operations Web. An architecture for building networked applications and services using WWW.")

("GUI" 
 "Graphical User Interface.")

("Guide" 
 "A hypertext system from the University of Kent \(GB\) and OWL for displaying online documentation .")

("GUIDE" 
 "Graphical User Interface Development Environment from Sun.")

("GUILE" 
 "An interpreter for the GROW project.")

("gunzip" 
 "The decompression utility corresponding to gzip .")

("gzip" 
 "A compression utility available with the gnu software.")

("h" 
 "A simple markup language intended for quick conversion of existing text to hypertext.")

("Hardware description language" 
 "A language used for the conceptual design of integrated circuits. Examples are VHDL and Verilog.")

("Harmony" 
 "A real-time operating system developed by the SEL in Canada.")

("Harvest" 
 "An information discovery and access system for the Internet from the University of Colorado.")

("Haskell" 
 "A functional language \(Hudak et al.\).")

("HBOOK" 
 "A histogramming package in the CERN program library.")

("hc" 
 "The compiler for the h hyperbook language.")

("HCI" 
 "Human Computer Interface \(or Interaction\).")

("HCS" 
 "Heterogeneous Computer System: a distributed system project .")

("HDF" 
 "Hierarchical Data Format from NCSA.")

("HDL" 
 "Hardware description language.")

("HDTV" 
 "High Definition Television.")

("Hebbian" 
 "Refers to the most common way for a neural network to learn, namely supervised learning. Using a training sample which should produce known responses, the connection weights are adjusted so as to minimize the differences between the desired and actual outputs for the training sample.")

("Helix" 
 "A hardware description language from Silvar-Lisco.")

("HEP" 
 "High Energy \(Particle\) Physics.")

("HEPDB" 
 "A database management system for HEP.")

("HEPiX" "A recently formed collaboration among various HEP institutes aiming at providing \"compatible\" 
 versions of the UNIX operating system at their sites.")

("HEPnet" 
 "An association concerned with networking requirements for high energy physicists.")

("HEPVM" "A collaboration among various HEP institutes to implement \"compatible\" 
 versions of IBM's VM-CMS operating system at their sites.")

("HERA" 
 "An electron-proton collider at DESY, W. Germany.")

("Hermes" 
 "An experimental object-oriented distributed systems language from IBM Watson Research Centre.")

("Hesiod" 
 "The name server of the Athena project.")

("Heuristic" 
 "A rule of thumb, simplification or educated guess that reduces or limits the search for solutions in domains that are difficult and poorly understood. Unlike algorithms, heuristics do not guarantee solutions.")

("Hewlett-Packard*" 
 "A manufacturer of workstations, electronic instrumentation and test equipment etc.")

("HIGZ" 
 "High Level Interface to Graphics and Zebra. Part of the PAW system.")

("HiPAC" 
 "An active DBMS from Xerox Advanced Information Technology.")

("HIPPI" 
 "HIgh Performance Parallel Interface: a 100 Mbyte/sec data transfer system with associated interfaces and switches, developed at Los Alamos National Lab and now ANSI standard X3T9/88-127.")

("HISTORIAN" 
 "A source code management system sold by OPCODE, Inc..")

("History" 
 "For more information on the history of computing, see the The Virtual Museum of Computing")

("HOL" 
 "An interactive theorem proving system based on Higher Order Logic.")

("Home Page" "The starting point for a WWW session. Many system adminstrators set up \"home pages\" 
 which are the default page shown when a user begins a session. These pages usually have a lot of options and menu items that apply to that particular institution and then have links to other places. Here is the CERN home page.")

("HOOD" 
 "Hierarchical Object Oriented Design: a method for Architectural Design primarily for software to be developed in Ada, leading to automated checking, documentation and source code generation.")

("Hope" 
 "A functional language \(Burstall et al. 1980\).")

("Hopfield" 
 "John Hopfield in the early 1980's investigated a particular kind of neural network which is now commonly referred to as the Hopfield network or Hopfield model. In the Hopfield network, there are no special input or output neurons \(see McCulloch-Pitts\), but all are both input and output, and all are connected to all others in both directions \(with equal weights in the two directions\). Input is applied simultaneously to all neurons which then output to each other and the process continues until a stable state is reached, which represents the network output.")

("HotJava" 
 "A WWW browser from Sun based on the Java language.")

("HP-UX" 
 "The version of UNIX running on Hewlett-Packard workstations.")

("HP VEE" 
 "Visual Engineering Environment from Hewlett-Packard: a package similar in intention to LabVIEW running on UNIX workstations with OSF/Motif.")

("HP" 
 "Hewlett-Packard.")

("HPLOT" 
 "A graphical output facility for HBOOK.")

("HPPI" 
 "An earlier name for HIPPI.")

("HTML" 
 "HyperText Markup Language. An SGML document type used to mark up hypertext in the WWW project.")

("HTTP" 
 "HyperText Transfer Protocol. The protocol used between client and server in the WWW project.")

("Hyper-G" 
 "A hypertext system from TU Graz.")

("Hyper-Man" 
 "A browser available with Epoch giving hypertext capability for the UNIX manual.")

("HyperBase" 
 "An experimental active multiuser database for hypertext systems from the University of Aalborg, written in C++.It is built on the client-server model enabling distributed, concurrent, and shared access from workstations in a local area network. See EHTS.")

("Hyperbole" 
 "An information management and hypertext system.")

("Hypercard" 
 "A software package for the Macintosh for storage and retrieval of information. It can handle images, and is designed for browsing. The powerful customisable interactive user interface allows new applications to be easily constructed by manipulating objects on the screen, often without conventional programming.")

("Hypermedia" 
 "Hypertext systems where the nodes can contain text, graphics, audio, video, as well as source code or other forms of data.")

("HyperNeWS" 
 "A Hypertext system from the Turing Institute Glasgow, based on NeWS.")

("HyperODA" 
 "ODA extensions for hypermedia.")

("Hypertalk" 
 "The language for writing procedures associated with objects in Hypercard.")

("Hypertext" 
 "An approach to information management in which text is stored in a network of nodes connected by links. The nodes are meant to be viewed through an interactive browser. A link is something which connects a piece of text to a destination piece of text; the source and destination areas are usually marked on a display by highlighting or special graphics. You are reading hypertext now by courtesy of WWW.")

("HyTime" 
 "Hypermedia/Time-based Structuring Language: an ANSI/ISO Standard \(ISO/IEC 10744\) from the SGML Users' Group's Special Interest Group on Hypertext and Multimedia \(SIGhyper\).")

("I-CASE" 
 "Integrated CASE: another term for an IPSE.")

("IAB" 
 "The Internet Architecture Board of the Internet Society.")

("IAD" 
 "A dynamic analyser from IBM giving information on run time performance and code utilisation.")

("IAFA" 
 "Internet Anonymous FTP Archives. An IETF working group.")

("IANA" 
 "Internet Assigned Numbers Authority.")

("IBM" 
 "International Business Machines.")

("IBN" 
 "The Belgian standards institute.")

("ICADD" 
 "International Committee for Accessible Document Design. Dedicated to making printed materials accessible to persons with print disabilities. Works on the generation of Braille, large print or electronically navigable editions of books from desktop publishing files.")

("ICCP" 
 "Institute for Certification of Computing Professionals.")

("ICSI" 
 "International Computer Science Isntitute at Berkeley, CA..")

("IDE" 
 "Interactive Development Environments: a US Software Engineering Company.")

("IDEA" 
 "International Data Encryption Algorithm \(used by PGP\).")

("IDL" 
 "Interactive Data Language. A package for interactive reduction, analysis, and visualization of scientific data, from Research Systems, Inc..")

("IDL" 
 "Interface Definition Language: an OSF standard for defining RPC stubs.")

("IDL" 
 "Interface Definition Language: associated with the CORBA standard.")

("IDSS" 
 "Intelligent Decision Support Systems.")

("IEC" 
 "International Electrotechnical Commission: a standardisation body at the same level as ISO.")

("IEF" 
 "Information Engineering Facility. A CASE tool from Texas Instruments which generates code from graphical business process models.")

("IEEE 1076" 
 "The IEEE standard for VHDL..")

("IEEE 488" 
 "The IEEE standard for GPIB.")

("IEEE 802" 
 "The IEEE standards for local area networks \(LANs\). The Ethernet standard is 802.3, the IBM Token Ring is IEEE 802.5.")

("IEEE" 
 "Institute of Electrical and Electronics Engineers \(USA\).")

("IESG" 
 "Internet Engineering Steering Group. Part of the Internet Society responsible for technical management of IETF activities and the Internet Standards process.")

("IETF" 
 "Internet Engineering Task Force. A group of people who make technical and other contributions to the engineering and evolution of the Internet and its technologies. It is the principal body engaged in the development of new Internet Standard specifications.")

("IETM" 
 "Interactive Electronic Technical Manual.")

("IFAC" 
 "International Federation of Automatic Control, involved in informatics related to control systems.")

("IFDL" 
 "Independent Form Description Language: DEC's language for describing form-based human interfaces in DECforms.")

("IFIP" 
 "International Federation of Information Processing.")

("IFPUG" 
 "International Function-point Users Group.")

("IGES" 
 "Initial Graphics Exchange Specification: an ASME/ANSI standard for the exchange of CAD data.")

("IIDMS/R" 
 "Integrated database management system: a DBMS from Cullinet Software Inc.")

("IIIS" 
 "International Institute of Informatics and Systemics.")

("ILU" 
 "Inter-Language Unification. A system from Xerox PARC that promotes software interoperability via interfaces.")

("Immediate version" 
 "See Child version.")

("IMS" 
 "Information Management System: a DBMS from IBM.")

("IMSE" 
 "Integrated Modelling Support Environment: an Esprit programme.")

("INCOSE" 
 "International Council on Systems Engineering. An international organization formed to develop, nurture and enhance the system engineering approach to multi-disciplinary system product development.")

("Inference" 
 "The logical process by which new facts are derived from known facts.")

("Inference engine" 
 "A program that infers facts from a set of knowledge or inputs.")

("INFN" 
 "Istituto Nazionale di Fisica Nucleare: an Italian State research organisation.")

("Informix" 
 "A relational DBMS vendor.")

("INGRES" 
 "A relational DBMS vendor.")

("Inheritance" 
 "In object-oriented programming, the ability to derive new classes from existing classes. A derived class inherits the instance variables and methods of the base class, and may add new instance variables and methods. A new method may be defined with the same names as one in the base class, in which case it overrides the original one.")

("INRIA" 
 "Institut National de Recherche en Informatique et Automatique, French computer science research institute.")

("Instantiation" 
 "A more precisely defined version of some object which was already partially defined. In object-oriented programming, a particular example of an object produced from its class template.")

("InterBase" 
 "A commercial active DBMS.")

("Interface Architect" 
 "An interface builder for Motif distributed by Hewlett-Packard \(see UIMX\).")

("Interleaf" 
 "A document preparation system available on the Sun, VAX, Apollo and other workstations.")

("INTERLINK" 
 "A commercial product comprising hardware and software for file transfer between IBM and VAX computers.")

("Intermedia Interchange Format" 
 "A Standard Hypertext Interchange format from IRIS.")

("Intermedia" 
 "A hypertext system developed by a research group at IRIS \(Brown University\).")

("Intermetrics" 
 "A software engineering company .")

("Internet Address" 
 "A thirty-two-bit number that uniquely identifies an Internet host. It is usually represented as four 8-bit numbers separated by dots e.g. 128.121.4.5. It consists of a network number and a host number, and can be subdivided in several ways.")

("Internet" 
 "A loosely-organized international collaboration of autonomous, interconnected networks, supporting host-to-host communication through voluntary adherence to open protocols and procedures defined by Internet Standards, typically based on the TCP/IP protocol suite.")

("Interpress" 
 "A page description language from Xerox.")

("InterViews" 
 "An object-oriented toolkit developed at Stanford University for building graphical user interfaces. It is implemented in C++ and provides a library of objects and a set of protocols for composing them.")

("Intrinsics" 
 "A library package on top of Xlib, extending the basic functions of the X Window System. It provides mechanisms for building widget sets and application environments..")

("Inventor" 
 "See Open Inventor.")

("Inverse engineering" 
 "The process of extracting high-level abstract specifications from source code using program transformations.")

("ION" 
 "Implementation-Oriented Notation. A notation designed to graphically document object-oriented programs.")

("IP address" 
 "An Internet address.")

("IP" 
 "Internet transport layer Protocol.")

("IPC" 
 "Inter-Process Communication.")

("IPE" 
 "Integrated Programming Environment.")

("IPF" 
 "Information Presentation Facility. A document markup system for OS/2 based on SGML.")

("IPSE" 
 "Integrated Project Support Environment: a term for a set of management and technical tools to support software development, usually integrated in a coherent framework: equivalent to an SEE.")

("IPTES" 
 "Incremental Prototyping Technology for Embedded Realtime Systems, an Esprit project.")

("IPVR" 
 "Institute of Parallel and Distributed High-Performance Systems \(Stuttgart\).")

("IQA" 
 "Institute of Quality Assurance \(UK\).")

("IRC" 
 "Internet Relay Chat. A system whereby a number of people can participate in a discussion in real time on the Internet.")

("IRD" 
 "Internet Resource Discovery.")

("IRDS" 
 "Information Resource Dictionary System. A set of ISO standards for CASE repositories. It governs the definition of data dictionaries to be implemented on top of relational databases \(see repository, data dictionary\).")

("Iris" 
 "An object-oriented DBMS.")

("IRIS" 
 "Institute for Research in Information and Scholarship of Brown University \(Providence RI\).")

("IRIS" 
 "See IRIS Explorer")

("IRIS Explorer" 
 "A visualisation system.")

("ISA" 
 "An Esprit project continuing the ANSA project.")

("ISA" 
 "International Smalltalk Association \(now disbanded\).")

("ISAM" 
 "Indexed Sequential Access Method: a file access method supporting both sequential and indexed access.")

("ISBN" 
 "International Standard Book Numbering.")

("ISCN" 
 "International Software Consulting Network. A network of process improvement experts.")

("ISDE" 
 "Integrated Software Development Environment: equivalent to an IPSE.")

("ISDN" 
 "Integrated Services Digital Network: a set of CCITT standards to support many types of signal traffic \(speech, data, video\) via a digital transmission system, eventually intended to replace current telephone systems. The Basic rate is 64 kbits/sec.")

("ISE" 
 "Interactive Software Engineering: a software engineering company marketing Eiffel among other products.")

("ISEE" 
 "Integrated Software Engineering Environment: equivalent to SEE.")

("ISERN" 
 "International Software Engineering Research Network.")

("ISF" 
 "Information Systems Factory: equivalent to an SEE.")

("ISIS" 
 "A toolkit for implementing fault-tolerant distributed systems, developed at Cornell and now available commercially")

("ISO" 
 "International Organisation for Standardisation.")

("ISOC" 
 "The Internet Society. A professional society concerned with the growth and evolution of the Internet, with the way it is used, and with related social, political, and technical issues.")

("ISODE" 
 "ISO Development Environment: software that implements a set of OSI upper-layer services. It supports OSI applications on top of OSI and TCP/IP networks.")

("ISPE" 
 "International Society for Productivity Enhancement.")

("ISTAR" 
 "An experimental IPSE. from Imperial Software Technology.")

("ISV" 
 "Independent Software Vendor \(not a hardware manufacturer\).")

("IT" 
 "Information Technology.")

("ITHACA" "An Esprit project to put a \"4th generation\" 
 object-oriented system to practical use in an industrial environment. The ITHACA environment offers an application support system incorporating advanced technologies in the fields of object-oriented programming, programming languages, database technologies, user interface systems and software development tools.")

("ITU" 
 "International Telecommunications Union.")

("Jackson method" 
 "A proprietary structured method for software analysis, design and programming.")

("JANET" 
 "The Joint Academic NETwork which links U.K. academic and research institutes.")

("Java" 
 "An Object-Oriented language from Sun, now widely used in WWW browsers.")

("JAZELLE" 
 "A data management system for HEP from SLAC.")

("JEDI" 
 "Joint Electronic Document Interchange.")

("JEPI" 
 "Joint Electronic Payment Initiative. A joint project between W3C and CommerceNet in the field of electronic payment using WWW.")

("JFIF" 
 "A data stream-oriented file format used for transmitting JPEG encoded bitmap data.")

("JOOP" 
 "Journal of Object-Oriented Programming.")

("JPEG" 
 "A standardized image compression mechanism. JPEG stands for Joint Photographic Experts Group, the original name of the committee that wrote the standard. JPEG is designed for compressing either full-color or gray-scale digital images of \"natural\", real-world scenes. It does not work so well on non-realistic images, such as cartoons or line drawings. JPEG does not handle black-and-white \(1-bit-per-pixel\) images, or motion picture compression. Standards for compressing those types of images are being worked on by other committees, named JBIG and MPEG.")

("jpg" 
 "See JPEG.")

("JSA" 
 "Japanese Standards Association")

("JSD" 
 "Jackson System Development.")

("JTC" 
 "Joint Technical Committee \(of ISO and IEC\).")

("Kala" 
 "A persistent data server: a link library providing an engine for applications needing persistence, transactions, crash recovery and rollback, versioning, distribution, and other facilities for which DBMSs are commonly used.")

("KAPPA" 
 "An object-oriented workbench for Sun workstations from Intellicorp.")

("KBS" 
 "Knowledge-based system.")

("KDD" 
 "Knowledge Discovery in Databases. A branch of Artificial Intelligence.")

("Kerberos" 
 "An authentication system from the Athena project, adopted by OSF as the basis of security for DME.")

("KERMIT" 
 "A protocol for file transfer. Mainly used for transfers to and from PC's.")

("kernel" 
 "The essential part of UNIX or other operating systems, responsible for resource allocation etc.")

("Khoros" 
 "A visualisation system from Khoral Research.")

("KIF" 
 "Knowledge Interchange Format. For knowledge sharing and communication among heterogeneous agents.")

("KISS" 
 "Keep It Simple Stupid. A homespun design philosohpy.")

("KISS" 
 "An Object-Oriented analysis and design approach.")

("KMS" 
 "Knowledge Management System: a distributed hypermedia system for managing knowledge in organisations A commercial system from Knowledge Systems Inc running on workstations, based on previous research with ZOG at Carnegie Mellon University.")

("Knowledge Engineering" 
 "The acquisition of knowledge from a human expert or similar source and its coding in an expert system.")

("Knowledge Representation" 
 "A subset of AI .")

("Kohonen" 
 "T. Kohonen of the University of Helsinki has been studying neural networks for many years with the idea of modelling as closely as possible the behaviour of biological systems, and his name is commonly associated with a particular kind of neural network in which there are only two kinds of neurons \(see McCulloch-Pitts\), input and others. All the input neurons are connected to all others, and the others are connected only to their other nearest neighbors. The training algorithm is a relatively simple one based on the geometric layout of the neurons, and makes use of annealing.")

("KQML" 
 "Knowledge Query and Manipulation Language.")

("KR" 
 "Knowledge Representation.")

("KUIP" 
 "Kernel User Interface Package: the human interface to PAW.")

("Labview" 
 "A package from National Instruments Corp originally developed to provide a graphical interface to instruments connected by the IEEE 488 \(GPIB\) bus. It has powerful graphical editing facilities for defining and interconnecting \"virtual instruments\".")

("LAMPF" 
 "Los Alamos Meson Physics Facility \(An 800 MeV proton and negative H ion high-current LINAC, 1mA average, 12mA peak\).")

("LAN" 
 "Local area network")

("Language-Based Editor" 
 "An editor that is aware of the syntactic, semantic and in some cases the structural rules of a specific programming language and provides a framework for the user to enter source code. Programs or changes to previously stored programs are incrementally parsed into an abstract syntax tree and automatically checked for correctness.")

("LANL" 
 "Los Alamos National Laboratory - Los Alamos, NM, USA.")

("LaTeX" 
 "A document preparation system based on TeX, popular in the HEP community. It adds a collection of commands to simplify typesetting, and lets the user concentrate on the structure of the text rather than on formatting commands.")

("LBE" 
 "Language-Based Editor.")

("LBL" 
 "Lawrence Berkeley Laboratory, Berkeley, CA, USA.")

("LCF" 
 "Logic for Computable Functions. A system for interactive automated reasoning.")

("LEAR" 
 "Low Energy Antiproton Ring.")

("LEDA" 
 "Library of Efficient Data types and Algorithms. A class library for C++ with graph classes from Uni Saarbruecken.")

("Legacy" 
 "Legacy system is a term used to describe old software systems still in use but which could benefit from re-engineering using more modern methods.")

("LEP" 
 "Large Electron Positron Collider. A 27km circumference accelerator at CERN, which brings bunches of electrons and positrons into collision.")

("lex" 
 "A lexical analysis tool for the UNIX environment.")

("LHC" 
 "Large Hadron Collider: proposed to be built in the LEP tunnel at CERN.")

("Life-Cycle" 
 "The software life-cycle consists of phases: requirements analysis, design, construction, testing and maintenance. The development process tends to run iteratively through these phases rather than linearly; several models \(spiral, waterfall etc\) have been proposed to describe this process.")

("Lifecycle" 
 "See Life-Cycle .")

("LIFIA" 
 "Laboratoire d'Informatique Fondamentale et d'Intelligence Artificielle.")

("LIFN" 
 "Location Independent File Name")

("LIGHT" 
 "LIfecycle Global HyperText. A project in the CERN ECP/IPT group whereby documents resulting from the software life cycle are available as hypertext.")

("Linda" 
 "A portable parallel language to simplify parallel programming. Extensions to C and Fortran, available from Scientific Computing Associates, Inc..")

("link" 
 "see Hypertext")

("lint" 
 "A C language preprocessor which carries out more thorough checks on the code than is usual with C compilers themselves.")

("Linux" 
 "An implementation of UNIX written from scratch with no proprietary code for IBM PC compatibles by Linus Torvalds and distributed under the GNU public licence.")

("LISP" 
 "A List Processing Language suitable for symbolic and logical programming.")

("LispView" 
 "CLOS based windowing system on OpenWindows.")

("Literate programming" 
 "Combining the use of a language such as TeX and a conventional programming language, so as to maintain documentation and source together.")

("LitProg" 
 "Literate Programming")

("Lml" 
 "A functional language \(Johnson 1984\).")

("LOC" 
 "Line of code. Used as a simple software metric.")

("Local Area Network" 
 "Usually abbreviated to LAN: a communications network which is geographically limited \(typically to a 1 km. radius\) allowing easy interconnection of terminals, microprocessors and computers within adjacent buildings. Ethernet and FDDI are examples of standard LANs.")

("Locus" 
 "A distributed system project supporting transparent access to data through a network-wide file system.")

("Logic Programming" 
 "Programming in a language such as Prolog, which allows the programmer to make a series of assertions which are interpreted by an inference engine.")

("LOGISCOPE" 
 "Software quality analysis tools from Verilog SA, used to evaluate the quality of software both statically \(based on software metrics\) and dynamically.")

("Lojban" 
 "An artificial language designed to be used by people in communication with each other, and possibly in the future with computers.")

("Looking Glass" 
 "A desktop manager for UNIX from Visix.")

("LOOPS" 
 "Lisp Object-oriented Programming System from Intelligent Systems Laboratory, Xerox Palo Alto Research Center.")

("LOTOS" 
 "A formal description technique used for protocol specfication in ISO OSI standards \(ISO 8807\).")

("LSE" 
 "Language Sensitive Editor: from DEC .")

("Lynx"
" A WWW browser from University of Kansas. \(See also LynxOS\).")

("LynxOS" 
 "A POSIX compliant real-time operating system from Lynx Real-Time Systems, Los Gatos, California, with a UNIX-like interface to application programs.")

("LZW" 
 "Lempel-Ziv-Welch data compression algorithm.")

("MACAnalyst" 
 "An analysis CASE tool for the Mac from Excel Software Inc.")

("MACDesigner" 
 "A design CASE tool for the Mac from Excel Software Inc.")

("Mach" 
 "An operating system kernel under development at Carnegie-Mellon University to support distributed and parallel computation. Mach is designed to support computing environments consisting of networks of uniprocessors and multiprocessors. Mach is the kernel of the OSF/1 system. .")

("Macintosh" 
 "A range of personal computers manufactured by Apple Computer Inc.")

("MacX" 
 "A package allowing the Macintosh to be used as an X server.")

("Madaline" 
 "A structure of many ADALINE units.")

("Maintenance" 
 "An important part of the software life-cycle. Maintenance is expensive in manpower and resources, and software engineering techniques aim to reduce its cost.")

("Make" 
 "A popular tool on UNIX systems to automate the recompilation, linking etc. of programs, taking account of the interdependencies of modules.")

("Makedoc" 
 "A program from Carleton University, Ottawa that generates documentation for Objective C programs. It will also generate a class hierarchy diagram. The output format is similar to that used by StepStone.")

("MAP" 
 "Manufacturers Automation Protocol, a set of protocols developed by General Motors based on Token Bus \(IEEE 802.4\) and giving predictable response in real time.")

("Maple" 
 "A mathematics package developed by the University of Waterloo and ETH Zurich.")

("MARC" 
 "MAchine Readable Cataloging: a record format for bibliographic information interchange based on the ANSI / NISO Z39.2 standard.")

("Markowitz" 
 "The author of the original Simscript language.")

("Markup" 
 "In computerised document preparation, a method of adding information to the text indicating the logical components of a document, or instructions for layout of the text on the page.")

("MASCOT" 
 "Modular Approach to Software Construction Operation and Test: a method for software design aimed at real-time embedded systems from the Royal Signals and Research Establishment, UK.")

("Mathematica" 
 "A general program for symbolic computing and programming from Wolfram Research.")

("MBONE" 
 "Multicast backbone: a virtual network on top of the Internet to support routing of IP multicast packets, intended for multimedia transmission.")

("McCulloch-Pitts" 
 "The McCulloch-Pitts neuron is the basic building block of neural networks. It receives one or more inputs and produces one or more identical outputs, each of which is a simple non-linear function of the sum of the inputs to the neuron. The non-linear function is typically a threshhold or step function which is usually smoothed \(i.e. a sigmoid\) to facilitate learning.")

("MCS" 
 "Meta Class System: a portable object-oriented extension of Common Lisp from GMD. It integrates the functionality of CLOS and TELOS.")

("MDL" 
 "An early object-oriented language from MIT .")

("Mellor" 
 "see Schlaer-Mellor.")

("Member Function" 
 "In C++, the name given to a method.")

("MERISE" 
 "Methode d'Etude et de Realisation Informatique pour les Systemes d'Enteprise: a Software Engineering method popular in France; many IPSE s are based on it.")

("Mesa" 
 "An early object-oriented programming language developed at the Xerox Palo Alto research centre.")

("Message" 
 "In object-oriented programming sending a message to an object \(to invoke a method\) is equivalent to calling a procedure in traditional programming languages, except that the actual code executed may only be selected at run-time depending on the class of the object. Thus, in response to the message \"drawSelf\", the method code invoked would be different if the target object were a circle or a square.")

("Meta-CASE tool" 
 "A term sometimes used for software packages \(like TBK or VSF\) which allow users to develop or customise their own CASE tools.")

("MetaCard" 
 "A commercial human interface and hypertext system for UNIX and X-windows, similar to Hypercard.")

("Metaclass" 
 "The class of a class. A metaclass is a class whose instances are themselves classes.")

("Metadata" 
 "Data definitions describing aspects of the actual data items, such as name, format etc.")

("Metafile" 
 "Typically a file of graphics data for transport between different machines.")

("Method" 
 "The name given in Smalltalk \(and sometimes in other object-oriented languages\) to a procedure or routine associated with an object.")

("Methodology" 
 "A term for a codified set of procedures for some phase of software engineering, such as analysis and design.")

("Metric" 
 "see Software Metrics.")

("Meyer" 
 "Bertrand Meyer, the author of the Eiffel Language and many articles on object-oriented software techniques.")

("Microkernel" 
 "An approach to operating systems design which puts emphasis on small modules which implement the basic features of the system and can be flexibly configured.")

("Microsoft" 
 "A vendor of systems and application software for personal computers and similar platforms.")

("MID" 
 "Metafile for Interactive Documents. A standard sponsored by the DoD.")

("Midas-WWW" 
 "A Motif-based browser for WWW.")

("Midas" 
 "A Motif-based toolkit for interactive data analysis by T.Johnson, SLAC. The basis for the Midas-WWW browser.")

("MIDI" 
 "Musical Instrument Digital Interface.")

("MIFF" 
 "Machine Independent File Format. A bitmap format.")

("MIMD" 
 "Multiple Instruction Multiple Data: a form of parallelism in multiprocessor computing where there are several instruction streams \(programs\) operating concurrently on several data streams.")

("MIME" 
 "Multimedia Internet Mail Extensions. A method of processing multi-part, multimedia messages on the Internet . \(RFC 1521-1522 etc.\).")

("MINUIT" 
 "A Program for Function Minimization and Error Analysis.")

("MIPS" 
 "A microprocessor vendor .")

("MIS" 
 "Management Information Systems.")

("MIT" 
 "Massachusetts Institute of Technology.")

("ML" 
 "A functional language.")

("MMM" 
 "A WWW browser from INRIA based on the Caml language.")

("MODSIM" 
 "A general-purpose modular block-structured language from CACI, which provides support for object-oriented programming and discrete event simulation. It is intended for building large process-based discrete event simulation models through modular and object-oriented mechanisms similar to those of Modula-2.")

("Modula-2" 
 "A high-level programming language designed by N.Wirth. It is a derivative of Pascal with well-defined interfaces between modules, and facilities for parallel computation.")

("Modula-3" 
 "A member of the Pascal family of languages. Designed in the late 1980s at Digital Equipment Corporation and Olivetti, it aims to correct deficiencies of Pascal and Modula-2.")

("Montage" 
 "An object-relational database management system from Montage Software: the commercialisation of POSTGRES")

("MOOD" 
 "Material's Object-Oriented Database. An object oriented database system from Tohoku University.")

("MOOSE" 
 "An object-oriented R&D project at CERN.")

("Mosaic" 
 "An X-Window based browser for WWW from NCSA.")

("MOSES" 
 "Methodology for Object-oriented Software Engineering of Systems.")

("Motif" 
 "The standard Graphical User Interface and window manager from OSF, running on theX Window System.")

("MPEG" 
 "Moving Pictures Experts Group of ISO that generates standards for digital video \(sequences of images in time\) and audio compression.")

("MPV" 
 "Extension of the VRTX real-time operating system to support multi-processing.")

("MS-DOS" 
 "An operating system developed by MicroSoft Corporation for computers using the Intel 16 and 32-bit family of processors.")

("MTBF" 
 "Mean Time Between Faults")

("Multibus" 
 "A bus standard for microprocessor-based systems, specified by IEEE Std.796")

("Multi-media" 
 "See Multimedia.")

("Multimedia" 
 "Human computer interaction involving text, graphics, voice, video etc.")

("Multiple Inheritance" 
 "In object-oriented programming, the possibility that a sub-class may be derived from multiple parents which are themselves not derived one from the other.")

("Muse" 
 "An electronic journal project at Johns Hopkins.")

("MVC" 
 "Model View Controller architecture for interactive software.")

("MVE" 
 "Modular Visualisation Environment. A type of application builder for scientific and other visualisation systems \(such as AVS, IBM Data Explorer, IRIS Explorer, Khoros\).")

("NAG" 
 "Numerical Algorithms Group.")

("NAPLPS" 
 "North American Presentation Layer Protocol Syntax.")

("NAS" 
 "Network Application Support: DEC's approach to applications integration across a distributed multivendor environment.")

("NASA" 
 "National Aeronautics and Space Administration \(USA\). NASA has many software engineering projects.")

("NBS" 
 "National Bureau of Standards: part of the U.S. Department of Commerce, now NIST.")

("NCOSE" 
 "National Council On Systems Engineering \(USA\).")

("NCS" 
 "Network Computing System: Apollo's RPC system used by DEC and Hewlett-Packard.The protocol has been adopted by OSF.")

("NCSA" 
 "National Center for Supercomputing Applications, Urbana, IL, USA.")

("NCSS" 
 "Non-Commented Source Statements. Used as a simple software metric.")

("NDL" 
 "National Database Language: a US standard for portability of database definitions and application programs.")

("Neptune" 
 "A hypertext system for computer assisted software engineering, developed at Tektronix.")

("netCDF" 
 "Network Common Data Form. A machine-independent, self-describing file format for scientific data.")

("NetClasses" 
 "A C++ class library for object transport and remote method invocation from Stanford.")

("NeuDL" 
 "Neural network Description Language from the University of Alabama.")

("Neural net" 
 "See Neural network")

("Neural network" 
 "A computing device which converts one or more input signals to one or more output signals by means of an interconnected set of elementary non-linear signal processors called neurons. Animal brains are examples of biological neural networks. Artificial Neural Networks are man-made computing devices modelled after their biological counterparts. The features which distinguish artificial neural networks from traditional Von Neumann \(sequential\) computers are: \(a\) the elementary processors are highly non-linear \(in the limit, they are simple threshold discriminators\), \(b\) the neurons are highly interconnencted which allows a high degree of parallelism and \(c\) there is no idle memory containing data and programs, but rather each neuron is pre-programmed and continuously active.")

("Neural" 
 "See Neural network")

("Neuron" 
 "See Neural network , also McCulloch-Pitts")

("NeWS" 
 "Network extensible Window System from Sun Microsystems, offering facilities similar to those of the X Window System. Communication is based on PostScript, and server functions can be extended.")

("NewWave" 
 "A graphical user interface and object-oriented environment from Hewlett-Packard, based on Windows and available on UNIX workstations.")

("NeXTstep" 
 "A graphical interface builder, object-oriented application builder, and windowing software for the NeXT and IBM AIX systems.")

("NFF" 
 "Neutral File Format. A minimal scene description language.")

("NFS" 
 "Network File System: developed by Sun to allow a computer to access files over a network as if they were on local disks; now public domain, a de facto standard.")

("NFT" 
 "Network File Transfer. An INTERLINK command.")

("Nial" 
 "Nested Interactive Array Language. A high-level array-oriented procedural language based on a mathematical theory of arrays, developed at Queen's University. It combines APL data structure ideas with LISP-style evaluation concepts and a conventional control structure syntax.")

("NIAM" 
 "Natural Language \(or Nijssen\) Information Analysis Method: a method for data modelling. \(see \"Conceptual Scheme and Relational Database Design\", Nijssen and Halpin, Prentice-Hall, 1989\)")

("NII" 
 "National Information Infrastructure \(USA\).")

("NIH" 
 "The US National Institutes of Health.")

("NIHCL" 
 "A class library for C++ from the NIH.")

("NISO" 
 "National Information Standards Organisation \(USA\). NISO Standards cover many aspects of library science, publishing, and information services, and address the application of both traditional and new technologies to information services.")

("NIST" 
 "National Institute of Standards and Technology, USA \(formerly NBS\).")

("NITF" 
 "National Imagery Transmission Format.")

("NLP" 
 "Natural Language Processing.")

("NLS" 
 "Native Language System: a set of interfaces specified by X/Open for developing applications to run in different natural language environments.")

("NLSR" 
 "Natural Language Software Registry. A summary of the capabilities and sources of language processing software available to researchers.")

("NMF" 
 "Network Management Forum of OSI.")

("NNTP" 
 "Network News Transfer Protocol: the protocol used for distributing news on the Internet.")

("Node" 
 "see Hypertext")

("Normal form" 
 "A relation in a relational database is said to be in normal form if it satisfies certain constraints. Codd's original work defined three such forms.")

("NoteCards" 
 "An ambitious hypertext system developed at Xerox PARC, \"designed to support the task of transforming a chaotic collection of unrelated thoughts into an integrated, orderly interpretation of ideas and their interconnections\".")

("Novell" 
 "A proprietary local area network protocol developed by Novell Netware for the interconnection of PCs over Ethernet.")

("NOWEB" 
 "A system of structured programming and documentation from M.Speh in DESY. See Literate Programming")

("NQIC" 
 "National Quality Information Centre of the IQA systems.")

("NQS" 
 "Batch processing software for UNIX systems.")

("NREN" 
 "National Research and Education Network \(USA\).")

("NSAI" 
 "National Stsndards Authority of Ireland.")

("NSE" 
 "Network Software Environment: a proprietary CASE framework from Sun Microsystems.")

("NSF" 
 "National Science Foundation \(USA\).")

("NSRD" 
 "National Software Reuse Directory. A directory of reusable software in the ASSET system, now incorporated in the Asset Reuse Library.")

("NTIS" 
 "National Technical Information Service of the US Department of Commerce.")

("NTP" 
 "Network Time Protocol: a protocol built on top of TCP/IP that allows local clocks to be synchronised with reference clocks on the Internet.")

("NURBS" 
 "Non-Uniform Rational B-Splines, a technique used in CAD etc..")

("Nu Thena" 
 "A software vendor specialising in rapid prototyping tools for real-time hardware and software systems, collaborating with DAZIX.")

("NuThena" 
 "See Nu-Thena")

("OAK" 
 "An early name for Java")

("OATH" 
 "Object-oriented Abstract Type Hierarchy, a class library for C++ from Texas Instruments.")

("Oberon" 
 "A programming language developed by N. Wirth and J. Gutknecht as a successor to Modula 2.")

("Object management system" 
 "In an IPSE, the system which maintains information about the system under development.")

("Object-oriented database" 
 "A system offering DBMS facilities in an object-oriented environment.")

("Object-oriented programming" 
 "see object-oriented")

("Object-oriented" 
 "Applied to analysis, design and programming. The basic concept in this approach is that of objects, which consist of data structures encapsulated with a set of routines, often called \"methods\" which operate on the data. Operations on the data must be performed via these methods, which are common to all instances of objects of a particular class. Thus, the interface to objects is well defined, and allows the code implementing the methods to be changed so long as the interface remains the same.")

("Object" 
 "In object-oriented programming, an instance of a data structure defined according to the template provided by its class, and which can respond to the messages defined by its class.")

("ObjectBroker" 
 "A distributed object system from DEC based on the CORBA standard.")

("ObjectCenter" 
 "A product offering similar facilities to CodeCenter for the C++ language, plus class browsing facilities etc \(formerly Saber-C++\).")

("Objecteering" 
 "An Object Oriented design tool from Softeam, based on the Class Relation Methodology, with C++ code generation.")

("Objective C" 
 "A Smalltalk-like extension of the C language which provides the possibility to use object-oriented programming constructs")

("Objective PASCAL" 
 "An extension of the PASCAL language which provides the possibility to use object-oriented programming constructs.")

("Objectworks" 
 "An object-oriented development environment developed by ParcPlace, available under Smalltalk and C++.")

("OBST" 
 "A persistent object management system developed by FZI Karlsruhe for the STONE project.")

("Occam" 
 "A programming language which facilitates writing parallel programs, allowing the programmer to specify whether processes are to be executed sequentially or in parallel. Based on CSP, it was originally developed for the Transputer.")

("OCLC" 
 "Online Computer Library Center.")

("OCR" 
 "Optical Character Recognition: recognition of printed or written characters by computer.")

("OCS" 
 "Object Compatibility Standard: an 88open standard for compilers and linkers.")

("ODA" 
 "Open \(formerly Office\) Document Architecture: an ISO standard \(8613\) for describing documents. It allows text, graphics, and facsimile documents to be transferred between different systems.")

("ODAC" 
 "The ODA consortium.")

("ODIF" 
 "Open Document Interchange Format: part of the ODA standard.")

("ODL" 
 "Object Definition Language from ODMG.")

("ODMG" 
 "Object Data Management Group. A vendor consortium developing standards for Object Data Definition and Manipulation Languages.")

("ODP" 
 "Open Distributed Processing. An ISO standardisation activity.")

("ODT" 
 "Open Desktop.")

("OEW" 
 "Object Engineering Workbench. A design tool for C++.")

("OFF" 
 "Object File Format for interchange and archiving of 3D objects, from Digital Equipment Corporation.")

("OLE" 
 "Object Linking and Embedding. A distributed object system from Microsoft.")

("OLTP" 
 "On-Line Transaction Processing: the processing of transactions by computers in real time.")

("OMA" 
 "Object Management Architecture: a set of standards under study by OMG.")

("OMF" 
 "Object Management Facility: part of the DAA proposed by Hewlett-Packard and Sun.")

("OMF" 
 "Open Model Forum for modelling and simulation tool standards.")

("OMG" 
 "Object Management Group: a consortium aimed at setting standards in object-oriented programming, especially for distributed applications.")

("OML" 
 "Object Manipulation Language from ODMG.")

("OML" 
 "OPEN Modelling Language.")

("OMT" 
 "An object-oriented methodology .")

("OMTool" 
 "A graphical tool from General Electric Advanced Concepts Center for design and analysis of systems with the OMT methodology with some C++/SQL code generation.")

("ONC" 
 "Open Network Computing: Sun's network protocols.")

("OnX" 
 "A graphics package from LAL Orsay")

("OO" 
 "Object-oriented: for example Analysis \(OOA\), Design \(OOD\), Programming \(OOP\), Programming Language \(OOPL\), Data Bases \(OODBMS\) etc.")

("OOA" 
 "Object-oriented Analysis.")

("OOD" 
 "Object-oriented Design.")

("OODBMS" 
 "Object-oriented database management system.")

("OODL" 
 "Object-oriented Dynamic Language.")

("OOP" 
 "Object-oriented programming.")

("OOPL" 
 "Object-oriented programming language: a language such as C++, Eiffel, Objective-C etc designed to support object-oriented programming.")

("OOPSLA" 
 "Conference on Object-oriented Programming Systems, Languages and Applications.")

("OOSD" 
 "Object-oriented structured design: a design method elaborated from structured design and incorporating the essential features of the object-oriented approach.")

("Open Desktop" 
 "A UNIX environment from SCO. \(part of the ACE initiative\).")

("Open Inventor" 
 "An object-oriented toolkit for developing interactive 3D graphics applications. It also defines an ASCII file format for exchanging 3D data among applications, which is the basis for VRML.")

("Open Look" 
 "A graphical user interface and window manager from Sun and AT&T.")

("Open Software Foundation" 
 "See OSF .")

("OpenDoc" 
 "A compound document architecture from CIL based on CORBA. It aims to enable embedding of features from different applications into a single working document.")

("OpenGL" 
 "An emerging graphics standard providing advanced rendering capabilities.")

("OpenWindows" 
 "A server program for the Sun which handles SunView, NeWS and X Window System protocols.")

("OQL" 
 "Object Query Language from ODMG.")

("Oracle*CASE" 
 "A set of CASE tools from Oracle.")

("Oracle Card" 
 "A hypercard-like product from Oracle for constructing DB applications, running on PC and Macintosh.")

("Oracle Toolkit" 
 "See Adaptable User Interface.")

("Oracle" 
 "A vendor of database management systems: also their relational DBMS.")

("ORB" 
 "Object Request Broker: part of the OMG standard.")

("ORKID" 
 "Open Real-time Kernel Interface Definition.")

("OS/2" 
 "An operating system from IBM and Microsoft for the PS/2 range of microcomputers.")

("OSA" 
 "Open Scripting Architecture. A CIL approach to the coexistence of multiple scripting systems.")

("OSE" 
 "Open Systems Environment.")

("OSF" 
 "Open Software Foundation. A foundation created by nine computer vendors, \(Apollo, DEC, Hewlett-Packard, IBM, Bull, Nixdorf, Philips, Siemens and Hitachi\) to promote \"Open Computing\". It is planned that common operating systems and interfaces, based on developments of UNIX, the X Window System, etc. will be forthcoming for a wide range of different hardware architectures.")

("OSI" 
 "Open Systems Interconnection: a seven-layer reference model developed by ISO as a framework for the development of standards for interconnecting heterogeneous computers.")

("OSTC" 
 "Open Systems Testing Consortium. An open organisation operating harmonised conformance testing services for OSI telecommunications and IT protocols.")

("OTI" 
 "Open Tool Interface.")

("OVL" 
 "Object Verification Language from ODMG.")

("OWL" 
 "A software company offering the Guide hypertext system .")

("P2P" 
 "Person to Person. A range of desktop conferencing products from IBM.")

("P-CAD" 
 "A CAE system marketed by CADAM, an IBM company.")

("PACS" 
 "Public Access Computer Systems.")

("Page Description Language" 
 "A language such as Adobe PostScript or Xerox Interpress which allow the appearance of a printed page to be described in a high-level device-independent way. Printing then becomes a two-stage process: an application produces a description in the language, which is then interpreted by a specific output device. Such a language can serve as an interchange standard for transmission and storage of printable documents.")

("Panda" 
 "An Internet navigation and information retrieval system from the University of Iowa.")

("Pansophic" 
 "A Software Engineering company in the US.")

("Parser" 
 "A function that recognizes valid sentences of a language by analysing the syntax structure of a set of tokens passed to it from a lexical analyzer.")

("PARADIGM PLUS" 
 "A configurable object-oriented CASE tool from Proto Soft Inc..")

("PARC" 
 "See Xerox PARC.")

("PaRC" 
 "A workstation cluster for engineering computing at CERN.")

("Parlog++" 
 "An object-oriented extension to MacParlog. It combines object-oriented and parallel logic programming, giving the benefits of both paradigms within a single coherent development environment.")

("Pascal" 
 "A programming language designed by N. Wirth for teaching purposes, emphasising structured programming constructs, data structures and strong typing.")

("Pattern" 
 "A formal way to describe a solution to a commonly recurring programming problem.")

("PATCHY" 
 "A FORTRAN code management program written at CERN.")

("PAW" 
 "Physics Analysis Workbench - general purpose portable tool for analysis and presentation of physics data.")

("PAW++" 
 "An extended version of PAW with a Motif human interface.")

("PC" 
 "Personal Computer.")

("PCA" 
 "A dynamic analyser from DEC giving information on run time performance and code utilisation.")

("PCL" 
 "Printer Control Language \(from Hewlett Packard\).")

("PCL" 
 "Portable Common LOOPS. A portable CLOS implementation.")

("PCTE+" 
 "A European NATO specification based on PCTE with security enhancements.")

("PCTE" 
 "Portable Common Tool Environment: an ECMA standard framework for software tools developed in the Esprit programme. It is based on an entity-relationship Object Management System and defines the way in which tools access this.")

("PCX" 
 "A bitmap format from Zsoft.")

("PDDM" 
 "Product Data and Document Management.")

("PDF" 
 "Portable Document Format from Adobe Systems.")

("PDL" 
 "Page Description Language.")

("PDL" 
 "Program Design Language.")

("PDM" 
 "Product Data Management. An integrated system for managing all types of technical data concerning a product.")

("PDS" 
 "Planetary Data Systems format from.")

("PDSA cycle" 
 "Plan, Do, See, Approve \(from Japan\).")

("PEM" 
 "Privacy Enhanced Mail. An Internet standard \(RFC 1421-1424\).")

("PEP" 
 "Protocol Extension Protocol. A proposed system to allow HTTP clients and servers to negotiate protocol extensions.")

("Perceptron" 
 "This term is sometimes used to refer to a single McCulloch-Pitts neuron, but may also refer to a network of neurons in which the output\(s\) of some neurons are connected through weighted connections to the input\(s\) of other neurons. The term multilayer perceptron specifically refers to a network composed of more than one layer of neurons, with some or all of the outputs of each layer connected to one or more of the inputs of another layer. The first layer is called the input layer, the last one is the output layer, and in between there may be one or more hidden layers.")

("Perl" 
 "Practical Extraction and Report Language. An interpreted scripting language for scanning text files, extracting information, and printing reports. It combines features of c , sed , awk and sh.")

("Personal Computer" 
 "A general-purpose single-user microcomputer designed to be operated by one person at a time.")

("Petri net" 
 "A graphical representation of concurrent systems in terms of tokens, places and transition bars.")

("PEX" 
 "\(PHIGS Extension to X\) Extension to the X Window System providing 3-D graphics support.")

("PGP" 
 "Pretty Good Privacy. A set of encryption tools for electronic mail etc.")

("PHIGS" 
 "Programmers Hierarchical Interactive Graphics System: an ANSI/ISO standard.")

("PICS" 
 "Platform for Internet Content Selection.")

("PII" 
 "Process Improvement Institute.")

("PIM" 
 "Product Information Management. See PDM")

("PinK" 
 "PinK is not KUIP. An interface between Tcl/Tk, BLT, ADAMO and DAD from DESY.")

("Plexus" 
 "A set of modular WWW server software written in Perl.")

("PMM" 
 "Process Maturity Model.")

("PNG" 
 "Portable Network Graphics. A standard for bitmapped image files.")

("Polymorphism" "In object-oriented programming, the term is used to describe variables which may refer at run-time to objects of different classes. For example, the variable \"myVehicle\" could refer to an object of class \"motorCar\" 
 or \"Truck\".")

("POSIX" 
 "Portable Operating System Interface for computer environments. A set of IEEE standards designed to provide application portability. IEEE1003.1 defines a UNIX-like operating system interface, 1003.2 the shell and utilities, and 1003.4 real-time extensions.")

("POSS" 
 "Persistent Object Service Specification: an OMG specification.")

("POSTGRES" 
 "An active DBMS from Univ. of Calif. Berkeley.")

("Postscript" 
 "A page description language from Adobe Systems Inc. Its primary application is to describe the appearance of text, graphical shapes and sampled images on printed or displayed pages. A program in PostScript can communicate a document description from a composition system to a printing system in a device-independent way. Many printers now interpret PostScript directly.")

("PPP" 
 "Point to Point Protocol.")

("PPTP" 
 "Point to Point Tunneling Protocol.")

("Pragma" 
 "A standardised form of kluge in Ada.")

("Predicate calculus" 
 "A notation for representing logical statements which goes beyond propositional calculus in certain ways.")

("PREMO" 
 "Presentation Environment for Multimedia Objects. An ISO standard under development for creation, presentation and interaction with information using single or multiple media.")

("Presentation Manager" 
 "The user interface to the OS/2 system.")

("ProDoc" 
 "A set of tools for software documentation from SPC")

("Project assurance" 
 "The process of specifying the support system: techniques, internal standards, measurements, tools, and training for a project; counseling the project team in the application of these elements and monitoring the adherence to the standards.")

("Project management" 
 "The process of planning, organizing, staffing, directing, and controlling the production of a system. Software tools are available to help with this.")

("Project planning" 
 "See Project management.")

("PROLOG" 
 "A language for PROgramming in LOGic.")

("Prometheus" 
 "A high-level programming language designed for logic, mathematics, and artificial intelligence. It contains elements from C, Pascal, LISP and Prolog plus novel features.")

("Propositional calculus" 
 "A system of symbolic logic.")

("PROST" 
 "Programme for Research in Open Systems Testing of the DTI")

("Protocol" 
 "An agreement about how to transmit data, especially across networks. Low level protocols define the electrical and physical standards to be observed, and deal with the transmission and error detection and correction of the bit stream. High level protocols deal with the data formatting, including the form of messages, the terminal to computer dialogue, files, etc.")

("Prototyper" 
 "An interface builder for the Macintosh from Smethers Barnes .")

("Prototyping" 
 "The creation of a model and the simulation of all aspects of a product. CASE tools support different degrees of prototyping. Some offer the end-user the ability to review all aspects of the user interface and the structure of documentation and reports before code is generated.")

("PS" 
 "PostScript.")

("PSA" 
 "Problem Statement Analyzer: see PSL/PSA.")

("Pseudocode" 
 "A notation resembling a programming language but not intended for actual compilation. It usually combines some of the structure of a programming language with an informal natural-language description of the computations to be carried out. It is often produced by CASE systems as a basis for later hand coding.")

("PSL/PSA" 
 "Problem Statement Language/Problem Statement Analyser: a CASE system developed by D.Teichroew. It allows computer-based development and analysis of a statement of requirements, and assistance during the design phase.")

("PSL" 
 "Problem Statement Language: see PSL/PSA.")

("PSL" 
 "Problem Statement Language: see PSL/PSA.")

("PSP" 
 "Personal Software Process. Methods to improve the quality of work of software engineers.")

("PTI" 
 "Portable Tool Interface: a standard such as PCTE, allowing interworking between different software tools via defined interfaces to the user and to the repository or object management system.")

("PureLink" 
 "An incremental linker from Pure Software.")

("Purify" 
 "A debugging tool from Pure Software.")

("PURL" 
 "Persistent URL. Instead of pointing directly to the location of an Internet resource, a PURL points to a resolution service that associates the PURL with the actual URL and returns that URL to the client. See the OCLC PURL Service.")

("PV~WAVE" 
 "Interactive scientific visualisation software from Visual Numerics.")

("QA" 
 "Quality Assurance.")

("QAM" 
 "Quality Assurance Management.")

("QBE" 
 "A query language.")

("QIP" 
 "Quality Improvement Paradigm.")

("Quantify" 
 "A performance analysis tool from Pure Software.")

("Query language" 
 "A language such as SQL whereby users of a database system can interactively formulate requests, generate reports etc.")

("RAD" 
 "Rapid Application Development. Often applied to tools such as Microsoft Visual Basic, Borland Delphi, Oracle Power Objects.")

("RAL" 
 "Rutherford Appleton Laboratory \(UK\).")

("RAID" 
 "Redundant Array of Inexpensive Disks. A data storage technique.")

("RARE" 
 "Reseaux Associes pour la Recherche Europeenne: an association of national and international European networks and users.")

("RBSE" 
 "Repository Based Software Engineering. A NASA research and development programme.")

("RCS" 
 "A code management system.")

("Rdb" 
 "DEC's SQL-based relational DBMS for VAX/VMS.")

("RDBA" 
 "Remote Database Access: a standard permitting the exchange of information between different DBMS systems.")

("RDBMS" 
 "Relational database management system.")

("Re-engineering" 
 "The examination and modification of a system to reconstitute it in a new form and the subsequent implementation of the new form.")

("Real-time" 
 "Generally used to describe systems that must guarantee a response to an external event within a given time")

("Realtime" 
 "see real-time")

("Redocumentation" 
 "The creation or revision of a semantically equivalent representation within the same relative abstraction level. The resulting forms of representation are usually considered alternate views intended for a human audience.")

("Reengineering" 
 "see Reverse engineering")

("REFINE" 
 "A set of reverse engineering tools from Reasoning Systems")

("Relation" 
 "A table in a relational database")

("Relational database" 
 "See Relational DBMS.")

("Relational DBMS" 
 "A DBMS based on the relational model developed by Codd. It allows the definition of data structures, storage and retrieval operations, and integrity constraints. In such a database, the data and relations between them are organised in tables. INGRES and Oracle are well-known examples.")

("Released version" 
 "A version of an object that is not modifiable, as designated by some person. Also known as baseline. See change management.")

("Rendezvous" 
 "In Ada, the method of synchronising the activity of different tasks.")

("Repository" 
 "The core of a CASE tool is typically a DBMS where all development documents are stored.")

("REQUEST" 
 "REliability and QUality of European Software Technology. An Esprit project \(now terminated\).")

("Requirements" 
 "The first stage of software development should be to define requirements with the potential users. In modern methods these requirements should be testable, and will usually be traceable in later development stages.")

("Restructuring" 
 "The transformation from one representation form to another at the same relative abstraction level, while preserving the subject system's external behavior \(functionality and semantics\).")

("Reusability" 
 "The possibility of using code developed for one application in another application: traditionally achieved using program libraries. Object-oriented programming offers the potential for greater reusability of code via its techniques of inheritance, genericity etc. Class libraries with intelligent browsers and application generators are under development to help in this process.")

("Reuse" 
 "The planned use of software artefacts for the solution of multiple problems.")

("Reverse Engineering" 
 "The process of analyzing an existing system to identify its components and their interrelationships, and create representations of the system in another form or at a higher level of abstraction. Usually undertaken in order to redesign the system for better maintainability.")

("RFC" 
 "Request For Comment. The name by which Internet standards are known.")

("RFT" 
 "Request For Technology - process established by OSF to get proposals for new standards.")

("RIFF" 
 "Resource Interchange File Format from Microsoft.")

("RIPE" 
 "Reseaux IP Europeens. A collaborative organisation of European Internet service providers.")

("RISC" 
 "Reduced Instruction Set Computer; one whose design is based on the rapid execution of a sequence of simple instructions rather than on the provision of a large variety of complex instructions.")

("RLF" 
 "Reuse Library Framework of the DoD")

("RMP" 
 "Reliable Multicast Protocol.")

("RM-ODP" 
 "The ISO Reference Model for Open Distributed Environments.")

("RNIS" 
 "Reseau Numerique a Integration de Services. French for ISDN.")

("ROOM" 
 "Real-Time Object-Oriented Modeling. An Object-Oriented analysis and design approach.")

("ROOT" 
 "An object oriented framework for large scale data analysis at CERN.")

("Root version" 
 "The initial value of an object. See change management.")

("RPC" 
 "Remote Procedure Call: a call to a routine that results in code being executed on a different system from the one where the request originated. An RPC system allows calling procedures and called procedures to execute on different systems without the programmer needing to explicitly code for this.")

("RSA" 
 "Rivest, Shamir, Adleman public key encryption technique \(used by PGP\)")

("RSVP" 
 "Rapid System Virtual Prototyping.")

("RTEE" 
 "Real Time Engineering Environment: a set of CASE tools produced by Westmount Technology B.V.")

("RTF" 
 "Rich Text Format: an interchange format from Microsoft for exchange of documents between Word and other document preparation systems.")

("RTL" 
 "Register Transfer Language: a kind of HDL used in describing the registers of a computer or digital electronic system, and the way in which data is transferred between them.")

("RTSA" 
 "Real-time structured analysis: versions of structured analysis capable of modelling real-time aspects of software.")

("Rule-based" "Having to do with systems that infer or use \"rules\" 
 \(i.e.logical statements\).")

("SA" 
 "Structured Analysis.")

("SAA" 
 "Systems Application Architecture: IBM's family of standard interfaces which enable software to be written independently of hardware and operating systems.")

("Saber-C++" 
 "see ObjectCenter.")

("Saber-C" 
 "see CodeCenter.")

("SADT" 
 "Structured Analysis and Design Technique.")

("SARA" 
 "Stichting Academisch Rekencentrum Amsterdam \(Academic Computing Services Amsterdam\).")

("SAGE" 
 "System Administrators Guild. A Special Technical Group within USENIX.")

("SASD" 
 "Structured Analysis, Structured Design.")

("SATAN" 
 "Security Administrator Tool for Analyzing Networks.")

("Sather" 
 "An object-oriented programming language that is a simplified optimized variant of Eiffel.")

("SBM" "Solution Based Modelling. a software development process described in the book \"Developing Object-Oriented Software for the Macintosh\" 
 written by Neal Goldstein and Jeff Alger, published by Addison Wesley in 1992.")

("SCCS" 
 "Source Code Control System: a popular code management system for UNIX systems.")

("SCM" 
 "Software Configuration management or Source Code management.")

("Schematic capture" 
 "The process of entering the logical design of an electronic circuit into a CAE system by creating a schematic representation of components and interconnections.")

("Scheme" 
 "A dialect of Lisp.")

("Schlaer-Mellor" 
 "An Object-Oriented Analysis \(OOA\) modeling method that addresses the the integration of structural and behavioral properties.")

("SCI" 
 "Scalable Coherent Interface, IEEE Std 1596-1992.")

("SCO" 
 "The Santa Cruz Operation, a leading supplier of UNIX systems for systems based on Intel microprocessors. Suppliers of Xenix and Open Desktop.")

("SCOPE" 
 "Software Assessment and Certification Programme. An Esprit project.")

("SCPI" 
 "Standard Commands for Programmable Instruments: a standard complementing IEE 488 developed by Hewlett-Packard and promoted by the SCPI Consortium, 8380 Hercules Drive, Suite P3, La Mesa, CA 91942, USA")

("ScriptX" 
 "A dynamic object-oriented programming language and class library for multimedia from Kaleida Labs.")

("SCSI" 
 "Small Computer Systems Interface.")

("SD" 
 "Structured Design: a program design method.")

("SDD" 
 "Software Design Description. ANSI/IEEE 1016-1987 specifies IEEE Recommended Practice for SDD.")

("SDE" 
 "Software Development Environment: equivalent to SEE.")

("SDIF" 
 "SGML Document Interchange Format.")

("SDL" 
 "Specification and Design Language: defined by the CCITT \(recommendation Z100\) to provide a tool for unambiguous specification and description of the behaviour of telecommunications systems. The area of application also includes process control and real-time applications. SDL provides a Graphic Representation \(SDL/GR\) and a textual Phrase Representation \(SDL/PR\), which are equivalent representations of the same semantics. A system is specified as a set of interconnected abstract machines which are extensions of the Finite State Machine \(FSM\).")

("SDM" 
 "Schematic Data Model.")

("SDS" 
 "Schema Definition Set in PCTE.")

("SE-ODP" 
 "Support Environment for Open Distributed Processing: an ECMA standard.")

("SE" 
 "Software Engineering, the methods used in developing software.")

("SEE" 
 "Simultaneous Engineering Environment: a CAE framework from DAZIX.")

("SEE" 
 "Software Engineering Environment: a set of management and technical tools to support software development, usually integrated in a coherent framework; equivalent to an IPSE.")

("SEI" 
 "Software Engineering Institute \(Carnegie Mellon University\).")

("SEL" 
 "Software Engineering Laboratory. The Institute for Information Technology of the National Research Council Canada. Also NASA's Goddard Space Flight Center.")

("Selector" 
 "In Smalltalk or Objective-C, the syntax of a message which selects a particular method in the target object.")

("Self" 
 "An object oriented programming language from Stanford, and an object oriented programming system from Sun Microsystems.")

("Semaphore" 
 "The classic method for restricting access to data shared between several cooperating processes .")

("SEP" 
 "A SASD tool from IDE.")

("SERC" 
 "Software Engineering Research Center \(Purdue University\).")

("Server" 
 "A computer which, by means of network connections, carries out parts of a computing task on behalf of one or more remote computers.")

("SES/workbench" 
 "An iconic simulation and design tool, linked to some of the major CASE systems now available or in development.")

("SET" 
 "Standard d'Echange et de Transfert: a French standard for exchange of CAD data.")

("Setext" 
 "A markup scheme intended for documents that are both human- and computer-readable.")

("SFA" 
 "Software Frameworks Association.")

("SGI" 
 "Silicon Graphics Incorporated, a vendor of graphical workstations and software")

("SGML" 
 "Standard Generalised Markup Language \(ISO 8879\). A generic markup language for representing documents. SGML is a system for defining structured document types, and markup languages to represent instances of those document types.")

("SGML Open" 
 "A non-profit, international consortium of providers of products and services, dedicated to accelerating the further adoption, application, and implementation of SGML.")

("ShapeTools" 
 "A code management system for UNIX from TU Berlin.")

("Shape_VC" 
 "A code management system which offers version control functionality similar to systems like RCS or SCCS with some extensions and a more UNIX-like command interface.")

("SHARE" 
 "An international users group of IBM and compatible hardware and software.")

("Shelf" 
 "A public library of classes for the Eiffel language.")

("Shell Script" 
 "A program written to be interpreted by the shell of an operating system, especially UNIX.")

("Shell" 
 "The outer part of an operating system, especially UNIX, which provides the user interface, as opposed to the kernel which provides the basic services to processes. The commonest UNIX shells are the c shell \(csh\) and the Bourne shell \(sh\) .")

("Shen" 
 "A security scheme for WWW.")

("SHIFT" 
 "Scalable Heterogeneous Integrated Facility Testbed. A parallel processing project at CERN.")

("SICL" 
 "Standard Instrument Control Library: a platform-independent API for software to control and test electronic instruments conforming to IEE 488")

("SICS" 
 "Swedish Institute for Computer Science.")

("SIGhyper" 
 "Special Interest Group on Hypertext and Multimedia of the SGML Users' Group.")

("SIMD" 
 "Single Instruction Multiple Data: a form of parallelism in multiprocessor computing where there is a single instruction stream \(programs\) operating concurrently on several data streams.")

("SIMEX" 
 "A set of C++ classes from the University of Minnesota, that provides a framework for building discrete event simulation models.")

("SIMON" 
 "System of Internet Mapping for Organised Navigation.")

("Simscript" 
 "A free-form, English-like general-purpose simulation language. SIMSCRIPT II.5 from CACI has evolved from the original work on SIMSCRIPT by H.Markowitz.")

("SIMULA" 
 "A program based on Algol 60 with extensions for simulation, which was a precursor of the object-oriented approach.")

("Single Inheritance" 
 "The property of an object-oriented language which restricts a sub-class to be derived from only one parent.")

("Sisal" 
 "Streams and Iterations in a Single-Assignment Language. A general-purpose functional language from CWI.")

("SLIP" 
 "Serial Line IP.")

("SMA" 
 "Software Maintenance Association.")

("Smalltalk" 
 "A pioneering object-oriented programming system developed at the Xerox Palo Alto research centre. It includes a language \(usually interpreted\), a programming environment, and an extensive object library.")

("SMCC" 
 "Sun Microsystems Computer Corporation")

("SMDL" 
 "Standard Music Description Language, based on HyTime")

("SMG" 
 "Screen Management Guidelines - a VMS package of run-time library routines providing windows on VT100 terminals.")

("SML/NJ" 
 "Standard ML of New Jersey.")

("SML" 
 "Standard ML: a functional language.")

("SMSL" 
 "Standard Multimedia Scripting Language.")

("SNA" 
 "Systems Network Architecture - IBM's networking standard.")

("Sniff" 
 "A C++/C programming environment providing browsing, cross-referencing, design visualization, documentation, and editing support. Developed by UBS Switzerland and marketed by takeFive Salzburg. \(See also SNiFF+. \)")

("SNOBOL" 
 "String Oriented Symbolic Language. A language from the 1960s for string manipluation.")

("SoftBench" 
 "An IPSE from Hewlett-Packard.")

("Softlab" 
 "A software engineering company strong in UK and Germany.")

("Software AG" 
 "SE company from FRG.")

("Software BackPlane" 
 "A CASE framework from Atherton.")

("Software bus" 
 "A support environment for heterogeneous distributed processing, such as the ANSA Testbench.")

("Software Engineering" 
 "A systematic approach to the analysis, design, implementation and maintenance of software. It usually involves the use of CASE tools. There are various models of the software life-cycle, and many methodologies for the different phases.")

("Software Metrics" 
 "Measures of software quality which indicate the complexity, understandability, testability, description and intricacy of code.")

("Software through Pictures" 
 "see StP.")

("SPARC" 
 "see ANSI/SPARC Architecture.")

("SOIF" 
 "Summary Object Interchange Format in the Harvest system.")

("SOM" 
 "System Object Model. An implementation of CORBA by IBM.")

("SOMA" 
 "Semantic Object Modelling Approach. An Object-Oriented analysis and design approach.")

("Sparcstation" 
 "A family of workstations from Sun .")

("SPC" 
 "Software Productivity Centre. A non-profit organization based in Vancouver, BC, Canada with the mandate to assist software developers to improve their software engineering process.")

("SPDL" 
 "Standard Page Description Language: a draft within the ODA standard.")

("SPEC" 
 "Standard Performance Evaluation Corporation. Formed to establish, maintain and endorse a standardized set of relevant benchmarks that can be applied to the newest generation of high-performance computers.")

("Specific markup" 
 "In computerised document preparation, a method of adding formatting commands to the text to control layout, such as new line, new page, center text etc. \(see Generic markup\).")

("SPI" 
 "Software Process Improvement.")

("SPIN" 
 "Software Process Improvement Network. Local interest groups sponsored by SEI.")

("SPT" 
 "Software Process Technology.")

("Spreadsheet" 
 "A type of application which manipulates data in rows and columns of cells. The value in a cell is calculated by a formula which can involve other cells. Popular in commercial applications.")

("Spring" 
 "A distributed object-oriented operating system from Sun.")

("Sprite" 
 "An operating system from Berkeley supporting multiprocessing and distributed files.")

("SQL/DS" 
 "A database package from IBM including a relational DBMS.")

("SQL2" 
 "An extended version of the SQL standard.")

("SQL" 
 "Structured Query Language: ISO, ANSI standard user front end to a relational database management system.")

("SRI" 
 "Stanford Research Institute.")

("SSADM" 
 "A software engineering method and toolset required by some UK government agencies.")

("SSII" 
 "Societe de Service en Ingenierie Informatique.")

("SSL" 
 "Secure Sockets Layer. A scheme for secure WWW communications.")

("Standards" 
 "Although boring, standards are necessary for interworking, portability and reusability. They may be de facto standards for various communities, or officially recognised national or international standards. Some important bodies concerned in one way or another with Software standards are ISO, ANSI, DoD, ECMA, IEEE, IETF, OSF.")

("StarBurst" 
 "An active DBMS from IBM Almaden Research Center.")

("STARS" 
 "Software Technology for Adaptable Reliable Systems. A DARPA project.")

("STAS" 
 "Scientific and Technical Attribute and element Set. Defines standard identifiers for referring to searchable fields in scientific databases.")

("State Diagram" 
 "see State Transition Diagram.")

("State transition diagram" 
 "A diagram consisting of circles to represent states and directed line segments to represent transitions between the states. One or more actions may be associated with each transition. The diagrom represents a Finite State Machine.")

("STD" 
 "State Transition Diagram.")

("STDWIN" 
 "A windowing interface from CWI with windows, menus, modal dialogs, mouse and keyboard input, scroll bars, drawing primitives, etc that is portable between platforms. STDWIN is available for Macintosh and the X Window System.")

("STEP" 
 "Standard for the exchange of product model data: a draft ISO standard for the exchange of CAD data.")

("StepStone" 
 "Corporation founded by Brad Cox, responsible for Objective-C.")

("STL" 
 "Standard Template Library for C++.")

("STL" 
 "Semantic Transfer Language. IEEE 1175: IEEE Trial-Use Standard Reference Model for Computing System Tool Interconnections.")

("STONE" 
 "A Structured and Open Environment: a project supported by the German Ministry of Research and Technology \(BMFT\) to design, implement and distribute a SEE for research and teaching.")

("StP" 
 "Software through Pictures: a set of CASE tools from IDE.")

("Strand" 
 "A concurrent programming language from Strand Software Technologies Limited.")

("Struct" 
 "A data type in C corresponding to a record in Ada or Pascal.")

("Structured analysis" 
 "One of a number of requirements analysis methods used in software engineering.")

("Structured design" 
 "One of a number of systematic top-down design techniques used in software engineering, usually after structured analysis.")

("Sublanguage" 
 "One of the languages associated with a DBMS, for example data-definition language or query language.")

("Sun" 
 "Sun Microsystems, a US workstation manufacturer with manufacturing capacity in Europe.")

("SunOS" 
 "The version of UNIX running on Sun workstations.")

("SunView" 
 "A windowing system from Sun Microsystems, superseded by NeWS.")

("Superclass" 
 "The class from which another class inherits \(see Inheritance\).")

("SVID" 
 "System V Interface Definition: allowing source code portability between different platforms running UNIX System V.")

("SWOT" 
 "Strengths, Weaknesses, Opportunities, Threats commercial product analysis.")

("Sybase" 
 "A relational DBMS vendor.")

("System V" 
 "One of the two major versions of the UNIX system, due to AT&T. \(see BSD\).")

("TAE Plus" 
 "A GUI builder from Century Computing.")

("TAFIM" 
 "Technical Architecture Framework for Information Management: a DoD standard.")

("Taligent" 
 "A software company set up by Apple, IBM and Hewlett-Packard.")

("Taos" 
 "An operating system kernel for parallel systems from Tao Systems.")

("TAPI" 
 "Telephony Application Programming Interface. A CTI standard from Microsoft and Intel.")

("TBK" 
 "Tool Builder Kit: a product from IPSYS which allows users to develop CASE tools appropriate to any software engineering methodology.")

("TCA" 
 "Trigger, Condition, Action model.")

("TC/IX" 
 "The LynxOS kernel ported to the MIPS R3000 RISC processor by CDC.")

("Tcl" 
 "Tool command language. A command language and associated library package running on a number of platforms.")

("Tcl/Tk" 
 "See Tk.")

("TCP/IP" 
 "A reliable connection-oriented protocol originated by DARPA for internetworking, encompassing both network and transport level protocols. While the terms TCP and IP specify two protocols, TCP/IP is often used to refer to the entire DoD protocol suite based upon these, including Telnet, FTP, UDP, and RDP.")

("Teamwork" 
 "A SASD tool from CADRE Technologies.")

("Tecate" 
 "A software system for exploratory visualization of data from networked sources including WWW.")

("TEI" 
 "Text Encoding Initiative. Defines a common interchange format for literary and linguistic data.")

("TELEPAC" 
 "The Swiss PTT X.25 Network.")

("TeleUSE" 
 "An interface builder for Motif .")

("Telnet" 
 "The Internet standard protocol for remote terminal connection service, running over TCP/IP. Telnet allows a user to log onto a remote host computer.")

("TELOS" 
 "The object system of LeLisp Version 16 and EULISP.")

("Template code" 
 "Pseudocode generated by an automated CASE system and requiring further hand-coding before compilation.")

("TestCenter" 
 "A testing environment for C and C++ programs from CenterLine Software.")

("Testing" 
 "The process of exercising a product to identify differences between expected and actual results and performance. Typically testing is bottom-up: unit test, integrate test and finally system test.")

("TET" 
 "Test Environment Toolkit project coordinated by X/Open")

("TeX" 
 "A computer typesetting program by D.E.Knuth popular for document preparation in the HEP community. It provides specific markup for text processing. .")

("Texel" "An object-oriented methodology \(see \"Object Oriented Methods\" by Ian Graham\).")

("Think C" 
 "An extension of ANSIC for the Macintosh by Symantec Corporation, similar to C++, to support object-oriented programming techniques.")

("TickIT" 
 "A software industry quality assessment scheme.")

("TIFF" 
 "Tag Image File Format from Aldus.")

("Tk" 
 "An extension to Tcl providing an interface to the X windows .")

("TLA" 
 "Three Letter Acronym.")

("Token" 
 "A basic, grammatically indivisible unit of a language.")

("Token ring" 
"A computer network arbitration scheme in which conflicts in the transmission of messages are avoided by the granting of \"tokens\"  which give permission to send. A station keeps the token while transmitting a message, if it has a message to transmit, and then passes it on to the next station.")

("Toolbuilder" 
 "see TBK")

("TOP" 
 "Technical/Office Protocol: a protocol stack for office automation developed by Boeing following the OSI model. This protocol is very similar to MAP except at the lowest levels, where it uses Ethernet \(IEEE 802.3\) rather than Token Bus \(IEEE 802.4\).")

("Transaction" 
 "A unit of interaction with a DBMS or similar system. It must be treated in a coherent and reliable way independent of other transactions .")

("Transputer" 
 "A family of microprocessors from Inmos with interprocessor links, programmable in Occam.")

("Trellis" 
 "An object-oriented application development system from DEC, based on the Trellis language.")

("TSAPI" 
 "Telephony Services Application Programming Interface. A CTI standard from Novell and AT&T.")

("TRUSIX" 
 "TRUSted unIX operating system.")

("TSEE" 
 "Technical and Engineering Environment: part of the RTEE toolset.")

("TULIP" 
 "The University Licensing Program. A cooperative research project for networked delivery and use of journals, by Elsevier Science and nine US Universities..")

("Tunes" 
 "A project to design a new computing environment at all levels of software.")

("TXL" 
 "A hybrid functional and rule-based language for source transformation applications from Queen's Univ. Canada.")

("UAA" 
 "Unified Agent Architecture.")

("UCS" 
 "Universal Character Set \(Universal Multiple-Octet Coded Character Set\) of ISO 10646.")

("UDP" 
 "User Datagram Protocol: the Internet standard protocol for sending datagrams between user programs. This protocol neither guarantees delivery nor does it require a connection. As a result it is lightweight and efficient, but all error processing and retransmission must be taken care of by the application program. This protocol is built on top of IP and uses IP for datagram delivery \(see TCP/IP\). .")

("UI" 
 "UNIX International: a consortium including Sun and AT&T, promoting an open environment base on UNIX System V including the Open Look windowing system.")

("UIL" 
 "User Interface Language: in OSF/Motif and DECwindows, a language for specifying widget hierarchies etc.")

("UIMS" 
 "User Interface Management System: a system supporting the development and execution of user interfaces, usually on top of windowing systems.")

("UIMX" 
 "An interface builder for Motif from Visual Edge.")

("UIS" 
 "A VMS graphics programming interface package for VAXstations.")

("Ultrix" 
 "A version of UNIX based on the Berkeley version, designed and implemented by DEC to run on their VAX and DECstation series of processors.")

("UNI" 
 "Ente Nazionale Italiano di Unificazione: the Italian national standards body, a member of ISO.")

("UNIX International" 
 "A consortium of AT&T and others formed to advise on the development of UNIX System V.")

("UNIX" 
 "Computer operating system developed by Bell Labs. Since it was written in C, it was possible to port it to run on different hardware architectures. It is now offered by many manufacturers and is the subject of an international standardisation effort. See also OSF.")

("UNO" 
 "Universal Network Objects.")

("URC" 
 "Uniform \(previously Universal\) Resource Characteristic \(Citation\).")

("URI" 
 "Uniform \(previously Universal\) Resource Identifier.")

("URL" 
 "Uniform \(previously Universal\) Resource Locator.")

("URN" 
 "Uniform \(previously Universal\) Resource Name.")

("Usenet" "The practice of using computer networks to exchange items of information grouped into \"newsgroups\" 
 by topic. This is supported by a number of diverse and informally applied mechanisms and conventions.")

("USENIX" 
 "The UNIX and Advanced Computing Systems Professional and Technical Association.")

("USL" 
 "UNIX System Laboratories: the software subsidiary of AT&T, responsible for UNIX System V and related software.")

("USMARC" 
 "See MARC.")

("UTF" 
 "Universal Text Format, an SGML standard for the news distribution indistry.")

("UTF" 
 "UCS Transformation Format of ISO 10646.")

("UUCP" 
 "The large international network of UNIX machines using the UUCP protocol to exchange news and electronic mail. .")

("V" 
 "A testbed for distributed system research .")

("Validation" 
 "The process of evaluating software at the end of the development process to ensure compliance with software requirements.")

("VAX DOCUMENT" 
 "A document preparation system from DEC.")

("VAX/VMS" 
 "see VMS.")

("VAX" 
 "A range of 32-bit computers manufactured by DEC.")

("VAXset" 
 "A set of software development tools from DEC, including a language-sensitive editor, compilers etc.")

("VAXstation" 
 "A family of workstations from DEC based on their VAX computer architecture.")

("VB" 
 "Visual Basic")

("VDL" 
 "Vienna Definition Language: an algebraic definition language, see VDM.")

("VDM" 
 "Vienna Definition Method: a program development method based on formal specification using the Meta-IV language.")

("VDM" 
 "Virtual Device Metafile.")

("VEE" 
 "see HP VEE.")

("Verification" 
 "The process of determining whether or not the products of a given phase in the life-cycle fulfill a set of established requirements.")

("Verilog SA" 
 "A French real-time software engineering company.")

("Verilog" 
 "A Hardware Description Language for electronic design and gate level simulation.")

("Version" 
 "A variant of the original value of an object. See change management")

("VHDL" 
 "Very High Speed Integrated Circuit Description Language: a high-level VLSI design language, now standardised as IEEE Std.1076.")

("VHE" 
 "Virtual Home Environment: a tool for using NFS on HP UX .")

("VIFF" 
 "Visualization Image File Format.")

("Viola" 
 "An experimental hypercard-like interpreted hypertext system by Pei Y. Wei of Berkeley.")

("VIP" 
 "Virtual Internet Protocol.")

("VIPA" 
 "VMEbus International Physics Association.")

("Visualisation" 
 "A method by which a computer system presents data to the user.")

("Visualization" 
 "A method by which a computer system presents data to the user.")

("Visual Basic" 
 "A programming language and development environment for Windows from Microsoft.")

("VITA" 
 "VMEbus International Trade Association.")

("VITAL" 
 "VHDL Initiative Towards ASIC Libraries.")

("VLIW" 
 "Very Long Instruction Word.")

("VLSI" 
 "Very Large Scale Integration. Refers to semiconductor chips composed of very many tightly packed logic elements or memories.")

("VM/CMS" 
 "Virtual Machine / Conversational Monitor System: an IBM operating system running on 43xx and 30xx series machines, providing efficient support for large numbers of interactive users.")

("VM" 
 "see VM/CMS.")

("VME" 
 "Common abbreviation for VMEbus.")

("VMEbus" 
 "A widely accepted backplane interconnection bus system developed by a consortium of companies led by Motorola, now standardized as IEEE Std. 1014.")

("VMS" 
 "The operating system offered by DEC as the standard system for their VAX range of processors.")

("VPN" 
 "Virtual Private Network. A computer network that appears to be a dedicated network to a particular set of users, whilst in fact using the infrastructure of public switched networks.")

("VRML" 
 "Virtual Reality Modeling Language.")

("VRTX" 
 "Virtual Real-Time Executive: a real-time operating system from ReadySystems for the Motorola 68000 family of microprocessors.")

("VSF" 
 "Virtual Software Factory: a product from Systematica which allows users to develop CASE tools appropriate to any software engineering methodology.")

("VSX" 
 "Verification Suite.for X/open")

("VTS" 
 "A suite of test programs for Motif from OSF.")

("VUE" 
 "Visual User Environment: a desktop manager for UNIX from Hewlett-Packard.")

("VUIT" 
 "Visual User Interface Tool: a WYSIWYG editor from DEC for building human interfaces to applications using OSF/Motif. It provides an interactive interface to UIL and the Motif toolkit.")

("VxWorks" 
 "A real-time software development environment and multitasking operating system from Wind River Systems that uses the VRTX kernel.")

("W3" 
 "See WWW.")

("W3C" 
 "The World Wide Web Consortium.")

("WAIS" 
 "Wide Area Information Servers: a distributed document retrieval system supported by Apple, Thinking Machines and Dow Jones. Servers answer questions from personal workstations following a standard protocol.")

("WABI" 
 "A software package to emulate Windows under X")

("WAN" 
 "Wide Area Network.")

("Warehouse" 
 "See Data Warehouse.")

("WARIA" 
 "Workflow And Reengineering International Association.")

("Wasserman" 
 "A.I.\(Tony\) Wasserman: president of IDE.")

("Waterfall" 
 "A software life-cycle model showing the phases of the cycle and their interrelations on a characteristic diagram.")

("WE" 
 "A hypertext authoring system developed at the University of North Carolina.")

("Web" 
 "See WWW.")

("WEB" 
 "See Literate Programming and also WorldWide Web")

("Westmount" 
 "A Netherlands software engineering vendor of RTEE and other products.")

("WFMS" 
 "WorkFlow Management System. Software to manage workflow in an organisation.")

("Whetstone" 
 "A benchmark program.")

("Widget" 
 "In the X Window System, a window with its associated input and output functions. Widgets, provided by a library package, are used as building blocks to construct a wide variety of application environments.")

("Willow" 
 "A Motif-based user interface program for bibliographic information retrieval systems, from Washington University.")

("WIMP" 
 "Windows, Icons, Menus and Pointers \(or maybe Windows, Icons, Mouse, Pull-down menus\). The style of user interface made popular by the Apple Macintosh and now available in other GUIs, such as OSF/Motif and NeWS.")

("Window manager" 
 "In a window system, a program which manages windows on a screen. It is responsible for moving and resizing windows, and other practical functions.")

("Window system" 
 "Software which supports windowing. Examples are the X Window System, and proprietary systems on the Macintosh, NeXT and Sun.")

("Windowing" 
 "The ability to interact at will with several processes in a computer through reserved areas, or windows, on a VDU screen.")

("Windows" 
 "A window system and user interface software from Microsoft for MS-DOS.")

("Windows 4GL" 
 "INGRES/Windows 4GL is a graphical tool running on top of workstation native windowing systems, to help developers to build user interfaces to INGRES applications.")

("WISE" 
 "World Wide Information System for Support of R&D Efforts. A project funded by the Commission of the European Communities to encourage \"Transborder Telework and Research Co-operation\".")

("WISE" 
 "Web-Integrated Software metrics Environment. A WWW based software management and metrics system from NASA.")

("WIT" 
 "WWW Interactive Talk.")

("WizDOM" 
 "Software for distributed UNIX system management from TIVOLI Systems of Austin, Texas")

("Word" 
 "A document processing program from Microsoft.")

("Workflow" 
 "The way in which work units \(information or actions\) are routed through an organisation. It can be formalised in terms of rules incorporating dependencies, staff roles etc. and hence automated.")

("Workstation" 
 "A general-purpose computer designed to be used by one person at a time and which offers higher performance than normally found in a PC, especially with respect to graphics, processing power and the ability to carry out several tasks at the same time.")

("WOSC" 
 "World Organisation of Systemics and Cybernetics.")

("WSRD" 
 "Worldwide Software Resources Discovery. An ASSET service.")

("WSL" 
 "Wide Spectrum Language developed for program transformation.")

("WWW" 
 "World-Wide Web: a project originated at CERN, aimed at providing hypertext-style access to information from a wide range of sources.")

("WYSIWYG" 
 "What You See Is What You Get: a feature of document preparation systems allowing the user to work on a document displayed on a screen in exactly the same form as it will appear when printed.")

("X client" 
 "An application process in the X Window System: it gains access to windowing services via the Xlib library. These are translated by the system into messages to an X server.")

("X Consortium" 
 "A vendor consortium supporting development of the X Window System.")

("X-designer" 
 "A user interface builder for Motif from Imperial Software Technology.")

("X protocol" 
 "A standard used by clients \(applications\) and servers in the X Window System for exchanging requests for window manipulations.")

("X server" 
 "A process which controls a bitmap display device.in an X Window System. It performs operations on request from client applications.")

("X terminal" 
 "An intelligent terminal which operates as an X server directly connected to Ethernet.")

("X-terminal" 
 "An intelligent terminal with a built-in implementation of an X server , which can therefore communicate with computers running X clients .")

("X Windows" 
 "See X Window System.")

("X Window System" 
 "A specification for device-independent windowing operations on bitmap display devices, developed by MIT and now a de facto standard supported by the X consortium.")

("X.25" 
 "A standard networking protocol suite approved by the CCITT and ISO. This protocol suite defines standard physical, link, and networking layers \(layers 1 through 3\). X.25 networks are in use throughout the world.")

("X.400" 
 "The set of CCITT communications standards covering mail services provided by data networks.")

("X.500" 
 "The set of CCITT standards covering electronic mail directory services.")

("X.desktop" 
 "A desktop manager for UNIX from IXI.")

("X/Open" 
 "An international consortium of vendors whose purpose is to define the X/Open Common Applications Environment designed to provide applications portability.")

("X11R4" 
 "Version 11 release 4 of the X protocol; the current standard.")

("X11R5" 
 "Version 11 release 5 of the X protocol; the new standard.")

("X3J16" 
 "The C++ standard technical committee.")

("X" 
 "An abbreviation for the X Window System.")

("Xanadu" 
 "An electronic publishing project due to Ted Nelson, the inventor of the term hypertext.")

("Xaw" 
 "The Athena Widget Set: a set of widgets distributed with the X Window System.")

("XDR" 
 "eXternal Data Representation - universal machine independent form of data sent by RPC systems. Described in RFC 1014.")

("XENIX" 
 "UNIX implementations from SCO.")

("Xerox" 
 "The Document Company.")

("Xerox PARC" 
 "The Palo Alto Research Center of the Xerox Corporation.")

("XIE" 
 "X Image Extension: extensions to the X protocol to handle images.")

("Xlib" 
 "X library: program interface to the X Window System.")

("XML" 
 "Xperimental Markup Language based on CML.")

("xmosaic" 
 "See Mosaic")

("XMP" 
 "The X/Open Management Protocols.")

("XNS" 
 "Xerox Network Services: a proprietary networking architecture developed by Xerox.")

("XOM" 
 "The X/Open OSI Abstract Data Manipulation API.")

("Xopen" 
 "See X/Open")

("XPG3" 
 "Version 3 of XPG.")

("XPG" 
 "X/open Portability Guide: defines the interfaces of the X/Open Common Applications Environment.")

("XRemote" 
 "A serial line protocol for the X Window System .")

("XRN" 
 "A newsreader program for Usenet news base on the X Window System. host.")

("XSI" 
 "X/Open System Interface specification: part of the X/Open Common Applications Environment.")

("Xt" 
 "The intrinsics of theX Window System Toolkit.")

("Xterminal" 
 "See X-terminal")

("XTI" 
 "X/open Transport Interface.")

("XUI" 
 "X User Interface: program interface to the X Window System supported by DEC.")

("Xv++" 
 "A library of classes from Interface Engineering, Stevenage, providing a C++ Application Programmer's Interface to the XView toolkit.")

("XView" 
 "A toolkit from Sun, derived from SunView, providing an Open Look user interface for X applications.")

("XVT" 
 "eXtensible Virtual Toolkit: a product allowing applications to be developed independent of GUI.")

("Xwindow" 
 "See X Window System")

("Y++" 
 "An Object-Oriented analysis and design approach.")

("yacc" 
 "Yet Another Compiler Compiler. A parser generator for UNIX by S.C. Johnson.")

("YACL" 
 "Yet Another Class Library.")

("YP" 
 "Yellow Pages: a name server in NFS to link clients desiring a service with servers who can provide it.")

("YSM" 
 "Yourdon Structured Method")

("Z" 
 "A formal specification language developed at Oxford University for describing computing systems, based on set theory and predicate calculus.")

("Z39.50" 
 "Information Retrieval Service Definition and Protocol Specification for Library Applications. Developed by NISO, this standard specifies an OSI application layer service to allow an application on one computer to query a database on another; it is used by WAIS.")

("ZEBRA" 
 "A data management package in the CERN Program Library.")

("ZOG" 
 "A high-performance hypertext system developed at Carnegie-Mellon University.")
)))

;;;test-me: *sting-se-glossary*
;;;test-me:(assoc-string "Data dictionary" *sting-se-glossary*)
;;;test-me:(cadr (assoc-string "Data dictionary" *sting-se-glossary*))
;;;test-me:(cadr (assoc-string "ZEBRA" *sting-se-glossary*))

;;; ==============================
(defvar *sting-se-glossary-terms* 'nil
  "Terms list from alist `*sting-se-glossary*', terms are alist keys.\n
See aslo; `mon-lookup-string-se'.")
(let (sting-se-glossary-terms)
  (setq sting-se-glossary-terms '())
  (mapcar (lambda (x)
            (setq sting-se-glossary-terms (cons (car x) sting-se-glossary-terms)))
          *sting-se-glossary*)
  (setq sting-se-glossary-terms (reverse sting-se-glossary-terms))
  (when (not (bound-and-true-p *sting-se-glossary-terms*))
    ;; (prin1 sting-se-glossary-terms (current-buffer)))
    (setq *sting-se-glossary-terms* sting-se-glossary-terms)))


;;;(progn (makunbound '*sting-se-glossary-terms*) (unintern '*sting-se-glossary-terms*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-09T17:44:27-04:00Z}#{09327} - by MON KEY>
(defun mon-lookup-string-se (&optional sting-term insertp intrp)
  "Lookup a sting-se-glossary term.
When STING-TERM is non-nil attempt to associate with a val.
When insertp is non-nil or when called-interactively with prefix arg
insert STING glossary value at point.\n
See also; `*sting-se-glossary*', `*sting-se-glossary-terms*'."
(interactive "i\nP\np")
 (let ((term (cond (sting-term 
                    (let ((term-val (assoc sting-term *sting-se-glossary*)))
                      (if  term-val 
                          (cadr term-val)
                        (message "%s not in glossary." sting-term))))
                   ;;Supply a subr for a failed key lookup of STRING-TERM???
                   ;; (cadr (assoc
                   ;;        (completing-read 
                   ;;         (format "%s not in glossary. Which term shall we look up instad? :" sting-term)
                   ;;         *sting-se-glossary-terms*) *sting-se-glossary*))
                   (intrp  (cadr (assoc 
                                  (completing-read "which term shall we look up:" *sting-se-glossary-terms*)
                           *sting-se-glossary*)))
                   ((and (not intrp) (not sting-term))
                    (error "Must provide a term to associate")))))
   (when (and intrp (not insertp)) (message "%s" term))
   (if insertp (princ term (current-buffer)) term)))

;;;test-me;(mon-lookup-string-se "Document Examiner")
;;;test-me;(mon-lookup-string-se "PECL" t)
;;;test-me;(mon-lookup-string-se nil t t)
;;;test-me;(call-interactively 'mon-lookup-string-se)
;;;test-me;(mon-lookup-string-se) ;simple error

;;;(progn (makunbound '*sting-se-glossary*) (unintern '*sting-se-glossary*))

;;; ==============================
(provide 'STING-software-engineering-glossary)
;;; ==============================

;;; ==============================
;;; NON-ALIST-VERSION
;;; ==============================
;; 4GL
;;     Fourth generation language. 
;; 88open
;;     A consortium with the aim of creating a multivendor open computing environment based on the Motorola 88000 RISC processor family - More information. 
;; AAP DTD
;;     A DTD for a standard SGML document type for scientific documents, defined by the AAP - More information. 
;; AAP
;;     The Association of American Publishers: engaged in standardisation efforts in document preparation. 
;; ABI
;;     Application Binary Interface: the interface by which an application program gains access to operating system and other services, designed to permit porting of compiled binary applications between systems with the same ABI. 
;; Abstract Class
;;     In object-oriented programming, a class designed only as a parent from which sub-classes may be derived, but which is not itself suitable for instantiation. Often used to "abstract out" incomplete sets of features which may then be shared by a group of sibling sub-classes which add different variations of the missing pieces. 
;; ACA
;;     Application Control Architecture: DEC's implementation of ORB 
;; ACE
;;     Advanced Computing Environment: a consortium to agree on an open architecture based on the MIPS R4000 chip. A computer architecture ARCS will be defined, on which either OS/2 or Open Desktop can be run - More information. 
;; ACE
;;     Adaptive Communication Environment, a C++ Wrapper Library for communications from the University of California at Irvine - More information. 
;; ACM
;;     Association for Computing Machinery - More information. 
;; Acrobat
;;     A platform-independent text and image formatter/viewer from Adobe Systems - More information. 
;; Actis
;;     An approach to integrated CASE by Apollo. 
;; Active object
;;     An object that encompasses its own thread of control. 
;; Active DBMS
;;     A conventional or passive DBMS combined with a means of event detection and condition monitoring. Event handling is often rule-based, as with an expert system. 
;; ActiveX
;;     A software development kit from Microsoft for develpment of Internet applications and content - More information. 
;; Actor
;;     In object-oriented programming, an object which exists as a concurrent process. 
;; Actor
;;     A term in Chorus denoting the unit of resource allocation. 
;; Actra
;;     A multiprocessor Smalltalk project. 
;; AD/Cycle
;;     (AD = Application Development): a set of SAA-compatible IBM-sponsored products for program development, running on workstations accessing a central repository on a mainframe. The stages cover requirements, analysis and design,production of the application, building and testing, and maintenance. Technologies used include code generators and knowledge based systems, as well as languages and debuggers. 
;; Ada
;;     A high-level computer language sponsored by the US Department of Defense. It has a multitasking mechanism, and a number of features useful for software engineering - More information. 
;; AdaIC
;;     Ada Information Clearinghouse - More information. 
;; Adaline
;;     Name given by Widrow to ADAptive LInear NEurons, that is neurons (seeMcCulloch-Pitts) which learn using the Widrow-Huff Delta Rule (see also Madaline). 
;; ADAMO
;;     A data management system written at CERN based on the Entity-Relationship model - More information. 
;; Adaptable User Interface
;;     A toolkit from Oracle allowing applications to be written portably for different windowing systems. It provides one call level interface along with a resource manager and editor across a range of "standard" GUIs, including Macintosh, Windows and the X Window System. 
;; Adaptive learning
;;     Learning in which a system programs itself by adjusting weights or strengths until it produces the desired output. Same as Hebbian. 
;; ADDD
;;     A Depository of Development Documents. A public domain Software Engineering Environment from GMD developed as part of the STONE project - More information. 
;; ADL
;;     Assertion (or API) Definition Language. A project for Automatic Interface Test Generation - More information. 
;; ADT
;;     Abstract Data Type: a class of data structures described by means of a set of operations rather than by physical representation, such as a class in object-oriented programming.. 
;; Aegis
;;     A CASE tool for project change management, part of the GNU software. 
;; AENOR
;;     Asociacion Espanola de Normalizacion y Certificacion. The Spanish standards organisation. 
;; AEP
;;     Application environment profile . 
;; AES
;;     Application environment specification: a set of specifications from OSF for programming and user interfaces, aimed at providing a consistent application environment on different hardware platforms. It includes O/S for the operating system (user commands and program interfaces), U/E for the User Environment (Motif), and N/S for Network services. 
;; AFIPS
;;     American Federation of Information Processing Societies. 
;; AFNOR
;;     Association Francaise pour la Normalisation: the French national standards institute, a member of ISO. 
;; AFS
;;     Andrew File System . 
;; AGOCG
;;     Advisory Group on Computer Graphics. Advising UK Higher Education on Computer Graphics, Visualization and Multimedia - More information. 
;; AGL
;;     Atelier de Genie Logiciel: French for IPSE. 
;; AI
;;     Artificial Intelligence . 
;; AIA
;;     Application Integration Architecture: DEC's "open standards" specifications. 
;; AICA
;;     Associazione Italiana di Calcolo Automatico. 
;; AIFF
;;     Audio IFF. A format developed by Apple for storing high-quality sampled sound and musical instrument info; also used by SGI and several professional audio packages. 
;; AIS
;;     Advanced Informatics Support project for administrative work at CERN - More information. 
;; AIX
;;     Advanced Interactive eXecutive: IBM's version of UNIX, taken as the basis for the OSF standard - More information. 
;; Algol
;;     A high-level programming language developed in the 1950s . 
;; Algorithm
;;     A systematic procedure guaranteed to produce a result after a finite number of steps. 
;; Alvey
;;     A funding programme for collaborative research in the UK. 
;; ami
;;     Applications of Metrics in Industry (Assess, Analyze, Metricate, Improve). A method for software project management and process improvement - More information. 
;; Amoeba
;;     A distributed operating system developed by A.Tanenbaum and others at Amsterdam - More information. 
;; AMS
;;     Andrew Message System. 
;; AMADEUS
;;     A PC client for Hyper-G. 
;; Analysis
;;     The part of the software development process concerned with defining the requirements for the product. 
;; ANDF
;;     Architecture-Neutral Distribution Format: an emerging OSF standard for software distribution. Programs are compiled into ANDF before distribution, and executables are produced from it for the local target system. 
;; Andrew File System
;;     The distributed file system of the Andrew project, adopted by the OSF as part of their DCE. 
;; Andrew Message System
;;     A multimedia interface to electronic mail and bulletin boards, developed as part of the Andrew project 
;; Andrew Project
;;     A distributed system project for support of educational and research computing at Carnegie Mellon University - More information. 
;; Andrew Toolkit
;;     A portable user interface toolkit developed as part of the Andrew project, running on the X Window System and distributed with X11R5. 
;; ANL
;;     Argonne National Laboratory, USA - More information. 
;; Anna
;;     A specification language from Stanford University for formally specifying Ada programs. It has a Specification Analyzer and a Consistency Checking System. 
;; Annealing
;;     A technique which can be applied to any minimization or learning process based on successive update steps (either random or deterministic) where the update step length is proportional to an arbitrarily set parameter which can play the role of a temperature. Then, in analogy with the annealing of metals, the temperature is made high in the early stages of the process for faster minimization or learning, then is reduced for greater stability. 
;; ANSA
;;     Advanced Network Systems Architecture: an architecture for distributed computer systems based on a model developed as an Esprit project - More information. 
;; ANSI Z39.50
;;     See Z39.50. 
;; ANSI/SPARC Architecture
;;     A layered model of database architecture comprising a physical schema, a conceptual schema, and user views. 
;; ANSI
;;     American National Standards Institute, responsible for approving U.S. standards in many areas, including computers and communications. ANSI is a member of ISO - More information. 
;; AnswerGarden
;;     A help desk software package from MIT. 
;; AOCE
;;     Apple Open Collaboration Environment. A set of software for e-mail, directory services etc. 
;; APA
;;     Application Portability Architecture: DEC's plan for portable applications software. 
;; apE
;;     A graphics package from the Ohio Supercomputer Centre . 
;; Apertos
;;     An object-oriented operating system from Sony Computer Science Laboratory - More information. 
;; API
;;     Application Program Interface: a term for the interface by which an application program gains access to operating system and other services, defined at source-code level. 
;; APL
;;     A Programming Language developed by Iverson for mathematical applications. 
;; Apollo
;;     Apollo Computer, now a division of Hewlett-Packard, also the name of a range of workstations manufactured by this company. 
;; AppKit
;;     A set of objects used by the application builder for the NeXTstep environment. 
;; Applet
;;     A small application, often downloaded from a remote server and run in a controlled environment. Typically written in a language such as Java for execution by a WWW browser. 
;; Apple
;;     Apple Computer Inc, manufacturers of the Macintosh range of Personal Computers. 
;; Appletalk
;;     The proprietary local area network protocol developed by Apple for their Macintosh range of processors. Current implementations exist on Localtalk and Ethertalk. 
;; APSE
;;     Ada Programming Support Environment. 
;; ARC
;;     (Previously ARCS) Advanced RISC Computing Specification: the standard hardware architecture of ACE., specifying the baseline hardware requirements to create ACE-compatible systems. 
;; Arcadia
;;     A software engineering research project by a consortium of US universities - More information. 
;; Archie
;;     An archive server database and query system operated by the McGill University School of Computer Science. Services remote requests for information on software kept on archives worldwide and available via ftp - More information. 
;; ARCS
;;     see ARC. 
;; Arjuna
;;     A system for reliable distributed computing from the Computing Laboratory, University of Newcastle upon Tyne. It supports atomic transactions on persistent objects. 
;; ARL
;;     ASSET Reuse Library. 
;; ARL
;;     Association of Research Libraries (North America) - More information. 
;; ARPANET
;;     U.S. Department of Defense (DARPA) wide area network. It became operational in 1968 and was the forerunner of the Internet. 
;; Artifex
;;     A CASE environment from ARTIS of Turin for the development of large event-driven distributed systems. It has code-generation and rapid prototyping features - More information. 
;; Artificial Intelligence
;;     The subfield of computer science concerned with the concepts and methods of symbolic inference by computer, and the symbolic representation of the knowledge to be used in making inferences - More information. 
;; ASCII
;;     American Standard Code for Information Interchange. 
;; ASDL
;;     Abstract-Type and Scheme-Definition Language: developed as part of Esprit project GRASPIN, as a basis for generating language-based editors and environments. It combines an object-oriented type system, syntax-directed translation schemes and a target-language interface. 
;; ASE
;;     Advanced Software Environment: an object-oriented application support system from Nixdorf. 
;; ASIC
;;     Application-Specific Integrated Circuit: an integrated circuit designed to perform a particular function by defining the interconnection of a set of basic circuit building blocks drawn from a library provided by the circuit manufacturer. 
;; ASIS
;;     Ada Semantic Interface Specification. An interface between an Ada library and any tool requiring information in it - More information. 
;; ASIS
;;     Application Software Installation Server at CERN - More information. 
;; ASME
;;     American Society of Mechanical Engineers: involved in CAD standardisation. 
;; ASN.1
;;     Abstract Syntax Notation 1: an ISO/CCITT standard for the description of data. It is intended to facilitate the exchange of data between application programs. 
;; ASPECT
;;     An IPSE developed by an Alvey project, using Z to specify the object-management system and tool interface. 
;; ASQ
;;     Automated Software Quality. The use of software tools, such as automated testing tools, to improve software quality. 
;; ASQC
;;     American Society for Quality Control. 
;; ASSET
;;     Asset Source for Software Engineering Technology. A programme to promote software reuse by the DoD - More information. 
;; AtFS
;;     Attributed File System: the basis of the Shape_VC toolkit. Cooperative work within projects is supported by a status model controlling visibility of version objects, locking, and "long transactions" for synchronizing concurrent updates. The concept of object attributes provides a basis for storing management information with versions and passing this information between individual tools. This mechanism is useful for building integrated environments from a set of unrelated tools. 
;; Athena
;;     Project Athena: a distributed system project for support of educational and research computing at MIT. Much of the software developed is now in wider use, especially the X Window System - More information. 
;; Atherton
;;     Atherton Technology developed the Software BackPlane CASE framework. Their Atherton Tool Integration Services were the basis for the ATIS standard. 
;; ATIS
;;     A Tools Integration Standard: an object-oriented interface to a set of services that allows the saving, accessing, and managing of information in a common repository. Developed by Atherton Technology and DEC, based on an extended version of the Software BackPlane, now proposed as an industry standard. 
;; ATK
;;     The Andrew Toolkit 
;; ATM
;;     Asynchronous Transfer Mode. A transmission system for telecommunications - More information. 
;; ATM
;;     Adobe Type Manager. 
;; AUE
;;     Andrew User Environment. Part of the Andrew project 
;; AUI
;;     Adaptable User Interface from Oracle. 
;; AUIS
;;     Andrew user Interface System - More information. 
;; AutoCAD
;;     A CAD software package for mechanical engineering marketed by Autodesk Inc. 
;; AVL
;;     Abstract Visualization Language in the Tecate project. 
;; AVS
;;     Application Visualisation System: a portable modular UNIX-based graphics package supported by a consortium of vendors including Convex, DEC, IBM, HP, SET Technologies, Stardent and WaveTracer - More information. 
;; AWK
;;     A pattern scanning and processing language named after its authors: Aho, Weinberger and Kernighan. 
;; aXe
;;     A text editor for the X-Window-System. 
;; B
;;     A Formal method of program design - More information. 
;; Bachman
;;     Proposed a style of Entity-Relationship modeling which differs from the original Chen proposals. 
;; Back-propagation
;;     An important algorithm for learning in feed-forward networks which makes use of a mathematical trick when the network is simulated on a digital computer, yielding in just two traversals of the network (once forward, and once back) both the difference between the desired and actual output, and the derivatives of this difference with respect to the connection weights. 
;; Backus Naur
;;     A formal language for syntax specification. 
;; Bamboo
;;     A trusted third-party authentication system from the University of Iowa, similar to Kerberos - More information. 
;; Baseline
;;     See Released version. 
;; BASIC
;;     Beginners All-purpose Symbolic Instruction Code: a programming language, usually interpreted, suitable for simple applications. 
;; BBN
;;     Bolt Beranek and Newman Inc.,of Cambridge, Massachusetts, was awarded the original contract to build the ARPANET and has been extensively involved in Internet development. It is responsible for managing NNSC, CSNET, and NEARnet. 
;; BCS
;;     Binary Compatibility Standard: the ABI of 88open. 
;; BCS
;;     British Computer Society. 
;; BEA
;;     Basic programming Environment for interactive-graphical Applications, from Siemens-Nixdorf. 
;; Bedrock
;;     A C++ class library for Macintosh user interface portability. 
;; Benchmark
;;     A standard set of programs which can be run on different platforms to compare performance - More information. 
;; Bento
;;     A multi-vendor initiative allowing files to contain typed parts, to allow standard access between parts of a compound document independent of the file system. 
;; Berkeley UNIX
;;     see BSD. 
;; BETA
;;     An object-oriented language and associated programming environment from Mjolner Informatics, Aarhus - More information. 
;; BHT
;;     Budget Holder's Toolkit (at CERN) - More information. 
;; BITNET
;;     An academic and research network connecting approximately 2500 computers, often IBM mainframes. It provides interactive electronic mail, and file transfer services via a store-and-forward technique based on IBM NJE protocols. BITNET and Internet traffic are exchanged via several gateway hosts. It is now operated by CREN. 
;; BMP
;;     Bitmap format (for Windows) - More information. 
;; BNF
;;     Backus-Naur Form. 
;; BOCS
;;     Berard Object and Class Specifier, an Object-oriented CASE tool from Berard Software Engineering. 
;; Boehm B.
;;     Proposed the COCOMO technique for evaluating the cost of a software project. 
;; BoM
;;     Bill of Materials. 
;; BON
;;     Better Object Notation. Used in the Esprit Business Classes project - More information. 
;; Bookreader
;;     DEC's CD-ROM-based online documentation browser. 
;; Bookviewer
;;     A hypertext documentation system from Oracle based on Oracle Toolkit. It allows the user to create private links and bookmarks, and to make multimedia annotations. 
;; BOOM
;;     Berard Object-Oriented Methodology - More information. 
;; BOS
;;     A data management system written at DESY and used in some HEP programs. 
;; Bourne shell
;;     A common UNIX shell - More information. 
;; BPM
;;     Business Process Modelling. 
;; BPR
;;     Business Process Reengineering. 
;; Browser
;;     A tool for navigating around hypertext documents. 
;; BSD
;;     Berkeley Source Distribution: the versions of UNIX developed and distributed by the University of California at Berkeley. Many commercial UNIX implementations such as SunOS and Dynix are derived from it. 
;; BSI
;;     British Standards Institution: a member of ISO. 
;; BSP method
;;     A CASE method from IBM . 
;; Byte
;;     A data unit of several bits smaller than a computer word: usually 8 bits. 
;; C++
;;     An extension to the C language developed primarily by B.Stroustrup at AT&T Bell Laboratories: it supports object-oriented programming among other enhancements - More information. 
;; C Beautifier
;;     A tool for tidying the syntax of C source code. 
;; c shell
;;     A common UNIX shell originating on Berkeley UNIX - More information. 
;; C
;;     A language developed in conjunction with the UNIX operating system at AT&T Bell Laboratories by D.Ritchie and now an ANSI standard. It has grown popular due to its simplicity, efficiency, and flexibility. C programs are often easily adapted to new environments - More information. 
;; Cache
;;     A small fast memory holding recently-accessed data, designed to speed up further access. 
;; CACI
;;     A company marketing SIMSCRIPT, MODSIM, and other simulation software products. 
;; CACM
;;     Communications of the ACM. 
;; CAD/CAM
;;     Computer Aided Design/Computer Aided Manufacturing (see CAD) 
;; CAD
;;     Computer Aided Design: usually applied to that part of CAE which has to do with the drawing or physical layout steps of engineering design. 
;; CADD
;;     Computer Aided Detector Design: a project to develop standards and methods to allow cooperation between HEP detector designers working in different institutes - More information. 
;; CADRE
;;     A software engineering vendor in the US. 
;; CAE
;;     Common Applications Environment of X/Open, based on POSIX and C. 
;; CAE
;;     Computer Aided Engineering: a technique for using computers to help with all phases of engineering design work. As CAD, but also involving the conceptual and analytical design steps. 
;; CAI
;;     Computer Aided Instruction. 
;; CAIS-A
;;     Common APSE Interface Set: DoD-STD-1838A. 
;; CAIS
;;     Common APSE Interface Specification. 
;; CAiSE
;;     Conference on Advanced Information Systems Engineering. 
;; CAJUN
;;     CD-ROM Acrobat Journals Using Networks. A project at Nottigham University More information. 
;; CALS
;;     Computer-Aided Acquisition and Logistics Support: a DoD standard for electronic exchange of data with commercial suppliers - More information. 
;; Caml
;;     A functional programming language in the style of ML - More information. 
;; CApH
;;     Conventions for the Application of HyTime. An activity of the GCA 
;; CAQ
;;     Computer Aided Quality. 
;; CARDS
;;     Central Archive for Reusable Defense Software of the DoD . 
;; CASE*Method
;;     An analysis and design method from Oracle, targeted at information management applications. 
;; CASE framework
;;     A set of products and conventions that allow CASE tools to be integrated into a coherent environment. 
;; CASE tools
;;     Software tools to help in the application of CASE methods to a software project. 
;; CASE
;;     Computer Aided Software Engineering: a technique for using computers to help with the systematic analysis, design, implementation and maintenance of software. Adopting the CASE approach to building and maintaining systems involves software tools and training for the developers who will use them. 
;; CAST
;;     Computer Aided Software Testing. 
;; CATE
;;     Computer Aided Test Engineering: CASE methods applied to electronics testing and linked to CAE 
;; CAUSE
;;     An international (mainly North American) nonprofit association for managing and using information technology in higher education - More information. 
;; cb
;;     C Beautifier. 
;; CBT
;;     Computer-Based Training. 
;; CCI
;;     Common Client Interface for Mosaic 
;; CCITT
;;     A committee of the ITU responsible for making technical recommendations about telephone and data communication systems for PTTs and suppliers. Plenary sessions are held every four years to adopt new standards. 
;; CCL
;;     Common Command Language. A standard for bibliographic information retrieval systems. 
;; CCS
;;     Common Communication Services: the standard program interface to networks in SAA. 
;; CDA
;;     Compound Document Architecture: DEC's set of standards for compound document creation, storage, retrieval, interchange and manipulation. 
;; CDC
;;     Control Data Corporation 
;; CDD/Plus
;;     DEC's CASE repository. 
;; CDE
;;     C Development environment from IDE 
;; CDE
;;     Common Desktop Environment. A Desktop manager from COSE - More information. 
;; CDF
;;     Common Data Format. A library and toolkit for multi-dimensional data sets - More information. 
;; CDM
;;     Content Data Model. An SGML-based DoD specification for interactive manuals. 
;; CDIF
;;     CASE Data Interchange Format: an emerging standard.for interchange of data between CASE tools - More information. 
;; CE
;;     Concurrent Engineering. 
;; CEBAF
;;     Continuous Electron Beam Facility in Newport News, VA USA - More information. 
;; Cecil
;;     An object-oriented language from Washington University intended to support rapid construction of high-quality, extensible software - More information. 
;; CEN
;;     Conseil Europeen pour la Normalisation: a body coordinating standardisation activities in the EEC and EFTA. countries. 
;; CERA
;;     Concurrent Engineering: Research and Applications. An international journal - More information. 
;; CERC
;;     Concurrent Engineering Research Center, West Virginia University - More information. 
;; CERN
;;     The European Laboratory for Particle Physics. 
;; CERNLIB
;;     The CERN Program Library - More information. 
;; CERT
;;     Computer Emergency Response Team. Now CERT Coordination Center, works with the Internet community on security problems - More information. 
;; CENELEC
;;     CEN-electricite. 
;; CFI
;;     CAD Framework Initiative. A consortium working on interface standards for integrating CAD tools and data. 
;; CFOOT
;;     Corporate Facilitators of Object-Oriented Technology. 
;; CGI
;;     Common Gateway Interface. A standard for running external programs under a WWW or similar information server - More information. 
;; CGI
;;     A (French) software engineering vendor in the US. 
;; CGM
;;     Computer Graphics Metafile: a standard file format for storage and communication of graphical information, widely used on personal computers and accepted by desktop publishing systems. (ANSI/ISO 8632-1987) - More information. 
;; Change Management
;;     A consistent set of techniques that aid in evolution, composition and policy management of the design and implementation of an object or system. 
;; Charm
;;     A portable object-oriented parallel programming system from University of Illinois - More information. 
;; Chen
;;     Peter Chen developed the Entity-Relationship model. 
;; CHEOPS
;;     A satellite-based batch data dissemination project between CERN and member state institutes. 
;; Child version
;;     A version of a version. See change management. 
;; CHILL
;;     CCITT High-Level Language. A real-time language used in telecommunications. 
;; Choices
;;     An object-oriented operating system from University of Illinois - More information. 
;; Chorus
;;     A distributed operating system developed at INRIA. 
;; CIAC
;;     Computer Incident Advisory Capability of the US DoE - More information. 
;; CIC
;;     Committee on Institutional Cooperation. An academic consortium of American Universities - More information. 
;; CICERO
;;     Control Information system Concepts based on Encapsulated Real-time Objects. A CERN DRDC proposal. 
;; CIDR
;;     Classless Inter-Domain Routing (on the Internet) 
;; CIL
;;     Component Integration Laboratories. An effort to create a common framework for interoperability between applications on desktop platforms, formed by Apple, IBM, Novell, Oracle, Taligent, WordPerfect, and Xerox - More information. 
;; CIM
;;     Computer Integrated Manufacturing. 
;; CIS
;;     Case Integration Services: a committee formed to discuss CASE tool integration standards related to ATIS. 
;; CISC
;;     Complex Instruction Set Computer. 
;; CISI
;;     A French software house. 
;; CIX
;;     Commercial Internet eXchange. A non-profit trade association of Public Data Internetwork service providers - More information. 
;; CL
;;     See Common Lisp. 
;; Class-Relation Method
;;     A design technique based on the concepts of object-oriented programming and the Entity-Relationship model from the French company Softeam. 
;; Class
;;     A language developed by the Andrew Project: one of the first attempts to add object-oriented features to C. 
;; Class
;;     The prototype for an object in an object-oriented language; analogous to a derived type in a procedural language. 
;; Class library
;;     A library of reusable classes for use with an object-oriented programming system - More information. 
;; Cleanroom
;;     A software development approach aimed at producing software with the minimum number of errors - More information. 
;; Client
;;     A system or process that requests a service from another system or process. 
;; CLHEP
;;     A C++ class library for high energy physics applications - More information. 
;; CLOS
;;     Common Lisp Object System: an object-oriented language derived from Common Lisp - More information. 
;; CLP
;;     Constraint Logic Programming. 
;; CLU
;;     An object-oriented programming language developed at MIT by Liskov et al. 
;; CLX
;;     The Common Lisp interface to the X Window System, equivalent to Xlib. 
;; CM
;;     Configuration Management. 
;; CMA
;;     Concert Multithread Architecture from DEC . 
;; CML
;;     Chemical Markup Language. A means for interchanging chemical information, based on SGML - More information. 
;; CMM
;;     Capability Maturity Model for software development organisations, from SEI - More information. 
;; CMS
;;     A code management system from DEC. 
;; CMVC
;;     Configuration Management Version Control from IBM. 
;; CMZ
;;     A portable interactive code management system from CodeME S.A.R.L in use in the high-energy physics community. 
;; CNET
;;     Centre national d'Etudes des Telecommunications: the French national telecommunications research centre at Lannion. 
;; CNI
;;     Coalition for Networked Information. Promotes the creation of and access to information resources in networked environments in order to enrich scholarship and enhance intellectual productivity - More information. 
;; CNRI
;;     Corporation for National Research Initiatives, Reston, VA. A US research and development organisation in information processing technology - More information. 
;; COBOL
;;     COmmon Business Oriented Language: an early and widely-used programming language for business applications. 
;; COCOMO
;;     Constructive Cost Model: a method for evaluating the cost of a software package proposed by B.Boehm, "Software Engineering Economics" Prentice-Hall 1987 - More information. 
;; CODA
;;     An object-oriented data-acquisition system at CEBAF - More information. 
;; Codd's First Normal Form
;;     see Normal Form. 
;; Code Management
;;     A source code management system helps program developers keep track of version history, releases, parallel versions etc. There are several in popular use - More information. 
;; CodeCenter
;;     A proprietary software development environment for C programs, offering an integrated toolkit for developing, testing, debugging and maintainance (formerly Saber-C) 
;; Cognitech
;;     A French software house specialising in Artificial Intelligence. 
;; COHESION
;;     DEC's CASE environment. 
;; Collage
;;     A synchronous collaborative data analysis tool for use over the Internet, from NCSA - More information. 
;; COM
;;     Common Object Model. An open architecture from DEC and Microsoft, allowing interoperation between ObjectBroker and OLE - More information. 
;; COMIS
;;     a COMpilation and Interpretation System. A FORTRAN interpreter use by the PAW system - More information. 
;; COMMA
;;     Common Object-oriented Methodology Metamodel Architecture from OPEN - More information. 
;; Common Lisp
;;     An ANSI standard version of Lisp. 
;; COMNET
;;     A simulation tool from CACI for analysing wide-area voice or data networks, based on SIMSCRIPT.. 
;; Compaq
;;     A US manufacturer of IBM PC-compatibles. 
;; Compression
;;     Data files are often compressed to take up less network bandwidth, memory etc. Common examples are program executables and visual images. Many algorithms and utilities exist for this - More information. 
;; COMSOFT
;;     Consortium for the Management of Emerging Software Technologies - More information. 
;; Concrete Class
;;     In object-oriented programming, a class suitable to be instantiated.(as opposed to an abstract class). 
;; Concurrent Clean
;;     A functional language for the Macintosh from the University of Nijmegen. 
;; Concurrent Engineering
;;     An approach where all aspects of a product's life-cycle are considered as early as possible in the design, manufacturing and maintenance process - More information. 
;; Configuration management
;;     The process of identifying, defining, recording and reporting the configuration items in a system and the change requests. Controlling the releases and change of the items throughout the life-cycle See also code management - More information. 
;; Constructor
;;     A function provided by a class in C++ to instantiate an object. 
;; Container class
;;     A class whose instances are collections of other objects. Examples include stacks, queues, lists and arrays. 
;; CooL
;;     Combined object-oriented Language from the ITHACA Esprit project, which combines C-based languages with database technology. 
;; COOL
;;     A class library for C++ from Texas Instruments - More information. 
;; COOTS
;;     Conference on Object-Oriented Technologies and Systems. 
;; CORBA
;;     Common Object Request Broker Architecture: an OMG specification - More information. 
;; CORDIS
;;     The European Community R&D information service - More information. 
;; CORE
;;     Chemistry Online Retrieval Experiment. A project to publish American Chemical Society journals electronically. 
;; Cortex
;;     An experimental slow controls project at CERN - More information. 
;; COS
;;     Corporation for Open Systems: an international consortium of computer users and vendors, set up to provide ways of testing OSI implementations. 
;; COSE
;;     Common Open Software Environment. An initiative by Hewlett-Packard, Sun, IBM, Novell, Univel and SCO to move towards consistency and interopability between Unix suppliers. 
;; COSS
;;     Common Object Services Specification in CORBA. 
;; COSINE
;;     Cooperation for Open Systems Interconnection Networking in Europe. A EUREKA project. 
;; CoST
;;     A set of software tools for SGML documents. 
;; COTS
;;     Commercial Off The Shelf solution - More information. 
;; CPAN
;;     Comprehensive Perl Archive Network - More information. 
;; CPI
;;     Common Program Interface: the API of SAA. 
;; CPSR
;;     Computer Professionals for Social Responsibility. A US non-profit organisation concerned with the effects of computers on society - More information. 
;; CPU
;;     Central Processing Unit, usually applied to that part of a computer which carries out the arithmetic and controls the instruction flow. 
;; CRAY
;;     Cray Research Inc.: manufacturers of a range of large powerful mainframes. 
;; CRC
;;     Class-Responsibility-Collaboration. A technique described in Object-Oriented Software by Wirfs-Brock. 
;; CREASE
;;     Catalog of Resources for Education in Ada and Software Engineering. A database maintained by AdaIC. 
;; CREN
;;     Corporation for Research and Educational Networking: responsible for providing networking service to BITNET and CSNET users - More information. 
;; cron
;;     The clock daemon in UNIX that executes commands at specified dates and times according to instructions in a file - More information. 
;; Cross software
;;     Software developed on one kind of computer for use on another (usually because the other computer does not have itself adequate facilities for software development). 
;; CRS4
;;     Centro di Ricerca, Sviluppo e Studi Superiori in Sardegna. ( Center for Advanced Studies, Research and Development in Sardinia). A high performance computing centre with an interesting information server. 
;; CSCW
;;     Computer Supported Cooperative Work (also known as Groupware): software tools and technology to support groups of people working together on a project, often at different sites - More information. 
;; csh
;;     See c shell 
;; cshell
;;     See c shell 
;; CSL
;;     Caml Special Light. An implementation of Caml - More information. 
;; CSMA/CD
;;     Carrier Sense Multiple Access with Collision Detection: a network arbitration scheme used on Ethernet. A station with a message to send starts sending if there is no carrier detected on the transmission medium. If a collision occurs, transmission is abandoned and retried after a delay. 
;; CSNET
;;     Computers and Science Network, operated by CREN for US computer science institutes. It provides electronic mail service via dial-up lines, plus X.25 and Internet services. 
;; CSP
;;     Communicating Sequential Processes. A programming model developed by T. Hoare at Oxford University - More information. 
;; CSS
;;     Cascading Style Sheets. A simple mechanism for adding style to WWW documents - More information. 
;; CSTC
;;     Computer Security Technology Center of the US DoE - More information. 
;; CTAN
;;     Comprehensive TeX Archive Network - More information. 
;; CTI
;;     Computer Telephony Integration. 
;; CUA
;;     Common User Access: the User Interface standard of SAA. 
;; curses
;;     A set of subroutines in UNIX for handling navigation on a terminal screen using the cursor - More information. 
;; CVS
;;     A code management system based on RCS - More information. 
;; CWI
;;     Dutch Centre for Mathematics and Computer Science, Amsterdam - More information. 
;; CWIS
;;     Campus-Wide Information System. Many universities and other institutes have computerised information systems, often based on WWW or gopher 
;; DAA
;;     Distributed Application Architecture: under design by Hewlett-Packard and Sun. A distributed object management environment that will allow applications to be developed independent of operating system, network or windowing system. 
;; DACNOS
;;     A prototype network operating system for multivendor environments, from IBM European Networking Centre Heidelberg and University of Karlsruhe. 
;; DAD
;;     Distributed Adamo Database. An extension to ADAMO - More information. 
;; daemon
;;     A process running in the background performing some service (such as handling print queues) in UNIX or other operating systems. 
;; DANTE
;;     A company established by the national research networks in Europe to provide international network services - More information. 
;; DARPA
;;     Defense Advanced Research Project Agency of the US Department of Defense,.responsible for the development of new technology, including ARPANET. 
;; DASE
;;     Distributed Application Support Environment . 
;; Data base
;;     See DBMS 
;; Data Definition Language
;;     A language enabling the structure and instances of a database to be defined in a human- and machine-readable form. 
;; Data dictionary
;;     A set of data descriptions that can be shared by several applications. 
;; Data Flow Diagram
;;     A graphical notation used to describe how data flows between processes in a system. An important tool of most structured analysis techniques. 
;; Data Model
;;     A set of data structures with manipulation and validation operators for general purpose usage. Examples are the Entity-Relationship model and NIAM 
;; Data Warehouse
;;     A database of information intended for use as part of a decision support system. The data is typically extracted from an organisation's operational databases. 
;; Database
;;     See DBMS. 
;; Datacom
;;     A DBMS from Computer Associates International.. 
;; DATATRIEVE
;;     A query and report system for use with DEC's VMS system (RMS, VAX Rdb/VMS or VAX DBMS). 
;; DataViews
;;     Graphical user interface development software from V.I.Corporation, aimed at constructing platform-independent interactive views of dynamic data. 
;; DAZIX
;;     Daisy/Cadnetix Corporation: a supplier of digital electronic CAE systems. 
;; DB2
;;     A DBMS from IBM. 
;; DB
;;     Database. 
;; DBA
;;     DataBase Administrator. 
;; dBASE III
;;     A DBMS from Ashton-Tate Corporation. 
;; DBMS
;;     Database management system: such systems typically manage large structured sets of persistent data, offering ad hoc query facilities to many users. They are widely used in business applications: commercial examples include Ingres, Oracle, Sybase etc. 
;; DCA
;;     Document Content Architecture.from IBM 
;; DCE
;;     Distributed Computing Environment from OSF - More information. 
;; DCF
;;     Document Composition Facility. 
;; DCOM
;;     Distributed Component Object Model Protocol - More information. 
;; DCSA
;;     Distributed Component Software Architecture - More information. 
;; DD
;;     Data Dictionary. 
;; DDE Manager
;;     An Oracle product that lets Windows applications that support the DDE protocol act as front end tools for Oracle. It allows applications like Excel, Word, Ami Professional, WingZ, and ToolBook to query, update, graph, and report information stored in Oracle. 
;; DDE protocol
;;     Dynamic Data Exchange: a Microsoft protocol that allows Windows applications to communicate using a client/server model. 
;; DDIF
;;     Digital Document Interchange Format. A CDA specification for representing compound documents in revisable format; a DEC standard for document encoding. 
;; DDL
;;     Data definition language. 
;; DDL
;;     Document Description Language. 
;; DDTS
;;     Distributed Defect Tracking System. 
;; DEC
;;     Digital Equipment Corporation: a computer manufacturer and software vendor. 
;; DECdesign
;;     A software analysis and design tool from DEC supporting several methodologies. 
;; DECdns
;;     Distributed Naming Service: adopted by OSF as the naming service for DCE. 
;; DECnet
;;     The network marketed by DEC to connect its computers together. 
;; DECstation
;;     A range of RISC based workstations manufactured by DEC. 
;; DECwindows
;;     DEC's windowing environment based on the X Window System. 
;; DECwrite
;;     DEC's CDA-based, WYSIWYG document processing application. It can generate and import SGML marked-up documents. 
;; Delphi
;;     An object-oriented development system from Borland - More information. 
;; Delta
;;     The information which differentiates a version from members of its immediate family. See change management 
;; Delta-4
;;     Definition and Design of an open Dependable Distributed system architecture. An Esprit project investigating the achievement of dependability in open distributed systems, including real-time systems. 
;; DELTASE
;;     A distributed processing environment concerned with fault-tolerant and process-control applications from the Esprit Delta-4 project. 
;; DEM
;;     Digital Elevation Model. A format for map files - More information. 
;; DeMarco
;;     Tom DeMarco proposed a form of Structured Analysis. 
;; Demeter
;;     A CASE tool developed mainly by Karl Lieberherr (see Aug/Sep 1988 issue of JOOP, OOPSLA '89 Proceedings "Contributions to Teaching Object-Oriented Design and Programming") 
;; DES
;;     Data Encryption Standard. A NIST encryption standard. 
;; Design
;;     Design is usually considered to be the phase of software development following analysis, and concerned with how the problem is to be solved. 
;; Design recovery
;;     A subset of reverse engineering in which domain knowledge, external information, and deduction of fuzzy reasoning are added to the observations of the subject system to identify meaningful higher level abstraction beyond those obtained directly by examining the system itself. 
;; Desktop manager
;;     A user interface to system services, usually icon and menu based like the Macintosh Finder, enabling the user to run applications and use a filing system without directly using the command language of the operating system. 
;; DESQview
;;     A system from Quarterdeck Office Systems implementing multitasking under MS-DOS. 
;; Destructor
;;     A function provided by a class in C++ to delete an object. 
;; DESY
;;     Deutsches Electronen Synchrotron Laboratory, Hamburg, Germany - More information. 
;; Development
;;     The process of analysis, design, coding and testing software. 
;; DFD
;;     Data Flow Diagram. 
;; DGL
;;     Data Generation Language: a tool for generating test data for hardware or software systems. 
;; DGL
;;     The distributed version of GL . 
;; Dhrystone
;;     A benchmark program in C and Ada. 
;; DIALOG
;;     A commercial bibliographic database and retrieval service from DIALOG Information Services. 
;; DIB
;;     Device Independent Bitmap, a format for portable images. 
;; Dienst
;;     A protocol for a distributed digital document library built on http - More information. 
;; DII
;;     Dynamic Invocation Interface. An OMG specification. 
;; DIIG
;;     Digital Information Infrastructure Guide. A resource to facilitate the development of the NII - More information. 
;; DIN
;;     Deutsche Institut fuer Normung: the German standardisation body, a member of ISO. 
;; DIP
;;     Document Image Processing: storage, management and retrieval of images. 
;; Dirt
;;     Design In Real Time: a user interface builder for the X Window System by R.Hesketh 
;; DISA
;;     Data Interchange Standards Association (USA) 
;; DISA
;;     Defense Information Systems Agency (USA) - More information. 
;; Display PostScript
;;     An extended form of PostScript permitting its interactive use with bitmap displays. 
;; DL/I
;;     The data manipulation language of IMS. 
;; DLG
;;     Digital Line Graph. A format for map files - More information. 
;; DLM
;;     Distributed Lock Manager on distributed VMS systems. 
;; DME
;;     Distributed Management Environment: an OSF standard presently at the RFT stage. 
;; DMS
;;     Document Management System. 
;; DNS
;;     Distributed Name Service: see DECdns. 
;; DOC
;;     Distributed Object Computing. 
;; Document Examiner
;;     A high-performance hypertext system by Symbolics that provides on-line access to their user documentation. 
;; Document Style Semantics and Specification Language
;;     An ISO standard under preparation, addressing the semantics of high-quality composition in a manner independent of particular formatting systems or processes. DSSSL is intended as a complementary standard to SGML for the specification of semantics. 
;; DoD-STD-2167A
;;     A DoD standard specifying the overall process of development and documentation for mission-critical software - More information. 
;; DoD-STD-2168
;;     A DoD standard for software quality assurance procedures. 
;; DoD
;;     The US Department of Defense, responsible for sponsoring many standards in the software engineering field - More information. 
;; DoE
;;     The US Department of Energy - More information. 
;; DOE
;;     Distributed Object Environment: a distributed object-oriented application framework from SunSoft. 
;; Domain
;;     Distributed Operating Multi Access Interactive Network:the proprietary network protocol used by Apollo workstations. 
;; DOMF
;;     Distributed Object Management Facility: an OMG-compliant object management system; part of DOE. from SunSoft. 
;; DOORS
;;     Dynamic Object Oriented Requirements System - More information. 
;; DORIS
;;     3-10 GeV center of mass electron-positron storage ring/collider at DESY. 
;; DPS
;;     Display PostScript. 
;; DQO
;;     Data Quality Objectives - More information. 
;; DRAGON
;;     An Esprit project aimed at providing effective support to reuse in real-time distributed Ada applications.. 
;; DRAGOON
;;     A distributed concurrent object-oriented Ada-based language from the Esprit DRAGON project. 
;; DSE
;;     Data Structure Editor. 
;; DSDM
;;     Dynamic Systems Development Method. A non-proprietary Rapid Application Development method - More information. 
;; DSEE
;;     Domain Software Engineering Environment: a proprietary CASE framework and configuration management system from Apollo. 
;; DSOM
;;     Distributed SOM 
;; DSP
;;     Digital Signal Processing. 
;; DS
;;     Dansk Standard. The Danish standards association. 
;; DSS
;;     Decision Support Systems. Software tools to help with management tasks. 
;; DSSSL
;;     Document Style Semantics and Specification Language. An ISO standard under preparation, addressing the semantics of high-quality composition in a manner independent of particular formatting systems or processes. DSSSL is intended as a complementary standard to SGML for the specification of semantics - More information. 
;; DTD
;;     Document Type Definition: the definition of a document type in SGML, consisting of a set of markup tags and their interpretation. 
;; DTI
;;     UK Department of Trade and Industry. 
;; DTIC
;;     Defense Technical Information Center of the US Dept. of Defense. 
;; DTL
;;     DVI Text Language. An ASCII DVI format. 
;; DTLS
;;     Descriptive Top-Level Specification language: used in POSIX and TRUSIX. 
;; DTP
;;     Desktop publishing. 
;; DTS
;;     Distributed Time Service . 
;; DVI
;;     Device independent file format. A dvi file containing a description of the formatted document is the usual output of TeX . 
;; Dylan
;;     An object-oriented dynamic language - More information. 
;; DWARF
;;     A debugging information format for UNIX System V 
;; E
;;     A database progamming language developed for the EXODUS project. 
;; E-mail
;;     See Electronic mail . 
;; EAPLS
;;     European Association for Programming Languages and Systems - More information. 
;; EARN
;;     European Academic and Research Network. A self-managing network in the research community originally sponsored by IBM. It uses BITNET protocols and connects to BITNET in the US - More information. 
;; EAST
;;     A Eureka project developing a software engineering platform. 
;; EC
;;     Electronic Commerce. Managing business transactions using networking and electronic means. 
;; ECFA
;;     European Committee for Future Accelerators. This body, whose principal role is to take care of Europe's requirements for future particle accelerators, has also looked at particle physics data handling on a European-wide basis. 
;; ECHO
;;     A public database service of the European Community - More information. 
;; ECHT
;;     European Conference on Hypertext. 
;; ECIP2
;;     An Esprit Project on the definition of a specification language at the requirement level. 
;; ECIS
;;     European Committee for Interoperable Systems. 
;; ECM
;;     Enterprise Component Modelling. 
;; ECMA
;;     European Computer Manufacturers Association - More information. 
;; ECO
;;     Engineering Change Order. 
;; ECOOP
;;     European Conference on Object-oriented Programming. 
;; ECRC
;;     Electronic Commerce Resource Centers. A network of US government sponsored centers that provide support to government and industry in developing and implementing strategies for business process improvement, implementing enabling technologies, and migrating to electronic commerce - More information. 
;; EDA
;;     Product line from Dazix. 
;; Eden
;;     An object-oriented distributed operating system based on an RPC mechanism . 
;; EDH
;;     Electronic Document Handling (at CERN) - More information. 
;; EDI
;;     Electronic Data Interchange: a set of standards for exchanging orders and other business transactions by electronic mail - More information. 
;; EDIF
;;     Electronic Design Interchange Format . 
;; EDM
;;     Engineering Data Management. 
;; EDMS
;;     Electronic Document Management System. 
;; EDUCOM
;;     A nonprofit consortium of US higher education institutions promoting access to and use of information resources and technology - More information. 
;; EEMA
;;     European Electronic Messaging Association. 
;; EER
;;     An extended entity-relationship model . 
;; EFF
;;     Electronic Frontier Foundation. An organisation working on civil rights issues in networking - More information. 
;; EHTS
;;     Emacs HyperText System: an experimental multiuser hypertext system from the University of Aalborg. It consists of a text editor (based on Epoch and GNU Emacs and written in elisp) and a graphical browser (based on XView and written in C) running under the X Window System and OpenWindows Both tools use HyperBase as database. 
;; EIA
;;     Electronic Industries Association. 
;; Eiffel
;;     An object-oriented programming language developed by B.Meyer et al. and commercialised by ISE - More information. 
;; Eiffel shelf
;;     A set of user-contributed classes available with the Eiffel system. 
;; EIS
;;     Executive Information System. 
;; EJO
;;     Electronic Journals Online. A service of the OCLC. 
;; ELOT
;;     The Greek standards association. 
;; Electronic Mail
;;     A system allowing computer users to exchange messages via a network. 
;; Ellemtel
;;     A C++ style guide originated by Ellemtel Telecom Systems, Stockholm. 
;; ELSA
;;     Electronic Library Services and Applications. A library of reusable public domain software supported by NASA - More information. 
;; emacs
;;     A popular editor and associated utilities for UNIX from the FSF 
;; email
;;     See Electronic mail . 
;; EMDIR
;;     The CERN Electronic Mail DIRectory utility. 
;; Encapsulation
;;     The ability to provide users with a well-defined interface to a set of functions in a way which hides their internal workings. In object-oriented programming, the technique of keeping together data structures and the methods (procedures) which act on them. 
;; Entity-Relationship diagram
;;     A type of diagram used in the Entity-Relationship model. 
;; Entity-Relationship
;;     An approach to data modelling proposed by P.Chen in 1976. 
;; EOQ
;;     European Organization for Quality. 
;; EOUG
;;     European ORACLE Users Group. 
;; EPCS
;;     Experimental Physics Control Systems: a group of the European Physical Society, focussing on all aspects of controls, especially informatics, in experimental physics, including accelerators and experiments. 
;; EPIC
;;     Electronic Privacy Information Center. A US center working on privacy issues relating to the National Information Infrastructure - More information. 
;; EPICS
;;     Experimental Physics and Industrial Control System. Software for accelerator, experiment, and process control from ANL and LANL - More information. 
;; EPO
;;     European Patent Office - More information. 
;; Epoch
;;     A version of GNU Emacs for the X Window system from NCSA. 
;; EPS
;;     Encapsulated PostScript - More information. 
;; EQA
;;     European Quality Award for process improvement. 
;; ER
;;     Entity-Relationship. 
;; ERA
;;     Entity-Relationship-Attribute. 
;; ERC
;;     An extended entity-relationship model . 
;; ERCIM
;;     European Research Consortium on Informatics and Mathematics. An association of European research organizations promoting cooperative research on key issues in information technology - More information. 
;; ERCS
;;     Extended Reference Concrete Syntaxes for SGML, to support East Asian and other non-English languages - More information. 
;; ERD
;;     Entity-relationship diagram. 
;; ESA
;;     European Space Agency - More information on ESA software standards. 
;; ESF
;;     Eureka Software Factory. 
;; ESI
;;     European Software Institute. A network of organisations co-operating in strategic planning of process improvement - More information. 
;; ESIS
;;     Element Structure Information Set produced by SGML parsers. 
;; ESML
;;     Extended Systems Modelling Language: a real-time software engineering methodology based on RTSA. 
;; ESPIF
;;     European Software Process Improvement Foundation - More information. 
;; Esprit
;;     A funding programme to develop Informatics in the EEC. - More information. 
;; Estelle
;;     A formal description technique developed for OSI protocol specification. 
;; ESUG
;;     European Smalltalk Users' Group. 
;; Ethernet
;;     A 10-megabit/second local area network developed by Xerox and now widely adopted. Hosts are connected to a coaxial cable, and transmission conflicts are avoided by backing off and re-sending later. IEEE standard 802.3 defines the hardware and transport layers of the network. 
;; ETLA
;;     Extended Three Letter Acronym. 
;; ETM
;;     An active DBMS from the University of Karlsruhe. 
;; ETSI
;;     European Telecommunications Standards Institute. 
;; EUnet
;;     The European UNIX network: an Internet service provider. More information. 
;; Eureka
;;     A European technological development programme. 
;; EuropaNET
;;     A combination of pan-European backbone services run by DANTE. 
;; EUSIDIC
;;     European Association of Information Services - More information. 
;; EUUG
;;     European UNIX User Group. 
;; EWOS
;;     European Workshop for Open Systems. 
;; Excelerator
;;     A set of CASE tools from Index Technology Corp. 
;; eXodus
;;     A package from White Pines allowing the Macintosh to be used as an X server. 
;; EXODUS
;;     An extensible database project developed at the University of Wisconsin. 
;; Expert system
;;     An intelligent computer program that contains a knowledge base, specialized software, and a set of algorithms or rules that infer new facts from knowledge and from incoming data. 
;; Express
;;     A data modelling language adopted by the ISO working group on STEP. 
;; Extensible database
;;     A DBMS that allows access to data from remote sources as if it were part of the database. 
;; EXUG
;;     European X User Group - More information. 
;; FATMEN
;;     A distributed file and tape management system for HEP data - More information. 
;; FDDI
;;     Fiber Distributed Data Interface: a new ANSI standard for a 100 megabits/second fibre optic token ring local area network 
;; FEA
;;     Finite Element Analysis. 
;; Feature
;;     An attribute or function of a class in Eiffel. 
;; Feed-forward
;;     A multilayer perceptron network in which the outputs from all neurons (see McCulloch-Pitts) go to following but not preceding layers, so there are no feedback loops. 
;; FFT
;;     Fast Fourier Transform 
;; FIMS
;;     Form Interface Management System. 
;; FIPS
;;     Federal Information Processing Standard: U.S. Government standards. 
;; FITS
;;     Flexible Image Transport System. The standard data interchange and archive format of the astronomy community - More information. 
;; FNAL
;;     Fermi National Accelerator Laboratory (Illinois, USA). 
;; Floppy
;;     A Fortran coding convention checker. The latest version has a feature for generating HTML. - More information. 
;; FOOM
;;     Formal Object Oriented Method. 
;; FOOT
;;     Forum for Object Oriented Technology at CERN - More information. 
;; Foresight
;;     A software product from Nu Thena providing graphical modelling tools for high level system design and simulation. 
;; Formal methods
;;     Several formal approaches to program specification have been developed, such as those based on VDM or Z. They can be used to develop software with high reliability, for safety-critical or high-volume applications - More information. 
;; FORML
;;     Formal Object Role Modeling Language. 
;; FORTH
;;     Greek FOundation for Research and Technology - More information. 
;; FORTRAN
;;     FORmula TRANslating system: a programming language widely used for many years in scientific applications. 
;; Forward delta
;;     The delta which, when combined with a version, creates a child version. See change management 
;; Forward engineering
;;     The traditional process of moving from high-level abstractions and logical, implementation-independent designs to the physical implementation of a system. 
;; FORWISS
;;     Bayerische Forschungszentrum fuer Wissensbasierte Systeme (Bavarian research centre for knowledge-based systems) in Passau - More information (in German). 
;; FOSI
;;     Formatted Output Specification Instance template for SGML 
;; FPA
;;     Function Point Analysis. 
;; FPM
;;     Function Point Metric. 
;; Fourth generation language
;;     A high-level language, usually non-procedural, to allow users inexperienced in programming to develop database applications. 
;; Framework
;;     In object-oriented systems, a set of classes that embodies an abstract design for solutions to a number of related problems - More information. 
;; FrameMaker
;;     Commercial publishing software available on a wide variety of workstations and addressing technical and scientific needs - More information. 
;; FreeHEP
;;     An organisation offering a repository of software and related information for high energy physics applications - More information. 
;; Fresco
;;     An object-oriented API for graphical user interfaces, under development by the X consortium as an open, multi-vendor standard. 
;; Friend
;;     Relationship between classes in the language C++. 
;; FSF
;;     Free Software Foundation (675 Massachusetts Ave., Cambridge, MA 02139, USA): dedicated to promoting the development and use of free software, especially the GNU system. 
;; FSM
;;     Finite State Machine. 
;; FTAM
;;     File Transfer, Access, and Management: an application layer protocol for file transfer and remote manipulation (ISO 8571). 
;; FTP
;;     File Transfer Protocol (based on TCP/IP). Also the name of a utility program available on several operating systems which makes use of this protocol to access and transfer files on remote computers. 
;; FTR
;;     Formal Technical Review. A software engineering technique - More information. 
;; Full-custom
;;     A technique used for the design of integrated circuits that involves the manipulation of circuit designs at the semiconductor device level. 
;; Function point
;;     A unit for estimating the functionality of a program - More information. 
;; Functional language
;;     A general purpose, high-level programming language based on the mathematical notion of functions. A functional program consists of a set of (possibly recursive) function definitions. Its execution consists of the evaluation of a function . Programs written in a functional language are generally compact and elegant, but tend to run slowly and consume a lot of memory. 
;; Functional programming
;;     See Functional language 
;; FUSE
;;     A DEC software development environment for ULTRIX, offering an integrated toolkit for developing, testing, debugging and maintainance. 
;; Fusion
;;     An object oriented analysis and design method developed by Hewlett Packard - More information. 
;; Futurebus+
;;     A high performance bus system specified by IEEE Std.896.2 
;; Fuzzy logic
;;     An alternative to traditional logic where truth values range between 0.0 and 1.0, with 0.0 representing absolute Falseness and 1.0 representing absolute Truth - More information. 
;; FVWM
;;     A window manager for the X Window System derived from twm - More information. 
;; FWEB
;;     See Literate Programming 
;; FWF
;;     Free Widget Foundation - More information. 
;; G2
;;     A real-time expert system from Gensym Corporation. 
;; GAIA
;;     GUI Application Interoperability Architecture project of OSF 
;; GAMS
;;     Guide to Available Mathematical Software at NIST - More information. 
;; GANDALF
;;     A software development environment from Carnegie Mellon University. 
;; Garbage collection
;;     The process of reclaiming storage which is no longer in use. 
;; Garnet
;;     A user interface development environment for Common Lisp and X or Macintosh from Carnegie Mellon - More information. 
;; GBIP
;;     General Purpose Interface Bus (IEEE 488). 
;; GCA
;;     Graphic Communications Association. 
;; GCC
;;     Gnu C Compiler. 
;; GDB
;;     Gnu DeBugger. 
;; GDMO
;;     Guidelines for the Definition of Managed Objects. A standard (ISO/IEC 10165-4 / ITU-T Rec. X.722) for defining data models on ASN.1 
;; GEANT
;;     A simulation, tracking and drawing package for HEP - More information. 
;; GEI
;;     A German software engineering company. 
;; GEN-X
;;     An expert system developed by General Electric. 
;; Generic Markup
;;     In computerised document preparation, a method of adding information to the text indicating the logical components of a document, such as paragraphs, headers or footnotes: SGML is an example of such a system. Specific instructions for layout of the text on the page do not appear in the markup. 
;; Genericity
;;     The possibility for a language to provided parameterized modules or types. e.g. List(of:Integer) or List(of:People). 
;; Genesia
;;     An expert system developed by Electricite de France and commercialised by STERIA (Paris). 
;; GEOS
;;     An object-oriented operating system project - More information. 
;; ghostscript
;;     The gnu PostScript interpreter. 
;; ghostview
;;     An X window interface to the ghostscript interpreter. 
;; GIF
;;     Graphics Interchange Format: a standard for digitised images compressed with the LZW algorithm - More information. 
;; GILS
;;     Government Information Locator Service. A plan for a decentralised collection of information locators and associated public services to find information throughout the US government. 
;; GINA
;;     Generic INteractive Application. A toolkit of useful classes and functions for authoring GUIs built on CLM, CLX and CLOS, from GMD 
;; GKS-3D
;;     The three-dimensional version of GKS, a standard for graphics I/O (ISO 8805). 
;; GKS
;;     Graphical Kernel System: a standard for graphics I/O (ANSI X3.124) - More information. 
;; GL
;;     A graphics package from Silicon Graphics. 
;; GLUT
;;     OpenGL Utility Toolkit - More information. 
;; GMD
;;     Gesellschaft fuer Mathematik und Datenverarbeitung (German Institute for Mathematics and Data Processing), D-53754 Sankt Augustin - More information. 
;; GNAT
;;     The GNU NYU Ada 95 compiler - More information. 
;; GNU
;;     GNU 's Not UNIX: a popular range of portable software from FSF, upwardly compatible with UNIX - More information. 
;; GOOD
;;     An object-oriented framework for graphical applications from TU Ilmenau running under X Windows with special support to IRIS GL, OpenGL, VOGL, etc. - More information. 
;; Gopher
;;     A Campus Wide Information System designed at the University of Minnesota - More information. 
;; GPIB
;;     General Purpose Interface Bus: an 8-bit parallel bus (IEEE 488). 
;; GPM
;;     General Purpose Macrogenerator written by C. Strachey around 1965. The author said "It contains in itself all the undesirable features of every possible machine code... It can also be almost impenetrably opaque". 
;; GQM
;;     Goal/Question/Metrics. A software engineering assessment method by V. Basili. 
;; Grapevine
;;     A distributed system project . 
;; Grammar
;;     A grammar is a mathematical system for defining a language, as well as a device for giving the sentences in the language a useful structure. 
;; GRAS
;;     A public domain graph-oriented database system for software engineering applications from RWTH Aachen 
;; GRASPIN
;;     An Esprit project to develop a personal software engineering environment to support the construction and verification of distributed and non-sequential software systems. 
;; Grasshopper
;;     An experimental operating system for persistent systems - More information. 
;; GRIB
;;     GRid In Binary. World Meteorological Organization data format - More information. 
;; Groupware
;;     see CSCW. 
;; GROW
;;     GNU Remote Operations Web. An architecture for building networked applications and services using WWW - More information. 
;; GUI
;;     Graphical User Interface. 
;; Guide
;;     A hypertext system from the University of Kent (GB) and OWL for displaying online documentation . 
;; GUIDE
;;     Graphical User Interface Development Environment from Sun. 
;; GUILE
;;     An interpreter for the GROW project. 
;; gunzip
;;     The decompression utility corresponding to gzip . 
;; gzip
;;     A compression utility available with the gnu software. 
;; h
;;     A simple markup language intended for quick conversion of existing text to hypertext - More information. 
;; Hardware description language
;;     A language used for the conceptual design of integrated circuits. Examples are VHDL and Verilog. 
;; Harmony
;;     A real-time operating system developed by the SEL in Canada. 
;; Harvest
;;     An information discovery and access system for the Internet from the University of Colorado - More information. 
;; Haskell
;;     A functional language (Hudak et al.). 
;; HBOOK
;;     A histogramming package in the CERN program library - More information. 
;; hc
;;     The compiler for the h hyperbook language. 
;; HCI
;;     Human Computer Interface (or Interaction) - More information. 
;; HCS
;;     Heterogeneous Computer System: a distributed system project . 
;; HDF
;;     Hierarchical Data Format from NCSA - More information. 
;; HDL
;;     Hardware description language. 
;; HDTV
;;     High Definition Television. 
;; Hebbian
;;     Refers to the most common way for a neural network to learn, namely supervised learning. Using a training sample which should produce known responses, the connection weights are adjusted so as to minimize the differences between the desired and actual outputs for the training sample. 
;; Helix
;;     A hardware description language from Silvar-Lisco. 
;; HEP
;;     High Energy (Particle) Physics. 
;; HEPDB
;;     A database management system for HEP - More information. 
;; HEPiX
;;     A recently formed collaboration among various HEP institutes aiming at providing "compatible" versions of the UNIX operating system at their sites - More information. 
;; HEPnet
;;     An association concerned with networking requirements for high energy physicists - More information. 
;; HEPVM
;;     A collaboration among various HEP institutes to implement "compatible" versions of IBM's VM-CMS operating system at their sites. 
;; HERA
;;     An electron-proton collider at DESY, W. Germany. 
;; Hermes
;;     An experimental object-oriented distributed systems language from IBM Watson Research Centre. 
;; Hesiod
;;     The name server of the Athena project. 
;; Heuristic
;;     A rule of thumb, simplification or educated guess that reduces or limits the search for solutions in domains that are difficult and poorly understood. Unlike algorithms, heuristics do not guarantee solutions. 
;; Hewlett-Packard*
;;     A manufacturer of workstations, electronic instrumentation and test equipment etc. 
;; HIGZ
;;     High Level Interface to Graphics and Zebra. Part of the PAW system - More information. 
;; HiPAC
;;     An active DBMS from Xerox Advanced Information Technology. 
;; HIPPI
;;     HIgh Performance Parallel Interface: a 100 Mbyte/sec data transfer system with associated interfaces and switches, developed at Los Alamos National Lab and now ANSI standard X3T9/88-127. 
;; HISTORIAN
;;     A source code management system sold by OPCODE, Inc.. 
;; History
;;     For more information on the history of computing, see the The Virtual Museum of Computing 
;; HOL
;;     An interactive theorem proving system based on Higher Order Logic - More information. 
;; Home Page
;;     The starting point for a WWW session. Many system adminstrators set up "home pages" which are the default page shown when a user begins a session. These pages usually have a lot of options and menu items that apply to that particular institution and then have links to other places. Here is the CERN home page. 
;; HOOD
;;     Hierarchical Object Oriented Design: a method for Architectural Design primarily for software to be developed in Ada, leading to automated checking, documentation and source code generation. 
;; Hope
;;     A functional language (Burstall et al. 1980). 
;; Hopfield
;;     John Hopfield in the early 1980's investigated a particular kind of neural network which is now commonly referred to as the Hopfield network or Hopfield model. In the Hopfield network, there are no special input or output neurons (see McCulloch-Pitts), but all are both input and output, and all are connected to all others in both directions (with equal weights in the two directions). Input is applied simultaneously to all neurons which then output to each other and the process continues until a stable state is reached, which represents the network output. 
;; HotJava
;;     A WWW browser from Sun based on the Java language - More information. 
;; HP-UX
;;     The version of UNIX running on Hewlett-Packard workstations. 
;; HP VEE
;;     Visual Engineering Environment from Hewlett-Packard: a package similar in intention to LabVIEW running on UNIX workstations with OSF/Motif. 
;; HP
;;     Hewlett-Packard. 
;; HPLOT
;;     A graphical output facility for HBOOK - More information. 
;; HPPI
;;     An earlier name for HIPPI. 
;; HTML
;;     HyperText Markup Language. An SGML document type used to mark up hypertext in the WWW project - More information. 
;; HTTP
;;     HyperText Transfer Protocol. The protocol used between client and server in the WWW project. 
;; Hyper-G
;;     A hypertext system from TU Graz - More information. 
;; Hyper-Man
;;     A browser available with Epoch giving hypertext capability for the UNIX manual. 
;; HyperBase
;;     An experimental active multiuser database for hypertext systems from the University of Aalborg, written in C++.It is built on the client-server model enabling distributed, concurrent, and shared access from workstations in a local area network. See EHTS. 
;; Hyperbole
;;     An information management and hypertext system - More information. 
;; Hypercard
;;     A software package for the Macintosh for storage and retrieval of information. It can handle images, and is designed for browsing. The powerful customisable interactive user interface allows new applications to be easily constructed by manipulating objects on the screen, often without conventional programming. 
;; Hypermedia
;;     Hypertext systems where the nodes can contain text, graphics, audio, video, as well as source code or other forms of data - More information. 
;; HyperNeWS
;;     A Hypertext system from the Turing Institute Glasgow, based on NeWS. 
;; HyperODA
;;     ODA extensions for hypermedia. 
;; Hypertalk
;;     The language for writing procedures associated with objects in Hypercard. 
;; Hypertext
;;     An approach to information management in which text is stored in a network of nodes connected by links. The nodes are meant to be viewed through an interactive browser. A link is something which connects a piece of text to a destination piece of text; the source and destination areas are usually marked on a display by highlighting or special graphics. You are reading hypertext now by courtesy of WWW - More information. 
;; HyTime
;;     Hypermedia/Time-based Structuring Language: an ANSI/ISO Standard (ISO/IEC 10744) from the SGML Users' Group's Special Interest Group on Hypertext and Multimedia (SIGhyper) - More information. 
;; I-CASE
;;     Integrated CASE: another term for an IPSE. 
;; IAB
;;     The Internet Architecture Board of the Internet Society - More information. 
;; IAD
;;     A dynamic analyser from IBM giving information on run time performance and code utilisation. 
;; IAFA
;;     Internet Anonymous FTP Archives. An IETF working group. 
;; IANA
;;     Internet Assigned Numbers Authority - More information. 
;; IBM
;;     International Business Machines - More information. 
;; IBN
;;     The Belgian standards institute. 
;; ICADD
;;     International Committee for Accessible Document Design. Dedicated to making printed materials accessible to persons with print disabilities. Works on the generation of Braille, large print or electronically navigable editions of books from desktop publishing files - More information. 
;; ICCP
;;     Institute for Certification of Computing Professionals. 
;; ICSI
;;     International Computer Science Isntitute at Berkeley, CA. - More information. 
;; IDE
;;     Interactive Development Environments: a US Software Engineering Company. 
;; IDEA
;;     International Data Encryption Algorithm (used by PGP). 
;; IDL
;;     Interactive Data Language. A package for interactive reduction, analysis, and visualization of scientific data, from Research Systems, Inc. - More information. 
;; IDL
;;     Interface Definition Language: an OSF standard for defining RPC stubs. 
;; IDL
;;     Interface Definition Language: associated with the CORBA standard - More information. 
;; IDSS
;;     Intelligent Decision Support Systems. 
;; IEC
;;     International Electrotechnical Commission: a standardisation body at the same level as ISO - More information. 
;; IEF
;;     Information Engineering Facility. A CASE tool from Texas Instruments which generates code from graphical business process models. 
;; IEEE 1076
;;     The IEEE standard for VHDL.. 
;; IEEE 488
;;     The IEEE standard for GPIB. 
;; IEEE 802
;;     The IEEE standards for local area networks (LANs). The Ethernet standard is 802.3, the IBM Token Ring is IEEE 802.5. 
;; IEEE
;;     Institute of Electrical and Electronics Engineers (USA) - More information. 
;; IESG
;;     Internet Engineering Steering Group. Part of the Internet Society responsible for technical management of IETF activities and the Internet Standards process - More information. 
;; IETF
;;     Internet Engineering Task Force. A group of people who make technical and other contributions to the engineering and evolution of the Internet and its technologies. It is the principal body engaged in the development of new Internet Standard specifications - More information. 
;; IETM
;;     Interactive Electronic Technical Manual. 
;; IFAC
;;     International Federation of Automatic Control, involved in informatics related to control systems. 
;; IFDL
;;     Independent Form Description Language: DEC's language for describing form-based human interfaces in DECforms. 
;; IFIP
;;     International Federation of Information Processing - More information. 
;; IFPUG
;;     International Function-point Users Group - More information. 
;; IGES
;;     Initial Graphics Exchange Specification: an ASME/ANSI standard for the exchange of CAD data. 
;; IIDMS/R
;;     Integrated database management system: a DBMS from Cullinet Software Inc. 
;; IIIS
;;     International Institute of Informatics and Systemics. 
;; ILU
;;     Inter-Language Unification. A system from Xerox PARC that promotes software interoperability via interfaces - More information. 
;; Immediate version
;;     See Child version. 
;; IMS
;;     Information Management System: a DBMS from IBM. 
;; IMSE
;;     Integrated Modelling Support Environment: an Esprit programme. 
;; INCOSE
;;     International Council on Systems Engineering. An international organization formed to develop, nurture and enhance the system engineering approach to multi-disciplinary system product development - More information. 
;; Inference
;;     The logical process by which new facts are derived from known facts. 
;; Inference engine
;;     A program that infers facts from a set of knowledge or inputs. 
;; INFN
;;     Istituto Nazionale di Fisica Nucleare: an Italian State research organisation - More information. 
;; Informix
;;     A relational DBMS vendor. 
;; INGRES
;;     A relational DBMS vendor. 
;; Inheritance
;;     In object-oriented programming, the ability to derive new classes from existing classes. A derived class inherits the instance variables and methods of the base class, and may add new instance variables and methods. A new method may be defined with the same names as one in the base class, in which case it overrides the original one. 
;; INRIA
;;     Institut National de Recherche en Informatique et Automatique, French computer science research institute - More information. 
;; Instantiation
;;     A more precisely defined version of some object which was already partially defined. In object-oriented programming, a particular example of an object produced from its class template. 
;; InterBase
;;     A commercial active DBMS. 
;; Interface Architect
;;     An interface builder for Motif distributed by Hewlett-Packard (see UIMX). 
;; Interleaf
;;     A document preparation system available on the Sun, VAX, Apollo and other workstations. 
;; INTERLINK
;;     A commercial product comprising hardware and software for file transfer between IBM and VAX computers. 
;; Intermedia Interchange Format
;;     A Standard Hypertext Interchange format from IRIS. 
;; Intermedia
;;     A hypertext system developed by a research group at IRIS (Brown University). 
;; Intermetrics
;;     A software engineering company . 
;; Internet Address
;;     A thirty-two-bit number that uniquely identifies an Internet host. It is usually represented as four 8-bit numbers separated by dots e.g. 128.121.4.5. It consists of a network number and a host number, and can be subdivided in several ways. 
;; Internet
;;     A loosely-organized international collaboration of autonomous, interconnected networks, supporting host-to-host communication through voluntary adherence to open protocols and procedures defined by Internet Standards, typically based on the TCP/IP protocol suite - More information. 
;; Interpress
;;     A page description language from Xerox. 
;; InterViews
;;     An object-oriented toolkit developed at Stanford University for building graphical user interfaces. It is implemented in C++ and provides a library of objects and a set of protocols for composing them. 
;; Intrinsics
;;     A library package on top of Xlib, extending the basic functions of the X Window System. It provides mechanisms for building widget sets and application environments.. 
;; Inventor
;;     See Open Inventor. 
;; Inverse engineering
;;     The process of extracting high-level abstract specifications from source code using program transformations - More information. 
;; ION
;;     Implementation-Oriented Notation. A notation designed to graphically document object-oriented programs - More information. 
;; IP address
;;     An Internet address. 
;; IP
;;     Internet transport layer Protocol. 
;; IPC
;;     Inter-Process Communication. 
;; IPE
;;     Integrated Programming Environment. 
;; IPF
;;     Information Presentation Facility. A document markup system for OS/2 based on SGML. 
;; IPSE
;;     Integrated Project Support Environment: a term for a set of management and technical tools to support software development, usually integrated in a coherent framework: equivalent to an SEE. 
;; IPTES
;;     Incremental Prototyping Technology for Embedded Realtime Systems, an Esprit project. 
;; IPVR
;;     Institute of Parallel and Distributed High-Performance Systems (Stuttgart). 
;; IQA
;;     Institute of Quality Assurance (UK). 
;; IRC
;;     Internet Relay Chat. A system whereby a number of people can participate in a discussion in real time on the Internet. 
;; IRD
;;     Internet Resource Discovery. 
;; IRDS
;;     Information Resource Dictionary System. A set of ISO standards for CASE repositories. It governs the definition of data dictionaries to be implemented on top of relational databases (see repository, data dictionary). 
;; Iris
;;     An object-oriented DBMS. 
;; IRIS
;;     Institute for Research in Information and Scholarship of Brown University (Providence RI). 
;; IRIS
;;     See IRIS Explorer 
;; IRIS Explorer
;;     A visualisation system - More information. 
;; ISA
;;     An Esprit project continuing the ANSA project. 
;; ISA
;;     International Smalltalk Association (now disbanded). 
;; ISAM
;;     Indexed Sequential Access Method: a file access method supporting both sequential and indexed access. 
;; ISBN
;;     International Standard Book Numbering. 
;; ISCN
;;     International Software Consulting Network. A network of process improvement experts. 
;; ISDE
;;     Integrated Software Development Environment: equivalent to an IPSE. 
;; ISDN
;;     Integrated Services Digital Network: a set of CCITT standards to support many types of signal traffic (speech, data, video) via a digital transmission system, eventually intended to replace current telephone systems. The Basic rate is 64 kbits/sec - More information. 
;; ISE
;;     Interactive Software Engineering: a software engineering company marketing Eiffel among other products. 
;; ISEE
;;     Integrated Software Engineering Environment: equivalent to SEE. 
;; ISERN
;;     International Software Engineering Research Network - More information. 
;; ISF
;;     Information Systems Factory: equivalent to an SEE. 
;; ISIS
;;     A toolkit for implementing fault-tolerant distributed systems, developed at Cornell and now available commercially 
;; ISO
;;     International Organisation for Standardisation - More information. 
;; ISOC
;;     The Internet Society. A professional society concerned with the growth and evolution of the Internet, with the way it is used, and with related social, political, and technical issues - More information. 
;; ISODE
;;     ISO Development Environment: software that implements a set of OSI upper-layer services. It supports OSI applications on top of OSI and TCP/IP networks - More information. 
;; ISPE
;;     International Society for Productivity Enhancement. 
;; ISTAR
;;     An experimental IPSE. from Imperial Software Technology. 
;; ISV
;;     Independent Software Vendor (not a hardware manufacturer). 
;; IT
;;     Information Technology. 
;; ITHACA
;;     An Esprit project to put a "4th generation" object-oriented system to practical use in an industrial environment. The ITHACA environment offers an application support system incorporating advanced technologies in the fields of object-oriented programming, programming languages, database technologies, user interface systems and software development tools - More information. 
;; ITU
;;     International Telecommunications Union - More information. 
;; Jackson method
;;     A proprietary structured method for software analysis, design and programming. 
;; JANET
;;     The Joint Academic NETwork which links U.K. academic and research institutes. 
;; Java
;;     An Object-Oriented language from Sun, now widely used in WWW browsers - More information. 
;; JAZELLE
;;     A data management system for HEP from SLAC. 
;; JEDI
;;     Joint Electronic Document Interchange - More information. 
;; JEPI
;;     Joint Electronic Payment Initiative. A joint project between W3C and CommerceNet in the field of electronic payment using WWW. 
;; JFIF
;;     A data stream-oriented file format used for transmitting JPEG encoded bitmap data - More information. 
;; JOOP
;;     Journal of Object-Oriented Programming. 
;; JPEG
;;     A standardized image compression mechanism. JPEG stands for Joint Photographic Experts Group, the original name of the committee that wrote the standard. JPEG is designed for compressing either full-color or gray-scale digital images of "natural", real-world scenes. It does not work so well on non-realistic images, such as cartoons or line drawings. JPEG does not handle black-and-white (1-bit-per-pixel) images, or motion picture compression. Standards for compressing those types of images are being worked on by other committees, named JBIG and MPEG - More information. 
;; jpg
;;     See JPEG. 
;; JSA
;;     Japanese Standards Association 
;; JSD
;;     Jackson System Development - More information. 
;; JTC
;;     Joint Technical Committee (of ISO and IEC). 
;; Kala
;;     A persistent data server: a link library providing an engine for applications needing persistence, transactions, crash recovery and rollback, versioning, distribution, and other facilities for which DBMSs are commonly used - More information. 
;; KAPPA
;;     An object-oriented workbench for Sun workstations from Intellicorp. 
;; KBS
;;     Knowledge-based system. 
;; KDD
;;     Knowledge Discovery in Databases. A branch of Artificial Intelligence. 
;; Kerberos
;;     An authentication system from the Athena project, adopted by OSF as the basis of security for DME - More information. 
;; KERMIT
;;     A protocol for file transfer. Mainly used for transfers to and from PC's. 
;; kernel
;;     The essential part of UNIX or other operating systems, responsible for resource allocation etc. 
;; Khoros
;;     A visualisation system from Khoral Research. 
;; KIF
;;     Knowledge Interchange Format. For knowledge sharing and communication among heterogeneous agents. 
;; KISS
;;     Keep It Simple Stupid. A homespun design philosohpy. 
;; KISS
;;     An Object-Oriented analysis and design approach - More information. 
;; KMS
;;     Knowledge Management System: a distributed hypermedia system for managing knowledge in organisations A commercial system from Knowledge Systems Inc running on workstations, based on previous research with ZOG at Carnegie Mellon University. 
;; Knowledge Engineering
;;     The acquisition of knowledge from a human expert or similar source and its coding in an expert system. 
;; Knowledge Representation
;;     A subset of AI . 
;; Kohonen
;;     T. Kohonen of the University of Helsinki has been studying neural networks for many years with the idea of modelling as closely as possible the behaviour of biological systems, and his name is commonly associated with a particular kind of neural network in which there are only two kinds of neurons (see McCulloch-Pitts), input and others. All the input neurons are connected to all others, and the others are connected only to their other nearest neighbors. The training algorithm is a relatively simple one based on the geometric layout of the neurons, and makes use of annealing. 
;; KQML
;;     Knowledge Query and Manipulation Language. 
;; KR
;;     Knowledge Representation. 
;; KUIP
;;     Kernel User Interface Package: the human interface to PAW. 
;; Labview
;;     A package from National Instruments Corp originally developed to provide a graphical interface to instruments connected by the IEEE 488 (GPIB) bus. It has powerful graphical editing facilities for defining and interconnecting "virtual instruments". 
;; LAMPF
;;     Los Alamos Meson Physics Facility (An 800 MeV proton and negative H ion high-current LINAC, 1mA average, 12mA peak). 
;; LAN
;;     Local area network 
;; Language-Based Editor
;;     An editor that is aware of the syntactic, semantic and in some cases the structural rules of a specific programming language and provides a framework for the user to enter source code. Programs or changes to previously stored programs are incrementally parsed into an abstract syntax tree and automatically checked for correctness. 
;; LANL
;;     Los Alamos National Laboratory - Los Alamos, NM, USA - More information. 
;; LaTeX
;;     A document preparation system based on TeX, popular in the HEP community. It adds a collection of commands to simplify typesetting, and lets the user concentrate on the structure of the text rather than on formatting commands - More information. 
;; LBE
;;     Language-Based Editor. 
;; LBL
;;     Lawrence Berkeley Laboratory, Berkeley, CA, USA. 
;; LCF
;;     Logic for Computable Functions. A system for interactive automated reasoning. 
;; LEAR
;;     Low Energy Antiproton Ring. 
;; LEDA
;;     Library of Efficient Data types and Algorithms. A class library for C++ with graph classes from Uni Saarbruecken. 
;; Legacy
;;     Legacy system is a term used to describe old software systems still in use but which could benefit from re-engineering using more modern methods. 
;; LEP
;;     Large Electron Positron Collider. A 27km circumference accelerator at CERN, which brings bunches of electrons and positrons into collision. 
;; lex
;;     A lexical analysis tool for the UNIX environment. 
;; LHC
;;     Large Hadron Collider: proposed to be built in the LEP tunnel at CERN. 
;; Life-Cycle
;;     The software life-cycle consists of phases: requirements analysis, design, construction, testing and maintenance. The development process tends to run iteratively through these phases rather than linearly; several models (spiral, waterfall etc) have been proposed to describe this process. 
;; Lifecycle
;;     See Life-Cycle . 
;; LIFIA
;;     Laboratoire d'Informatique Fondamentale et d'Intelligence Artificielle. 
;; LIFN
;;     Location Independent File Name 
;; LIGHT
;;     LIfecycle Global HyperText. A project in the CERN ECP/IPT group whereby documents resulting from the software life cycle are available as hypertext - More information. 
;; Linda
;;     A portable parallel language to simplify parallel programming. Extensions to C and Fortran, available from Scientific Computing Associates, Inc. - More information. 
;; link
;;     see Hypertext 
;; lint
;;     A C language preprocessor which carries out more thorough checks on the code than is usual with C compilers themselves. 
;; Linux
;;     An implementation of UNIX written from scratch with no proprietary code for IBM PC compatibles by Linus Torvalds and distributed under the GNU public licence - More information. 
;; LISP
;;     A List Processing Language suitable for symbolic and logical programming - More information. 
;; LispView
;;     CLOS based windowing system on OpenWindows. 
;; Literate programming
;;     Combining the use of a language such as TeX and a conventional programming language, so as to maintain documentation and source together - More information. 
;; LitProg
;;     Literate Programming 
;; Lml
;;     A functional language (Johnson 1984). 
;; LOC
;;     Line of code. Used as a simple software metric. 
;; Local Area Network
;;     Usually abbreviated to LAN: a communications network which is geographically limited (typically to a 1 km. radius) allowing easy interconnection of terminals, microprocessors and computers within adjacent buildings. Ethernet and FDDI are examples of standard LANs. 
;; Locus
;;     A distributed system project supporting transparent access to data through a network-wide file system. 
;; Logic Programming
;;     Programming in a language such as Prolog, which allows the programmer to make a series of assertions which are interpreted by an inference engine - More information. 
;; LOGISCOPE
;;     Software quality analysis tools from Verilog SA, used to evaluate the quality of software both statically (based on software metrics) and dynamically - More information. 
;; Lojban
;;     An artificial language designed to be used by people in communication with each other, and possibly in the future with computers - More information. 
;; Looking Glass
;;     A desktop manager for UNIX from Visix. 
;; LOOPS
;;     Lisp Object-oriented Programming System from Intelligent Systems Laboratory, Xerox Palo Alto Research Center. 
;; LOTOS
;;     A formal description technique used for protocol specfication in ISO OSI standards (ISO 8807). 
;; LSE
;;     Language Sensitive Editor: from DEC . 
;; Lynx
;;     A WWW browser from University of Kansas - More information.

;;     (See also LynxOS). 
;; LynxOS
;;     A POSIX compliant real-time operating system from Lynx Real-Time Systems, Los Gatos, California, with a UNIX-like interface to application programs. 
;; LZW
;;     Lempel-Ziv-Welch data compression algorithm. 
;; MACAnalyst
;;     An analysis CASE tool for the Mac from Excel Software Inc. 
;; MACDesigner
;;     A design CASE tool for the Mac from Excel Software Inc. 
;; Mach
;;     An operating system kernel under development at Carnegie-Mellon University to support distributed and parallel computation. Mach is designed to support computing environments consisting of networks of uniprocessors and multiprocessors. Mach is the kernel of the OSF/1 system - More information. . 
;; Macintosh
;;     A range of personal computers manufactured by Apple Computer Inc. 
;; MacX
;;     A package allowing the Macintosh to be used as an X server. 
;; Madaline
;;     A structure of many ADALINE units. 
;; Maintenance
;;     An important part of the software life-cycle. Maintenance is expensive in manpower and resources, and software engineering techniques aim to reduce its cost. 
;; Make
;;     A popular tool on UNIX systems to automate the recompilation, linking etc. of programs, taking account of the interdependencies of modules. 
;; Makedoc
;;     A program from Carleton University, Ottawa that generates documentation for Objective C programs. It will also generate a class hierarchy diagram. The output format is similar to that used by StepStone. 
;; MAP
;;     Manufacturers Automation Protocol, a set of protocols developed by General Motors based on Token Bus (IEEE 802.4) and giving predictable response in real time. 
;; Maple
;;     A mathematics package developed by the University of Waterloo and ETH Zurich. 
;; MARC
;;     MAchine Readable Cataloging: a record format for bibliographic information interchange based on the ANSI / NISO Z39.2 standard. 
;; Markowitz
;;     The author of the original Simscript language. 
;; Markup
;;     In computerised document preparation, a method of adding information to the text indicating the logical components of a document, or instructions for layout of the text on the page. 
;; MASCOT
;;     Modular Approach to Software Construction Operation and Test: a method for software design aimed at real-time embedded systems from the Royal Signals and Research Establishment, UK. 
;; Mathematica
;;     A general program for symbolic computing and programming from Wolfram Research - More information. 
;; MBONE
;;     Multicast backbone: a virtual network on top of the Internet to support routing of IP multicast packets, intended for multimedia transmission - More information. 
;; McCulloch-Pitts
;;     The McCulloch-Pitts neuron is the basic building block of neural networks. It receives one or more inputs and produces one or more identical outputs, each of which is a simple non-linear function of the sum of the inputs to the neuron. The non-linear function is typically a threshhold or step function which is usually smoothed (i.e. a sigmoid) to facilitate learning. 
;; MCS
;;     Meta Class System: a portable object-oriented extension of Common Lisp from GMD. It integrates the functionality of CLOS and TELOS. 
;; MDL
;;     An early object-oriented language from MIT . 
;; Mellor
;;     see Schlaer-Mellor. 
;; Member Function
;;     In C++, the name given to a method. 
;; MERISE
;;     Methode d'Etude et de Realisation Informatique pour les Systemes d'Enteprise: a Software Engineering method popular in France; many IPSE s are based on it. 
;; Mesa
;;     An early object-oriented programming language developed at the Xerox Palo Alto research centre. 
;; Message
;;     In object-oriented programming sending a message to an object (to invoke a method) is equivalent to calling a procedure in traditional programming languages, except that the actual code executed may only be selected at run-time depending on the class of the object. Thus, in response to the message "drawSelf", the method code invoked would be different if the target object were a circle or a square. 
;; Meta-CASE tool
;;     A term sometimes used for software packages (like TBK or VSF) which allow users to develop or customise their own CASE tools. 
;; MetaCard
;;     A commercial human interface and hypertext system for UNIX and X-windows, similar to Hypercard. 
;; Metaclass
;;     The class of a class. A metaclass is a class whose instances are themselves classes. 
;; Metadata
;;     Data definitions describing aspects of the actual data items, such as name, format etc. 
;; Metafile
;;     Typically a file of graphics data for transport between different machines. 
;; Method
;;     The name given in Smalltalk (and sometimes in other object-oriented languages) to a procedure or routine associated with an object. 
;; Methodology
;;     A term for a codified set of procedures for some phase of software engineering, such as analysis and design. 
;; Metric
;;     see Software Metrics. 
;; Meyer
;;     Bertrand Meyer, the author of the Eiffel Language and many articles on object-oriented software techniques. 
;; Microkernel
;;     An approach to operating systems design which puts emphasis on small modules which implement the basic features of the system and can be flexibly configured . 
;; Microsoft
;;     A vendor of systems and application software for personal computers and similar platforms - More information. 
;; MID
;;     Metafile for Interactive Documents. A standard sponsored by the DoD. 
;; Midas-WWW
;;     A Motif-based browser for WWW - More information. 
;; Midas
;;     A Motif-based toolkit for interactive data analysis by T.Johnson, SLAC. The basis for the Midas-WWW browser. 
;; MIDI
;;     Musical Instrument Digital Interface. 
;; MIFF
;;     Machine Independent File Format. A bitmap format - More information. 
;; MIMD
;;     Multiple Instruction Multiple Data: a form of parallelism in multiprocessor computing where there are several instruction streams (programs) operating concurrently on several data streams. 
;; MIME
;;     Multimedia Internet Mail Extensions. A method of processing multi-part, multimedia messages on the Internet . (RFC 1521-1522 etc.) - More information. 
;; MINUIT
;;     A Program for Function Minimization and Error Analysis - More information. 
;; MIPS
;;     A microprocessor vendor . 
;; MIS
;;     Management Information Systems. 
;; MIT
;;     Massachusetts Institute of Technology - More information. 
;; ML
;;     A functional language - More information. 
;; MMM
;;     A WWW browser from INRIA based on the Caml language - More information. 
;; MODSIM
;;     A general-purpose modular block-structured language from CACI, which provides support for object-oriented programming and discrete event simulation. It is intended for building large process-based discrete event simulation models through modular and object-oriented mechanisms similar to those of Modula-2 - More information. 
;; Modula-2
;;     A high-level programming language designed by N.Wirth. It is a derivative of Pascal with well-defined interfaces between modules, and facilities for parallel computation. 
;; Modula-3
;;     A member of the Pascal family of languages. Designed in the late 1980s at Digital Equipment Corporation and Olivetti, it aims to correct deficiencies of Pascal and Modula-2 - More information. 
;; Montage
;;     An object-relational database management system from Montage Software: the commercialisation of POSTGRES 
;; MOOD
;;     Material's Object-Oriented Database. An object oriented database system from Tohoku University - More information. 
;; MOOSE
;;     An object-oriented R&D project at CERN - More information. 
;; Mosaic
;;     An X-Window based browser for WWW from NCSA - More information. 
;; MOSES
;;     Methodology for Object-oriented Software Engineering of Systems - More information. 
;; Motif
;;     The standard Graphical User Interface and window manager from OSF, running on theX Window System - More information. 
;; MPEG
;;     Moving Pictures Experts Group of ISO that generates standards for digital video (sequences of images in time) and audio compression. 
;; MPV
;;     Extension of the VRTX real-time operating system to support multi-processing. 
;; MS-DOS
;;     An operating system developed by MicroSoft Corporation for computers using the Intel 16 and 32-bit family of processors. 
;; MTBF
;;     Mean Time Between Faults 
;; Multibus
;;     A bus standard for microprocessor-based systems, specified by IEEE Std.796 
;; Multi-media
;;     See Multimedia. 
;; Multimedia
;;     Human computer interaction involving text, graphics, voice, video etc - More information. 
;; Multiple Inheritance
;;     In object-oriented programming, the possibility that a sub-class may be derived from multiple parents which are themselves not derived one from the other. 
;; Muse
;;     An electronic journal project at Johns Hopkins - More information. 
;; MVC
;;     Model View Controller architecture for interactive software - More information. 
;; MVE
;;     Modular Visualisation Environment. A type of application builder for scientific and other visualisation systems (such as AVS, IBM Data Explorer, IRIS Explorer, Khoros). 
;; NAG
;;     Numerical Algorithms Group - More information. 
;; NAPLPS
;;     North American Presentation Layer Protocol Syntax. 
;; NAS
;;     Network Application Support: DEC's approach to applications integration across a distributed multivendor environment. 
;; NASA
;;     National Aeronautics and Space Administration (USA). NASA has many software engineering projects - More information. 
;; NBS
;;     National Bureau of Standards: part of the U.S. Department of Commerce, now NIST. 
;; NCOSE
;;     National Council On Systems Engineering (USA) - More information. 
;; NCS
;;     Network Computing System: Apollo's RPC system used by DEC and Hewlett-Packard.The protocol has been adopted by OSF. 
;; NCSA
;;     National Center for Supercomputing Applications, Urbana, IL, USA - More information. 
;; NCSS
;;     Non-Commented Source Statements. Used as a simple software metric. 
;; NDL
;;     National Database Language: a US standard for portability of database definitions and application programs. 
;; Neptune
;;     A hypertext system for computer assisted software engineering, developed at Tektronix. 
;; netCDF
;;     Network Common Data Form. A machine-independent, self-describing file format for scientific data - More information. 
;; NetClasses
;;     A C++ class library for object transport and remote method invocation from Stanford - More information. 
;; NeuDL
;;     Neural network Description Language from the University of Alabama - More information. 
;; Neural net
;;     See Neural network 
;; Neural network
;;     A computing device which converts one or more input signals to one or more output signals by means of an interconnected set of elementary non-linear signal processors called neurons. Animal brains are examples of biological neural networks. Artificial Neural Networks are man-made computing devices modelled after their biological counterparts. The features which distinguish artificial neural networks from traditional Von Neumann (sequential) computers are: (a) the elementary processors are highly non-linear (in the limit, they are simple threshold discriminators), (b) the neurons are highly interconnencted which allows a high degree of parallelism and (c) there is no idle memory containing data and programs, but rather each neuron is pre-programmed and continuously active - More information. 
;; Neural
;;     See Neural network 
;; Neuron
;;     See Neural network , also McCulloch-Pitts 
;; NeWS
;;     Network extensible Window System from Sun Microsystems, offering facilities similar to those of the X Window System. Communication is based on PostScript, and server functions can be extended. 
;; NewWave
;;     A graphical user interface and object-oriented environment from Hewlett-Packard, based on Windows and available on UNIX workstations. 
;; NeXTstep
;;     A graphical interface builder, object-oriented application builder, and windowing software for the NeXT and IBM AIX systems. 
;; NFF
;;     Neutral File Format. A minimal scene description language - More information. 
;; NFS
;;     Network File System: developed by Sun to allow a computer to access files over a network as if they were on local disks; now public domain, a de facto standard. 
;; NFT
;;     Network File Transfer. An INTERLINK command. 
;; Nial
;;     Nested Interactive Array Language. A high-level array-oriented procedural language based on a mathematical theory of arrays, developed at Queen's University. It combines APL data structure ideas with LISP-style evaluation concepts and a conventional control structure syntax - More information. 
;; NIAM
;;     Natural Language (or Nijssen) Information Analysis Method: a method for data modelling. (see "Conceptual Scheme and Relational Database Design", Nijssen and Halpin, Prentice-Hall, 1989) 
;; NII
;;     National Information Infrastructure (USA) - More information. 
;; NIH
;;     The US National Institutes of Health - More information. 
;; NIHCL
;;     A class library for C++ from the NIH - More information. 
;; NISO
;;     National Information Standards Organisation (USA). NISO Standards cover many aspects of library science, publishing, and information services, and address the application of both traditional and new technologies to information services - More information. 
;; NIST
;;     National Institute of Standards and Technology, USA (formerly NBS) - More information. 
;; NITF
;;     National Imagery Transmission Format - More information. 
;; NLP
;;     Natural Language Processing. 
;; NLS
;;     Native Language System: a set of interfaces specified by X/Open for developing applications to run in different natural language environments. 
;; NLSR
;;     Natural Language Software Registry. A summary of the capabilities and sources of language processing software available to researchers - More information. 
;; NMF
;;     Network Management Forum of OSI 
;; NNTP
;;     Network News Transfer Protocol: the protocol used for distributing news on the Internet. 
;; Node
;;     see Hypertext 
;; Normal form
;;     A relation in a relational database is said to be in normal form if it satisfies certain constraints. Codd's original work defined three such forms. 
;; NoteCards
;;     An ambitious hypertext system developed at Xerox PARC, "designed to support the task of transforming a chaotic collection of unrelated thoughts into an integrated, orderly interpretation of ideas and their interconnections". 
;; Novell
;;     A proprietary local area network protocol developed by Novell Netware for the interconnection of PCs over Ethernet. 
;; NOWEB
;;     A system of structured programming and documentation from M.Speh in DESY. See Literate Programming 
;; NQIC
;;     National Quality Information Centre of the IQA systems. 
;; NQS
;;     Batch processing software for UNIX systems. 
;; NREN
;;     National Research and Education Network (USA) - More information. 
;; NSAI
;;     National Stsndards Authority of Ireland. 
;; NSE
;;     Network Software Environment: a proprietary CASE framework from Sun Microsystems. 
;; NSF
;;     National Science Foundation (USA) - More information. 
;; NSRD
;;     National Software Reuse Directory. A directory of reusable software in the ASSET system, now incorporated in the Asset Reuse Library. 
;; NTIS
;;     National Technical Information Service of the US Department of Commerce. 
;; NTP
;;     Network Time Protocol: a protocol built on top of TCP/IP that allows local clocks to be synchronised with reference clocks on the Internet. 
;; NURBS
;;     Non-Uniform Rational B-Splines, a technique used in CAD etc. - More information. 
;; Nu Thena
;;     A software vendor specialising in rapid prototyping tools for real-time hardware and software systems, collaborating with DAZIX. 
;; NuThena
;;     See Nu-Thena 
;; OAK
;;     An early name for Java 
;; OATH
;;     Object-oriented Abstract Type Hierarchy, a class library for C++ from Texas Instruments - More information. 
;; Oberon
;;     A programming language developed by N. Wirth and J. Gutknecht as a successor to Modula 2 - More information. 
;; Object management system
;;     In an IPSE, the system which maintains information about the system under development. 
;; Object-oriented database
;;     A system offering DBMS facilities in an object-oriented environment. 
;; Object-oriented programming
;;     see object-oriented 
;; Object-oriented
;;     Applied to analysis, design and programming. The basic concept in this approach is that of objects, which consist of data structures encapsulated with a set of routines, often called "methods" which operate on the data. Operations on the data must be performed via these methods, which are common to all instances of objects of a particular class. Thus, the interface to objects is well defined, and allows the code implementing the methods to be changed so long as the interface remains the same - More information. 
;; Object
;;     In object-oriented programming, an instance of a data structure defined according to the template provided by its class, and which can respond to the messages defined by its class. 
;; ObjectBroker
;;     A distributed object system from DEC based on the CORBA standard. 
;; ObjectCenter
;;     A product offering similar facilities to CodeCenter for the C++ language, plus class browsing facilities etc (formerly Saber-C++) - More information. 
;; Objecteering
;;     An Object Oriented design tool from Softeam, based on the Class Relation Methodology, with C++ code generation - More information. 
;; Objective C
;;     A Smalltalk-like extension of the C language which provides the possibility to use object-oriented programming constructs 
;; Objective PASCAL
;;     An extension of the PASCAL language which provides the possibility to use object-oriented programming constructs. 
;; Objectworks
;;     An object-oriented development environment developed by ParcPlace, available under Smalltalk and C++. 
;; OBST
;;     A persistent object management system developed by FZI Karlsruhe for the STONE project - More information. 
;; Occam
;;     A programming language which facilitates writing parallel programs, allowing the programmer to specify whether processes are to be executed sequentially or in parallel. Based on CSP, it was originally developed for the Transputer - More information. 
;; OCLC
;;     Online Computer Library Center - More information. 
;; OCR
;;     Optical Character Recognition: recognition of printed or written characters by computer - More information. 
;; OCS
;;     Object Compatibility Standard: an 88open standard for compilers and linkers. 
;; ODA
;;     Open (formerly Office) Document Architecture: an ISO standard (8613) for describing documents. It allows text, graphics, and facsimile documents to be transferred between different systems. 
;; ODAC
;;     The ODA consortium. 
;; ODIF
;;     Open Document Interchange Format: part of the ODA standard. 
;; ODL
;;     Object Definition Language from ODMG. 
;; ODMG
;;     Object Data Management Group. A vendor consortium developing standards for Object Data Definition and Manipulation Languages - More information. 
;; ODP
;;     Open Distributed Processing. An ISO standardisation activity. 
;; ODT
;;     Open Desktop. 
;; OEW
;;     Object Engineering Workbench. A design tool for C++ - More information. 
;; OFF
;;     Object File Format for interchange and archiving of 3D objects, from Digital Equipment Corporation - More information. 
;; OLE
;;     Object Linking and Embedding. A distributed object system from Microsoft - More information. 
;; OLTP
;;     On-Line Transaction Processing: the processing of transactions by computers in real time. 
;; OMA
;;     Object Management Architecture: a set of standards under study by OMG. 
;; OMF
;;     Object Management Facility: part of the DAA proposed by Hewlett-Packard and Sun. 
;; OMF
;;     Open Model Forum for modelling and simulation tool standards - More information. 
;; OMG
;;     Object Management Group: a consortium aimed at setting standards in object-oriented programming, especially for distributed applications - More information. 
;; OML
;;     Object Manipulation Language from ODMG. 
;; OML
;;     OPEN Modelling Language - More information. 
;; OMT
;;     An object-oriented methodology . 
;; OMTool
;;     A graphical tool from General Electric Advanced Concepts Center for design and analysis of systems with the OMT methodology with some C++/SQL code generation - More information. 
;; ONC
;;     Open Network Computing: Sun's network protocols. 
;; OnX
;;     A graphics package from LAL Orsay 
;; OO
;;     Object-oriented: for example Analysis (OOA), Design (OOD), Programming (OOP), Programming Language (OOPL), Data Bases (OODBMS) etc. 
;; OOA
;;     Object-oriented Analysis. 
;; OOD
;;     Object-oriented Design. 
;; OODBMS
;;     Object-oriented database management system. 
;; OODL
;;     Object-oriented Dynamic Language. 
;; OOP
;;     Object-oriented programming. 
;; OOPL
;;     Object-oriented programming language: a language such as C++, Eiffel, Objective-C etc designed to support object-oriented programming. 
;; OOPSLA
;;     Conference on Object-oriented Programming Systems, Languages and Applications. 
;; OOSD
;;     Object-oriented structured design: a design method elaborated from structured design and incorporating the essential features of the object-oriented approach. 
;; Open Desktop
;;     A UNIX environment from SCO. (part of the ACE initiative). 
;; Open Inventor
;;     An object-oriented toolkit for developing interactive 3D graphics applications. It also defines an ASCII file format for exchanging 3D data among applications, which is the basis for VRML - More information. 
;; Open Look
;;     A graphical user interface and window manager from Sun and AT&T. 
;; Open Software Foundation
;;     See OSF . 
;; OpenDoc
;;     A compound document architecture from CIL based on CORBA. It aims to enable embedding of features from different applications into a single working document - More information. 
;; OpenGL
;;     An emerging graphics standard providing advanced rendering capabilities - More information. 
;; OpenWindows
;;     A server program for the Sun which handles SunView, NeWS and X Window System protocols. 
;; OQL
;;     Object Query Language from ODMG. 
;; Oracle*CASE
;;     A set of CASE tools from Oracle. 
;; Oracle Card
;;     A hypercard-like product from Oracle for constructing DB applications, running on PC and Macintosh. 
;; Oracle Toolkit
;;     See Adaptable User Interface. 
;; Oracle
;;     A vendor of database management systems: also their relational DBMS. 
;; ORB
;;     Object Request Broker: part of the OMG standard. 
;; ORKID
;;     Open Real-time Kernel Interface Definition. 
;; OS/2
;;     An operating system from IBM and Microsoft for the PS/2 range of microcomputers - More information. 
;; OSA
;;     Open Scripting Architecture. A CIL approach to the coexistence of multiple scripting systems. 
;; OSE
;;     Open Systems Environment. 
;; OSF
;;     Open Software Foundation. A foundation created by nine computer vendors, (Apollo, DEC, Hewlett-Packard, IBM, Bull, Nixdorf, Philips, Siemens and Hitachi) to promote "Open Computing". It is planned that common operating systems and interfaces, based on developments of UNIX, the X Window System, etc. will be forthcoming for a wide range of different hardware architectures - More information. 
;; OSI
;;     Open Systems Interconnection: a seven-layer reference model developed by ISO as a framework for the development of standards for interconnecting heterogeneous computers - More information. 
;; OSTC
;;     Open Systems Testing Consortium. An open organisation operating harmonised conformance testing services for OSI telecommunications and IT protocols. 
;; OTI
;;     Open Tool Interface. 
;; OVL
;;     Object Verification Language from ODMG. 
;; OWL
;;     A software company offering the Guide hypertext system . 
;; P2P
;;     Person to Person. A range of desktop conferencing products from IBM - More information. 
;; P-CAD
;;     A CAE system marketed by CADAM, an IBM company. 
;; PACS
;;     Public Access Computer Systems. 
;; Page Description Language
;;     A language such as Adobe PostScript or Xerox Interpress which allow the appearance of a printed page to be described in a high-level device-independent way. Printing then becomes a two-stage process: an application produces a description in the language, which is then interpreted by a specific output device. Such a language can serve as an interchange standard for transmission and storage of printable documents - More information. 
;; Panda
;;     An Internet navigation and information retrieval system from the University of Iowa - More information. 
;; Pansophic
;;     A Software Engineering company in the US. 
;; Parser
;;     A function that recognizes valid sentences of a language by analysing the syntax structure of a set of tokens passed to it from a lexical analyzer. 
;; PARADIGM PLUS
;;     A configurable object-oriented CASE tool from Proto Soft Inc. - More information. 
;; PARC
;;     See Xerox PARC. 
;; PaRC
;;     A workstation cluster for engineering computing at CERN - More information. 
;; Parlog++
;;     An object-oriented extension to MacParlog. It combines object-oriented and parallel logic programming, giving the benefits of both paradigms within a single coherent development environment. 
;; Pascal
;;     A programming language designed by N.Wirth for teaching purposes, emphasising structured programming constructs, data structures and strong typing. 
;; Pattern
;;     A formal way to describe a solution to a commonly recurring programming problem - More information. 
;; PATCHY
;;     A FORTRAN code management program written at CERN. 
;; PAW
;;     Physics Analysis Workbench - general purpose portable tool for analysis and presentation of physics data - More information. 
;; PAW++
;;     An extended version of PAW with a Motif human interface. 
;; PC
;;     Personal Computer. 
;; PCA
;;     A dynamic analyser from DEC giving information on run time performance and code utilisation. 
;; PCL
;;     Printer Control Language (from Hewlett Packard). 
;; PCL
;;     Portable Common LOOPS. A portable CLOS implementation. 
;; PCTE+
;;     A European NATO specification based on PCTE with security enhancements. 
;; PCTE
;;     Portable Common Tool Environment: an ECMA standard framework for software tools developed in the Esprit programme. It is based on an entity-relationship Object Management System and defines the way in which tools access this - More information. 
;; PCX
;;     A bitmap format from Zsoft - More information. 
;; PDDM
;;     Product Data and Document Management. 
;; PDF
;;     Portable Document Format from Adobe Systems - More information. 
;; PDL
;;     Page Description Language. 
;; PDL
;;     Program Design Language. 
;; PDM
;;     Product Data Management. An integrated system for managing all types of technical data concerning a product. 
;; PDS
;;     Planetary Data Systems format from - More information. 
;; PDSA cycle
;;     Plan, Do, See, Approve (from Japan). 
;; PEM
;;     Privacy Enhanced Mail. An Internet standard (RFC 1421-1424). 
;; PEP
;;     Protocol Extension Protocol. A proposed system to allow HTTP clients and servers to negotiate protocol extensions. 
;; Perceptron
;;     This term is sometimes used to refer to a single McCulloch-Pitts neuron, but may also refer to a network of neurons in which the output(s) of some neurons are connected through weighted connections to the input(s) of other neurons. The term multilayer perceptron specifically refers to a network composed of more than one layer of neurons, with some or all of the outputs of each layer connected to one or more of the inputs of another layer. The first layer is called the input layer, the last one is the output layer, and in between there may be one or more hidden layers. 
;; Perl
;;     Practical Extraction and Report Language. An interpreted scripting language for scanning text files, extracting information, and printing reports. It combines features of c , sed , awk and sh - More information. 
;; Personal Computer
;;     A general-purpose single-user microcomputer designed to be operated by one person at a time. 
;; Petri net
;;     A graphical representation of concurrent systems in terms of tokens, places and transition bars - More information. 
;; PEX
;;     (PHIGS Extension to X) Extension to the X Window System providing 3-D graphics support. 
;; PGP
;;     Pretty Good Privacy. A set of encryption tools for electronic mail etc. - More information. 
;; PHIGS
;;     Programmers Hierarchical Interactive Graphics System: an ANSI/ISO standard. 
;; PICS
;;     Platform for Internet Content Selection. 
;; PII
;;     Process Improvement Institute - More information. 
;; PIM
;;     Product Information Management. See PDM 
;; PinK
;;     PinK is not KUIP. An interface between Tcl/Tk, BLT, ADAMO and DAD from DESY - More information. 
;; Plexus
;;     A set of modular WWW server software written in Perl - More information. 
;; PMM
;;     Process Maturity Model. 
;; PNG
;;     Portable Network Graphics. A standard for bitmapped image files - More information. 
;; Polymorphism
;;     In object-oriented programming, the term is used to describe variables which may refer at run-time to objects of different classes. For example, the variable "myVehicle" could refer to an object of class "motorCar" or "Truck". 
;; POSIX
;;     Portable Operating System Interface for computer environments. A set of IEEE standards designed to provide application portability. IEEE1003.1 defines a UNIX-like operating system interface, 1003.2 the shell and utilities, and 1003.4 real-time extensions. 
;; POSS
;;     Persistent Object Service Specification: an OMG specification. 
;; POSTGRES
;;     An active DBMS from Univ. of Calif. Berkeley. 
;; Postscript
;;     A page description language from Adobe Systems Inc. Its primary application is to describe the appearance of text, graphical shapes and sampled images on printed or displayed pages. A program in PostScript can communicate a document description from a composition system to a printing system in a device-independent way. Many printers now interpret PostScript directly - More information. 
;; PPP
;;     Point to Point Protocol. 
;; PPTP
;;     Point to Point Tunneling Protocol. 
;; Pragma
;;     A standardised form of kluge in Ada. 
;; Predicate calculus
;;     A notation for representing logical statements which goes beyond propositional calculus in certain ways. 
;; PREMO
;;     Presentation Environment for Multimedia Objects. An ISO standard under development for creation, presentation and interaction with information using single or multiple media - More information. 
;; Presentation Manager
;;     The user interface to the OS/2 system. 
;; ProDoc
;;     A set of tools for software documentation from SPC 
;; Project assurance
;;     The process of specifying the support system: techniques, internal standards, measurements, tools, and training for a project; counseling the project team in the application of these elements and monitoring the adherence to the standards. 
;; Project management
;;     The process of planning, organizing, staffing, directing, and controlling the production of a system. Software tools are available to help with this - More information. 
;; Project planning
;;     See Project management. 
;; PROLOG
;;     A language for PROgramming in LOGic. 
;; Prometheus
;;     A high-level programming language designed for logic, mathematics, and artificial intelligence. It contains elements from C, Pascal, LISP and Prolog plus novel features - More information. 
;; Propositional calculus
;;     A system of symbolic logic. 
;; PROST
;;     Programme for Research in Open Systems Testing of the DTI 
;; Protocol
;;     An agreement about how to transmit data, especially across networks. Low level protocols define the electrical and physical standards to be observed, and deal with the transmission and error detection and correction of the bit stream. High level protocols deal with the data formatting, including the form of messages, the terminal to computer dialogue, files, etc. 
;; Prototyper
;;     An interface builder for the Macintosh from Smethers Barnes . 
;; Prototyping
;;     The creation of a model and the simulation of all aspects of a product. CASE tools support different degrees of prototyping. Some offer the end-user the ability to review all aspects of the user interface and the structure of documentation and reports before code is generated. 
;; PS
;;     PostScript. 
;; PSA
;;     Problem Statement Analyzer: see PSL/PSA. 
;; Pseudocode
;;     A notation resembling a programming language but not intended for actual compilation. It usually combines some of the structure of a programming language with an informal natural-language description of the computations to be carried out. It is often produced by CASE systems as a basis for later hand coding. 
;; PSL/PSA
;;     Problem Statement Language/Problem Statement Analyser: a CASE system developed by D.Teichroew. It allows computer-based development and analysis of a statement of requirements, and assistance during the design phase. 
;; PSL
;;     Problem Statement Language: see PSL/PSA. 
;; PSL
;;     Problem Statement Language: see PSL/PSA. 
;; PSP
;;     Personal Software Process. Methods to improve the quality of work of software engineers. 
;; PTI
;;     Portable Tool Interface: a standard such as PCTE, allowing interworking between different software tools via defined interfaces to the user and to the repository or object management system. 
;; PureLink
;;     An incremental linker from Pure Software - More information. 
;; Purify
;;     A debugging tool from Pure Software - More information. 
;; PURL
;;     Persistent URL. Instead of pointing directly to the location of an Internet resource, a PURL points to a resolution service that associates the PURL with the actual URL and returns that URL to the client. See the OCLC PURL Service. 
;; PV~WAVE
;;     Interactive scientific visualisation software from Visual Numerics - More information. 
;; QA
;;     Quality Assurance. 
;; QAM
;;     Quality Assurance Management. 
;; QBE
;;     A query language. 
;; QIP
;;     Quality Improvement Paradigm. 
;; Quantify
;;     A performance analysis tool from Pure Software - More information. 
;; Query language
;;     A language such as SQL whereby users of a database system can interactively formulate requests, generate reports etc. 
;; RAD
;;     Rapid Application Development. Often applied to tools such as Microsoft Visual Basic, Borland Delphi, Oracle Power Objects. 
;; RAL
;;     Rutherford Appleton Laboratory (UK) - More information. 
;; RAID
;;     Redundant Array of Inexpensive Disks. A data storage technique. 
;; RARE
;;     Reseaux Associes pour la Recherche Europeenne: an association of national and international European networks and users - More information. 
;; RBSE
;;     Repository Based Software Engineering. A NASA research and development programme - More information. 
;; RCS
;;     A code management system. 
;; Rdb
;;     DEC's SQL-based relational DBMS for VAX/VMS. 
;; RDBA
;;     Remote Database Access: a standard permitting the exchange of information between different DBMS systems. 
;; RDBMS
;;     Relational database management system. 
;; Re-engineering
;;     The examination and modification of a system to reconstitute it in a new form and the subsequent implementation of the new form - More information. 
;; Real-time
;;     Generally used to describe systems that must guarantee a response to an external event within a given time 
;; Realtime
;;     see real-time 
;; Redocumentation
;;     The creation or revision of a semantically equivalent representation within the same relative abstraction level. The resulting forms of representation are usually considered alternate views intended for a human audience. 
;; Reengineering
;;     see Reverse engineering 
;; REFINE
;;     A set of reverse engineering tools from Reasoning Systems 
;; Relation
;;     A table in a relational database 
;; Relational database
;;     See Relational DBMS. 
;; Relational DBMS
;;     A DBMS based on the relational model developed by Codd. It allows the definition of data structures, storage and retrieval operations, and integrity constraints. In such a database, the data and relations between them are organised in tables. INGRES and Oracle are well-known examples. 
;; Released version
;;     A version of an object that is not modifiable, as designated by some person. Also known as baseline. See change management. 
;; Rendezvous
;;     In Ada, the method of synchronising the activity of different tasks. 
;; Repository
;;     The core of a CASE tool is typically a DBMS where all development documents are stored. 
;; REQUEST
;;     REliability and QUality of European Software Technology. An Esprit project (now terminated). 
;; Requirements
;;     The first stage of software development should be to define requirements with the potential users. In modern methods these requirements should be testable, and will usually be traceable in later development stages - More information. 
;; Restructuring
;;     The transformation from one representation form to another at the same relative abstraction level, while preserving the subject system's external behavior (functionality and semantics). 
;; Reusability
;;     The possibility of using code developed for one application in another application: traditionally achieved using program libraries. Object-oriented programming offers the potential for greater reusability of code via its techniques of inheritance, genericity etc. Class libraries with intelligent browsers and application generators are under development to help in this process. 
;; Reuse
;;     The planned use of software artefacts for the solution of multiple problems - More information. 
;; Reverse Engineering
;;     The process of analyzing an existing system to identify its components and their interrelationships, and create representations of the system in another form or at a higher level of abstraction. Usually undertaken in order to redesign the system for better maintainability - More information. 
;; RFC
;;     Request For Comment. The name by which Internet standards are known - More information. 
;; RFT
;;     Request For Technology - process established by OSF to get proposals for new standards. 
;; RIFF
;;     Resource Interchange File Format from Microsoft - More information. 
;; RIPE
;;     Reseaux IP Europeens. A collaborative organisation of European Internet service providers - More information. 
;; RISC
;;     Reduced Instruction Set Computer; one whose design is based on the rapid execution of a sequence of simple instructions rather than on the provision of a large variety of complex instructions. 
;; RLF
;;     Reuse Library Framework of the DoD 
;; RMP
;;     Reliable Multicast Protocol - More information. 
;; RM-ODP
;;     The ISO Reference Model for Open Distributed Environments. 
;; RNIS
;;     Reseau Numerique a Integration de Services. French for ISDN. 
;; ROOM
;;     Real-Time Object-Oriented Modeling. An Object-Oriented analysis and design approach - More information. 
;; ROOT
;;     An object oriented framework for large scale data analysis at CERN - More information. 
;; Root version
;;     The initial value of an object. See change management. 
;; RPC
;;     Remote Procedure Call: a call to a routine that results in code being executed on a different system from the one where the request originated. An RPC system allows calling procedures and called procedures to execute on different systems without the programmer needing to explicitly code for this. 
;; RSA
;;     Rivest, Shamir, Adleman public key encryption technique (used by PGP) 
;; RSVP
;;     Rapid System Virtual Prototyping. 
;; RTEE
;;     Real Time Engineering Environment: a set of CASE tools produced by Westmount Technology B.V. 
;; RTF
;;     Rich Text Format: an interchange format from Microsoft for exchange of documents between Word and other document preparation systems - More information. 
;; RTL
;;     Register Transfer Language: a kind of HDL used in describing the registers of a computer or digital electronic system, and the way in which data is transferred between them. 
;; RTSA
;;     Real-time structured analysis: versions of structured analysis capable of modelling real-time aspects of software. 
;; Rule-based
;;     Having to do with systems that infer or use "rules" (i.e.logical statements). 
;; SA
;;     Structured Analysis. 
;; SAA
;;     Systems Application Architecture: IBM's family of standard interfaces which enable software to be written independently of hardware and operating systems. 
;; Saber-C++
;;     see ObjectCenter. 
;; Saber-C
;;     see CodeCenter. 
;; SADT
;;     Structured Analysis and Design Technique. 
;; SARA
;;     Stichting Academisch Rekencentrum Amsterdam (Academic Computing Services Amsterdam) - More information. 
;; SAGE
;;     System Administrators Guild. A Special Technical Group within USENIX. 
;; SASD
;;     Structured Analysis, Structured Design. 
;; SATAN
;;     Security Administrator Tool for Analyzing Networks - More information. 
;; Sather
;;     An object-oriented programming language that is a simplified optimized variant of Eiffel - More information. 
;; SBM
;;     Solution Based Modelling. a software development process described in the book "Developing Object-Oriented Software for the Macintosh" written by Neal Goldstein and Jeff Alger, published by Addison Wesley in 1992. 
;; SCCS
;;     Source Code Control System: a popular code management system for UNIX systems. 
;; SCM
;;     Software Configuration management or Source Code management. 
;; Schematic capture
;;     The process of entering the logical design of an electronic circuit into a CAE system by creating a schematic representation of components and interconnections. 
;; Scheme
;;     A dialect of Lisp. 
;; Schlaer-Mellor
;;     An Object-Oriented Analysis (OOA) modeling method that addresses the the integration of structural and behavioral properties. 
;; SCI
;;     Scalable Coherent Interface, IEEE Std 1596-1992 - More information. 
;; SCO
;;     The Santa Cruz Operation, a leading supplier of UNIX systems for systems based on Intel microprocessors. Suppliers of Xenix and Open Desktop - More information. 
;; SCOPE
;;     Software Assessment and Certification Programme. An Esprit project - More information. 
;; SCPI
;;     Standard Commands for Programmable Instruments: a standard complementing IEE 488 developed by Hewlett-Packard and promoted by the SCPI Consortium, 8380 Hercules Drive, Suite P3, La Mesa, CA 91942, USA 
;; ScriptX
;;     A dynamic object-oriented programming language and class library for multimedia from Kaleida Labs - More information. 
;; SCSI
;;     Small Computer Systems Interface. 
;; SD
;;     Structured Design: a program design method. 
;; SDD
;;     Software Design Description. ANSI/IEEE 1016-1987 specifies IEEE Recommended Practice for SDD. 
;; SDE
;;     Software Development Environment: equivalent to SEE. 
;; SDIF
;;     SGML Document Interchange Format. 
;; SDL
;;     Specification and Design Language: defined by the CCITT (recommendation Z100) to provide a tool for unambiguous specification and description of the behaviour of telecommunications systems. The area of application also includes process control and real-time applications. SDL provides a Graphic Representation (SDL/GR) and a textual Phrase Representation (SDL/PR), which are equivalent representations of the same semantics. A system is specified as a set of interconnected abstract machines which are extensions of the Finite State Machine (FSM) - More information. 
;; SDM
;;     Schematic Data Model. 
;; SDS
;;     Schema Definition Set in PCTE. 
;; SE-ODP
;;     Support Environment for Open Distributed Processing: an ECMA standard. 
;; SE
;;     Software Engineering, the methods used in developing software. 
;; SEE
;;     Simultaneous Engineering Environment: a CAE framework from DAZIX. 
;; SEE
;;     Software Engineering Environment: a set of management and technical tools to support software development, usually integrated in a coherent framework; equivalent to an IPSE. 
;; SEI
;;     Software Engineering Institute (Carnegie Mellon University) - More information. 
;; SEL
;;     Software Engineering Laboratory. The Institute for Information Technology of the National Research Council Canada - More information. Also NASA's Goddard Space Flight Center - More information. 
;; Selector
;;     In Smalltalk or Objective-C, the syntax of a message which selects a particular method in the target object. 
;; Self
;;     An object oriented programming language from Stanford, and an object oriented programming system from Sun Microsystems - More information. 
;; Semaphore
;;     The classic method for restricting access to data shared between several cooperating processes . 
;; SEP
;;     A SASD tool from IDE. 
;; SERC
;;     Software Engineering Research Center (Purdue University). 
;; Server
;;     A computer which, by means of network connections, carries out parts of a computing task on behalf of one or more remote computers. 
;; SES/workbench
;;     An iconic simulation and design tool, linked to some of the major CASE systems now available or in development. 
;; SET
;;     Standard d'Echange et de Transfert: a French standard for exchange of CAD data. 
;; Setext
;;     A markup scheme intended for documents that are both human- and computer-readable - More information. 
;; SFA
;;     Software Frameworks Association - More information. 
;; SGI
;;     Silicon Graphics Incorporated, a vendor of graphical workstations and software 
;; SGML
;;     Standard Generalised Markup Language (ISO 8879). A generic markup language for representing documents. SGML is a system for defining structured document types, and markup languages to represent instances of those document types - More information. 
;; SGML Open
;;     A non-profit, international consortium of providers of products and services, dedicated to accelerating the further adoption, application, and implementation of SGML - More information. 
;; ShapeTools
;;     A code management system for UNIX from TU Berlin. 
;; Shape_VC
;;     A code management system which offers version control functionality similar to systems like RCS or SCCS with some extensions and a more UNIX-like command interface. 
;; SHARE
;;     An international users group of IBM and compatible hardware and software - More information. 
;; Shelf
;;     A public library of classes for the Eiffel language. 
;; Shell Script
;;     A program written to be interpreted by the shell of an operating system, especially UNIX. 
;; Shell
;;     The outer part of an operating system, especially UNIX, which provides the user interface, as opposed to the kernel which provides the basic services to processes. The commonest UNIX shells are the c shell (csh) and the Bourne shell (sh) . 
;; Shen
;;     A security scheme for WWW - More information. 
;; SHIFT
;;     Scalable Heterogeneous Integrated Facility Testbed. A parallel processing project at CERN. 
;; SICL
;;     Standard Instrument Control Library: a platform-independent API for software to control and test electronic instruments conforming to IEE 488 
;; SICS
;;     Swedish Institute for Computer Science - More information. 
;; SIGhyper
;;     Special Interest Group on Hypertext and Multimedia of the SGML Users' Group - More information. 
;; SIMD
;;     Single Instruction Multiple Data: a form of parallelism in multiprocessor computing where there is a single instruction stream (programs) operating concurrently on several data streams. 
;; SIMEX
;;     A set of C++ classes from the University of Minnesota, that provides a framework for building discrete event simulation models - More information. 
;; SIMON
;;     System of Internet Mapping for Organised Navigation - More information. 
;; Simscript
;;     A free-form, English-like general-purpose simulation language. SIMSCRIPT II.5 from CACI has evolved from the original work on SIMSCRIPT by H.Markowitz. 
;; SIMULA
;;     A program based on Algol 60 with extensions for simulation, which was a precursor of the object-oriented approach. 
;; Single Inheritance
;;     The property of an object-oriented language which restricts a sub-class to be derived from only one parent. 
;; Sisal
;;     Streams and Iterations in a Single-Assignment Language. A general-purpose functional language from CWI. 
;; SLIP
;;     Serial Line IP. 
;; SMA
;;     Software Maintenance Association. 
;; Smalltalk
;;     A pioneering object-oriented programming system developed at the Xerox Palo Alto research centre. It includes a language (usually interpreted), a programming environment, and an extensive object library - More information. 
;; SMCC
;;     Sun Microsystems Computer Corporation 
;; SMDL
;;     Standard Music Description Language, based on HyTime 
;; SMG
;;     Screen Management Guidelines - a VMS package of run-time library routines providing windows on VT100 terminals. 
;; SML/NJ
;;     Standard ML of New Jersey. 
;; SML
;;     Standard ML: a functional language. 
;; SMSL
;;     Standard Multimedia Scripting Language - More information. 
;; SNA
;;     Systems Network Architecture - IBM's networking standard. 
;; Sniff
;;     A C++/C programming environment providing browsing, cross-referencing, design visualization, documentation, and editing support. Developed by UBS Switzerland and marketed by takeFive Salzburg. (See also SNiFF+ - More information. ) 
;; SNOBOL
;;     String Oriented Symbolic Language. A language from the 1960s for string manipluation. 
;; SoftBench
;;     An IPSE from Hewlett-Packard. 
;; Softlab
;;     A software engineering company strong in UK and Germany. 
;; Software AG
;;     SE company from FRG. 
;; Software BackPlane
;;     A CASE framework from Atherton. 
;; Software bus
;;     A support environment for heterogeneous distributed processing, such as the ANSA Testbench. 
;; Software Engineering
;;     A systematic approach to the analysis, design, implementation and maintenance of software. It usually involves the use of CASE tools. There are various models of the software life-cycle, and many methodologies for the different phases. 
;; Software Metrics
;;     Measures of software quality which indicate the complexity, understandability, testability, description and intricacy of code. 
;; Software through Pictures
;;     see StP. 
;; SPARC
;;     see ANSI/SPARC Architecture. 
;; SOIF
;;     Summary Object Interchange Format in the Harvest system. 
;; SOM
;;     System Object Model. An implementation of CORBA by IBM - More information. 
;; SOMA
;;     Semantic Object Modelling Approach. An Object-Oriented analysis and design approach - More information. 
;; Sparcstation
;;     A family of workstations from Sun . 
;; SPC
;;     Software Productivity Centre. A non-profit organization based in Vancouver, BC, Canada with the mandate to assist software developers to improve their software engineering process - More information. 
;; SPDL
;;     Standard Page Description Language: a draft within the ODA standard. 
;; SPEC
;;     Standard Performance Evaluation Corporation. Formed to establish, maintain and endorse a standardized set of relevant benchmarks that can be applied to the newest generation of high-performance computers - More information. 
;; Specific markup
;;     In computerised document preparation, a method of adding formatting commands to the text to control layout, such as new line, new page, center text etc. (see Generic markup). 
;; SPI
;;     Software Process Improvement. 
;; SPIN
;;     Software Process Improvement Network. Local interest groups sponsored by SEI. 
;; SPT
;;     Software Process Technology. 
;; Spreadsheet
;;     A type of application which manipulates data in rows and columns of cells. The value in a cell is calculated by a formula which can involve other cells. Popular in commercial applications. 
;; Spring
;;     A distributed object-oriented operating system from Sun - More information. 
;; Sprite
;;     An operating system from Berkeley supporting multiprocessing and distributed files. 
;; SQL/DS
;;     A database package from IBM including a relational DBMS. 
;; SQL2
;;     An extended version of the SQL standard. 
;; SQL
;;     Structured Query Language: ISO, ANSI standard user front end to a relational database management system. 
;; SRI
;;     Stanford Research Institute. 
;; SSADM
;;     A software engineering method and toolset required by some UK government agencies. 
;; SSII
;;     Societe de Service en Ingenierie Informatique. 
;; SSL
;;     Secure Sockets Layer. A scheme for secure WWW communications - More information. 
;; Standards
;;     Although boring, standards are necessary for interworking, portability and reusability. They may be de facto standards for various communities, or officially recognised national or international standards. Some important bodies concerned in one way or another with Software standards are ISO, ANSI, DoD, ECMA, IEEE, IETF, OSF - More information. 
;; StarBurst
;;     An active DBMS from IBM Almaden Research Center. 
;; STARS
;;     Software Technology for Adaptable Reliable Systems. A DARPA project - More information. 
;; STAS
;;     Scientific and Technical Attribute and element Set. Defines standard identifiers for referring to searchable fields in scientific databases. 
;; State Diagram
;;     see State Transition Diagram. 
;; State transition diagram
;;     A diagram consisting of circles to represent states and directed line segments to represent transitions between the states. One or more actions may be associated with each transition. The diagrom represents a Finite State Machine. 
;; STD
;;     State Transition Diagram. 
;; STDWIN
;;     A windowing interface from CWI with windows, menus, modal dialogs, mouse and keyboard input, scroll bars, drawing primitives, etc that is portable between platforms. STDWIN is available for Macintosh and the X Window System. 
;; STEP
;;     Standard for the exchange of product model data: a draft ISO standard for the exchange of CAD data. 
;; StepStone
;;     Corporation founded by Brad Cox, responsible for Objective-C. 
;; STL
;;     Standard Template Library for C++ - More information. 
;; STL
;;     Semantic Transfer Language. IEEE 1175: IEEE Trial-Use Standard Reference Model for Computing System Tool Interconnections. 
;; STONE
;;     A Structured and Open Environment: a project supported by the German Ministry of Research and Technology (BMFT) to design, implement and distribute a SEE for research and teaching. 
;; StP
;;     Software through Pictures: a set of CASE tools from IDE - More information. 
;; Strand
;;     A concurrent programming language from Strand Software Technologies Limited. 
;; Struct
;;     A data type in C corresponding to a record in Ada or Pascal. 
;; Structured analysis
;;     One of a number of requirements analysis methods used in software engineering. 
;; Structured design
;;     One of a number of systematic top-down design techniques used in software engineering, usually after structured analysis. 
;; Sublanguage
;;     One of the languages associated with a DBMS, for example data-definition language or query language. 
;; Sun
;;     Sun Microsystems, a US workstation manufacturer with manufacturing capacity in Europe. 
;; SunOS
;;     The version of UNIX running on Sun workstations. 
;; SunView
;;     A windowing system from Sun Microsystems, superseded by NeWS. 
;; Superclass
;;     The class from which another class inherits (see Inheritance). 
;; SVID
;;     System V Interface Definition: allowing source code portability between different platforms running UNIX System V. 
;; SWOT
;;     Strengths, Weaknesses, Opportunities, Threats commercial product analysis. 
;; Sybase
;;     A relational DBMS vendor. 
;; System V
;;     One of the two major versions of the UNIX system, due to AT&T. (see BSD). 
;; TAE Plus
;;     A GUI builder from Century Computing - More information. 
;; TAFIM
;;     Technical Architecture Framework for Information Management: a DoD standard - More information. 
;; Taligent
;;     A software company set up by Apple, IBM and Hewlett-Packard - More information. 
;; Taos
;;     An operating system kernel for parallel systems from Tao Systems - More information. 
;; TAPI
;;     Telephony Application Programming Interface. A CTI standard from Microsoft and Intel. 
;; TBK
;;     Tool Builder Kit: a product from IPSYS which allows users to develop CASE tools appropriate to any software engineering methodology - More information. 
;; TCA
;;     Trigger, Condition, Action model. 
;; TC/IX
;;     The LynxOS kernel ported to the MIPS R3000 RISC processor by CDC. 
;; Tcl
;;     Tool command language. A command language and associated library package running on a number of platforms - More information. 
;; Tcl/Tk
;;     See Tk. 
;; TCP/IP
;;     A reliable connection-oriented protocol originated by DARPA for internetworking, encompassing both network and transport level protocols. While the terms TCP and IP specify two protocols, TCP/IP is often used to refer to the entire DoD protocol suite based upon these, including Telnet, FTP, UDP, and RDP. 
;; Teamwork
;;     A SASD tool from CADRE Technologies. 
;; Tecate
;;     A software system for exploratory visualization of data from networked sources including WWW - More information. 
;; TEI
;;     Text Encoding Initiative. Defines a common interchange format for literary and linguistic data - More information. 
;; TELEPAC
;;     The Swiss PTT X.25 Network. 
;; TeleUSE
;;     An interface builder for Motif . 
;; Telnet
;;     The Internet standard protocol for remote terminal connection service, running over TCP/IP. Telnet allows a user to log onto a remote host computer. 
;; TELOS
;;     The object system of LeLisp Version 16 and EULISP. 
;; Template code
;;     Pseudocode generated by an automated CASE system and requiring further hand-coding before compilation. 
;; TestCenter
;;     A testing environment for C and C++ programs from CenterLine Software - More information. 
;; Testing
;;     The process of exercising a product to identify differences between expected and actual results and performance. Typically testing is bottom-up: unit test, integrate test and finally system test - More information. 
;; TET
;;     Test Environment Toolkit project coordinated by X/Open 
;; TeX
;;     A computer typesetting program by D.E.Knuth popular for document preparation in the HEP community. It provides specific markup for text processing - More information. . 
;; Texel
;;     An object-oriented methodology (see "Object Oriented Methods" by Ian Graham). 
;; Think C
;;     An extension of ANSIC for the Macintosh by Symantec Corporation, similar to C++, to support object-oriented programming techniques. 
;; TickIT
;;     A software industry quality assessment scheme - More information. 
;; TIFF
;;     Tag Image File Format from Aldus - More information. 
;; Tk
;;     An extension to Tcl providing an interface to the X windows . 
;; TLA
;;     Three Letter Acronym. 
;; Token
;;     A basic, grammatically indivisible unit of a language. 
;; Token ring
;;     A computer network arbitration scheme in which conflicts in the transmission of messages are avoided by the granting of "tokens" which give permission to send. A station keeps the token while transmitting a message, if it has a message to transmit, and then passes it on to the next station. 
;; Toolbuilder
;;     see TBK 
;; TOP
;;     Technical/Office Protocol: a protocol stack for office automation developed by Boeing following the OSI model. This protocol is very similar to MAP except at the lowest levels, where it uses Ethernet (IEEE 802.3) rather than Token Bus (IEEE 802.4). 
;; Transaction
;;     A unit of interaction with a DBMS or similar system. It must be treated in a coherent and reliable way independent of other transactions . 
;; Transputer
;;     A family of microprocessors from Inmos with interprocessor links, programmable in Occam. 
;; Trellis
;;     An object-oriented application development system from DEC, based on the Trellis language. 
;; TSAPI
;;     Telephony Services Application Programming Interface. A CTI standard from Novell and AT&T. 
;; TRUSIX
;;     TRUSted unIX operating system. 
;; TSEE
;;     Technical and Engineering Environment: part of the RTEE toolset. 
;; TULIP
;;     The University Licensing Program. A cooperative research project for networked delivery and use of journals, by Elsevier Science and nine US Universities. - More information. 
;; Tunes
;;     A project to design a new computing environment at all levels of software - More information. 
;; TXL
;;     A hybrid functional and rule-based language for source transformation applications from Queen's Univ. Canada. 
;; UAA
;;     Unified Agent Architecture. 
;; UCS
;;     Universal Character Set (Universal Multiple-Octet Coded Character Set) of ISO 10646. 
;; UDP
;;     User Datagram Protocol: the Internet standard protocol for sending datagrams between user programs. This protocol neither guarantees delivery nor does it require a connection. As a result it is lightweight and efficient, but all error processing and retransmission must be taken care of by the application program. This protocol is built on top of IP and uses IP for datagram delivery (see TCP/IP). . 
;; UI
;;     UNIX International: a consortium including Sun and AT&T, promoting an open environment base on UNIX System V including the Open Look windowing system. 
;; UIL
;;     User Interface Language: in OSF/Motif and DECwindows, a language for specifying widget hierarchies etc. 
;; UIMS
;;     User Interface Management System: a system supporting the development and execution of user interfaces, usually on top of windowing systems. 
;; UIMX
;;     An interface builder for Motif from Visual Edge. 
;; UIS
;;     A VMS graphics programming interface package for VAXstations. 
;; Ultrix
;;     A version of UNIX based on the Berkeley version, designed and implemented by DEC to run on their VAX and DECstation series of processors. 
;; UNI
;;     Ente Nazionale Italiano di Unificazione: the Italian national standards body, a member of ISO. 
;; UNIX International
;;     A consortium of AT&T and others formed to advise on the development of UNIX System V. 
;; UNIX
;;     Computer operating system developed by Bell Labs. Since it was written in C, it was possible to port it to run on different hardware architectures. It is now offered by many manufacturers and is the subject of an international standardisation effort. See also OSF - More information. 
;; UNO
;;     Universal Network Objects. 
;; URC
;;     Uniform (previously Universal) Resource Characteristic (Citation) - More information. 
;; URI
;;     Uniform (previously Universal) Resource Identifier - More information. 
;; URL
;;     Uniform (previously Universal) Resource Locator - More information. 
;; URN
;;     Uniform (previously Universal) Resource Name - More information. 
;; Usenet
;;     The practice of using computer networks to exchange items of information grouped into "newsgroups" by topic. This is supported by a number of diverse and informally applied mechanisms and conventions - More information. 
;; USENIX
;;     The UNIX and Advanced Computing Systems Professional and Technical Association - More information. 
;; USL
;;     UNIX System Laboratories: the software subsidiary of AT&T, responsible for UNIX System V and related software. 
;; USMARC
;;     See MARC. 
;; UTF
;;     Universal Text Format, an SGML standard for the news distribution indistry - More information. 
;; UTF
;;     UCS Transformation Format of ISO 10646. 
;; UUCP
;;     The large international network of UNIX machines using the UUCP protocol to exchange news and electronic mail. . 
;; V
;;     A testbed for distributed system research . 
;; Validation
;;     The process of evaluating software at the end of the development process to ensure compliance with software requirements. 
;; VAX DOCUMENT
;;     A document preparation system from DEC. 
;; VAX/VMS
;;     see VMS. 
;; VAX
;;     A range of 32-bit computers manufactured by DEC. 
;; VAXset
;;     A set of software development tools from DEC, including a language-sensitive editor, compilers etc. 
;; VAXstation
;;     A family of workstations from DEC based on their VAX computer architecture. 
;; VB
;;     Visual Basic 
;; VDL
;;     Vienna Definition Language: an algebraic definition language, see VDM. 
;; VDM
;;     Vienna Definition Method: a program development method based on formal specification using the Meta-IV language - More information. 
;; VDM
;;     Virtual Device Metafile. 
;; VEE
;;     see HP VEE. 
;; Verification
;;     The process of determining whether or not the products of a given phase in the life-cycle fulfill a set of established requirements. 
;; Verilog SA
;;     A French real-time software engineering company. 
;; Verilog
;;     A Hardware Description Language for electronic design and gate level simulation. 
;; Version
;;     A variant of the original value of an object. See change management 
;; VHDL
;;     Very High Speed Integrated Circuit Description Language: a high-level VLSI design language, now standardised as IEEE Std.1076 - More information. 
;; VHE
;;     Virtual Home Environment: a tool for using NFS on HP UX . 
;; VIFF
;;     Visualization Image File Format - More information. 
;; Viola
;;     An experimental hypercard-like interpreted hypertext system by Pei Y. Wei of Berkeley. 
;; VIP
;;     Virtual Internet Protocol - More information. 
;; VIPA
;;     VMEbus International Physics Association. 
;; Visualisation
;;     A method by which a computer system presents data to the user - More information. 
;; Visualization
;;     A method by which a computer system presents data to the user - More information. 
;; Visual Basic
;;     A programming language and development environment for Windows from Microsoft - More information. 
;; VITA
;;     VMEbus International Trade Association. 
;; VITAL
;;     VHDL Initiative Towards ASIC Libraries - More information. 
;; VLIW
;;     Very Long Instruction Word. 
;; VLSI
;;     Very Large Scale Integration. Refers to semiconductor chips composed of very many tightly packed logic elements or memories - More information. 
;; VM/CMS
;;     Virtual Machine / Conversational Monitor System: an IBM operating system running on 43xx and 30xx series machines, providing efficient support for large numbers of interactive users. 
;; VM
;;     see VM/CMS. 
;; VME
;;     Common abbreviation for VMEbus. 
;; VMEbus
;;     A widely accepted backplane interconnection bus system developed by a consortium of companies led by Motorola, now standardized as IEEE Std. 1014. 
;; VMS
;;     The operating system offered by DEC as the standard system for their VAX range of processors. 
;; VPN
;;     Virtual Private Network. A computer network that appears to be a dedicated network to a particular set of users, whilst in fact using the infrastructure of public switched networks. 
;; VRML
;;     Virtual Reality Modeling Language - More information. 
;; VRTX
;;     Virtual Real-Time Executive: a real-time operating system from ReadySystems for the Motorola 68000 family of microprocessors. 
;; VSF
;;     Virtual Software Factory: a product from Systematica which allows users to develop CASE tools appropriate to any software engineering methodology. 
;; VSX
;;     Verification Suite.for X/open 
;; VTS
;;     A suite of test programs for Motif from OSF. 
;; VUE
;;     Visual User Environment: a desktop manager for UNIX from Hewlett-Packard. 
;; VUIT
;;     Visual User Interface Tool: a WYSIWYG editor from DEC for building human interfaces to applications using OSF/Motif. It provides an interactive interface to UIL and the Motif toolkit. 
;; VxWorks
;;     A real-time software development environment and multitasking operating system from Wind River Systems that uses the VRTX kernel. 
;; W3
;;     See WWW. 
;; W3C
;;     The World Wide Web Consortium - More information. 
;; WAIS
;;     Wide Area Information Servers: a distributed document retrieval system supported by Apple, Thinking Machines and Dow Jones. Servers answer questions from personal workstations following a standard protocol - More information. 
;; WABI
;;     A software package to emulate Windows under X 
;; WAN
;;     Wide Area Network. 
;; Warehouse
;;     See Data Warehouse. 
;; WARIA
;;     Workflow And Reengineering International Association - More information. 
;; Wasserman
;;     A.I.(Tony) Wasserman: president of IDE. 
;; Waterfall
;;     A software life-cycle model showing the phases of the cycle and their interrelations on a characteristic diagram. 
;; WE
;;     A hypertext authoring system developed at the University of North Carolina. 
;; Web
;;     See WWW. 
;; WEB
;;     See Literate Programming and also WorldWide Web 
;; Westmount
;;     A Netherlands software engineering vendor of RTEE and other products. 
;; WFMS
;;     WorkFlow Management System. Software to manage workflow in an organisation. 
;; Whetstone
;;     A benchmark program. 
;; Widget
;;     In the X Window System, a window with its associated input and output functions. Widgets, provided by a library package, are used as building blocks to construct a wide variety of application environments - More information. 
;; Willow
;;     A Motif-based user interface program for bibliographic information retrieval systems, from Washington University - More information. 
;; WIMP
;;     Windows, Icons, Menus and Pointers (or maybe Windows, Icons, Mouse, Pull-down menus). The style of user interface made popular by the Apple Macintosh and now available in other GUIs, such as OSF/Motif and NeWS. 
;; Window manager
;;     In a window system, a program which manages windows on a screen. It is responsible for moving and resizing windows, and other practical functions. 
;; Window system
;;     Software which supports windowing. Examples are the X Window System, and proprietary systems on the Macintosh, NeXT and Sun. 
;; Windowing
;;     The ability to interact at will with several processes in a computer through reserved areas, or windows, on a VDU screen. 
;; Windows
;;     A window system and user interface software from Microsoft for MS-DOS. 
;; Windows 4GL
;;     INGRES/Windows 4GL is a graphical tool running on top of workstation native windowing systems, to help developers to build user interfaces to INGRES applications. 
;; WISE
;;     World Wide Information System for Support of R&D Efforts. A project funded by the Commission of the European Communities to encourage "Transborder Telework and Research Co-operation" - More information. 
;; WISE
;;     Web-Integrated Software metrics Environment. A WWW based software management and metrics system from NASA - More information. 
;; WIT
;;     WWW Interactive Talk - More information. 
;; WizDOM
;;     Software for distributed UNIX system management from TIVOLI Systems of Austin, Texas 
;; Word
;;     A document processing program from Microsoft. 
;; Workflow
;;     The way in which work units (information or actions) are routed through an organisation. It can be formalised in terms of rules incorporating dependencies, staff roles etc. and hence automated - More information. 
;; Workstation
;;     A general-purpose computer designed to be used by one person at a time and which offers higher performance than normally found in a PC, especially with respect to graphics, processing power and the ability to carry out several tasks at the same time. 
;; WOSC
;;     World Organisation of Systemics and Cybernetics. 
;; WSRD
;;     Worldwide Software Resources Discovery. An ASSET service. 
;; WSL
;;     Wide Spectrum Language developed for program transformation - More information. 
;; WWW
;;     World-Wide Web: a project originated at CERN, aimed at providing hypertext-style access to information from a wide range of sources - More information. 
;; WYSIWYG
;;     What You See Is What You Get: a feature of document preparation systems allowing the user to work on a document displayed on a screen in exactly the same form as it will appear when printed. 
;; X client
;;     An application process in the X Window System: it gains access to windowing services via the Xlib library. These are translated by the system into messages to an X server. 
;; X Consortium
;;     A vendor consortium supporting development of the X Window System - More information. 
;; X-designer
;;     A user interface builder for Motif from Imperial Software Technology. 
;; X protocol
;;     A standard used by clients (applications) and servers in the X Window System for exchanging requests for window manipulations. 
;; X server
;;     A process which controls a bitmap display device.in an X Window System. It performs operations on request from client applications. 
;; X terminal
;;     An intelligent terminal which operates as an X server directly connected to Ethernet. 
;; X-terminal
;;     An intelligent terminal with a built-in implementation of an X server , which can therefore communicate with computers running X clients . 
;; X Windows
;;     See X Window System. 
;; X Window System
;;     A specification for device-independent windowing operations on bitmap display devices, developed by MIT and now a de facto standard supported by the X consortium - More information. 
;; X.25
;;     A standard networking protocol suite approved by the CCITT and ISO. This protocol suite defines standard physical, link, and networking layers (layers 1 through 3). X.25 networks are in use throughout the world. 
;; X.400
;;     The set of CCITT communications standards covering mail services provided by data networks. 
;; X.500
;;     The set of CCITT standards covering electronic mail directory services. 
;; X.desktop
;;     A desktop manager for UNIX from IXI. 
;; X/Open
;;     An international consortium of vendors whose purpose is to define the X/Open Common Applications Environment designed to provide applications portability - More information. 
;; X11R4
;;     Version 11 release 4 of the X protocol; the current standard. 
;; X11R5
;;     Version 11 release 5 of the X protocol; the new standard. 
;; X3J16
;;     The C++ standard technical committee. 
;; X
;;     An abbreviation for the X Window System. 
;; Xanadu
;;     An electronic publishing project due to Ted Nelson, the inventor of the term hypertext - More information. 
;; Xaw
;;     The Athena Widget Set: a set of widgets distributed with the X Window System. 
;; XDR
;;     eXternal Data Representation - universal machine independent form of data sent by RPC systems. Described in RFC 1014. 
;; XENIX
;;     UNIX implementations from SCO. 
;; Xerox
;;     The Document Company - More information. 
;; Xerox PARC
;;     The Palo Alto Research Center of the Xerox Corporation - More information. 
;; XIE
;;     X Image Extension: extensions to the X protocol to handle images. 
;; Xlib
;;     X library: program interface to the X Window System. 
;; XML
;;     Xperimental Markup Language based on CML. 
;; xmosaic
;;     See Mosaic 
;; XMP
;;     The X/Open Management Protocols. 
;; XNS
;;     Xerox Network Services: a proprietary networking architecture developed by Xerox. 
;; XOM
;;     The X/Open OSI Abstract Data Manipulation API. 
;; Xopen
;;     See X/Open 
;; XPG3
;;     Version 3 of XPG. 
;; XPG
;;     X/open Portability Guide: defines the interfaces of the X/Open Common Applications Environment. 
;; XRemote
;;     A serial line protocol for the X Window System . 
;; XRN
;;     A newsreader program for Usenet news base on the X Window System. host. 
;; XSI
;;     X/Open System Interface specification: part of the X/Open Common Applications Environment. 
;; Xt
;;     The intrinsics of theX Window System Toolkit. 
;; Xterminal
;;     See X-terminal 
;; XTI
;;     X/open Transport Interface. 
;; XUI
;;     X User Interface: program interface to the X Window System supported by DEC. 
;; Xv++
;;     A library of classes from Interface Engineering, Stevenage, providing a C++ Application Programmer's Interface to the XView toolkit. 
;; XView
;;     A toolkit from Sun, derived from SunView, providing an Open Look user interface for X applications. 
;; XVT
;;     eXtensible Virtual Toolkit: a product allowing applications to be developed independent of GUI. 
;; Xwindow
;;     See X Window System 
;; Y++
;;     An Object-Oriented analysis and design approach - More information. 
;; yacc
;;     Yet Another Compiler Compiler. A parser generator for UNIX by S.C.Johnson 
;; YACL
;;     Yet Another Class Library - More information. 
;; YP
;;     Yellow Pages: a name server in NFS to link clients desiring a service with servers who can provide it. 
;; YSM
;;     Yourdon Structured Method 
;; Z
;;     A formal specification language developed at Oxford University for describing computing systems, based on set theory and predicate calculus - More information. 
;; Z39.50
;;     Information Retrieval Service Definition and Protocol Specification for Library Applications. Developed by NISO, this standard specifies an OSI application layer service to allow an application on one computer to query a database on another; it is used by WAIS - More information. 
;; ZEBRA
;;     A data management package in the CERN Program Library - More information. 
;; ZOG
;;     A high-performance hypertext system developed at Carnegie-Mellon University. 
;;; ==============================

;;; ================================================================
;;; STING-software-engineering-glossary.el ends here
;;; EOF
