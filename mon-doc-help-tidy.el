;;; mon-doc-help-tidy.el --- extends mon-doc-help-utils package with HTML tidy docs
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-doc-help-tidy.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-06-20T18:00:54-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: docs, extensions, external, hypermedia, processes

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-doc-help-tidy provides extends mon-doc-help-utils package with tidy docs
;;
;; FUNCTIONS:►►►
;; `mon-doc-help-tidy'
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;; Tidy is released under the MIT Licence.
;; :SEE (URL `http://tidy.sourceforge.net/') 
;;
;; URL: http://www.emacswiki.org/emacs/mon-doc-help-tidy.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-06-20T18:30:07-04:00Z}#{10247} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-doc-help-tidy. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-06-20T18:00:54-04:00Z}#{10247} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================

;;; ==============================
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; :NOTE converted from html man page routed through emacs-w3m.
;;; As generated with HTML for Tidy release 2008-06-28.
;;; file:///usr/doc/tidy-20090604T2303/tidy_man.html
;;; :COURTESY Dave Raggett <dsr@w3.org>, and is now maintained
;;; and the Tidy team :SEE (URL `http://tidy.sourceforge.net/') 
;;; Tidy is released under the MIT Licence.
;;; :CREATED <Timestamp: #{2010-06-20T17:56:24-04:00Z}#{10247} - by MON KEY>
(defun mon-doc-help-tidy (&optional insertp intrp)
  "Usage and flags for the `tidy' command.\n
`tidy` Validates, corrects, and pretty-print HTML files.\n
       
;; :TIDY-SYNTAX

 tidy [option ...] [file ...] [option ...] [file ...]

;; :TIDY-DESCRIPTION

Tidy reads HTML, XHTML and XML files and writes cleaned up markup. For
HTML variants, it detects and corrects many common coding errors and
strives to produce visually equivalent markup that is both W3C
compliant and works on most browsers. A common use of Tidy is to
convert plain HTML to XHTML. For generic XML files, Tidy is limited to
correcting basic well-formedness errors and pretty printing.

If no input file is specified, Tidy reads the standard input. If no
output file is specified, Tidy writes the tidied markup to the
standard output. If no error file is specified, Tidy writes messages
to the standard error. For command line options that expect a
numerical argument, a default is assumed if no meaningful value can be
found.

;; :TIDY-OPTIONS

;; :TIDY-FILE-MANIPULATION

-output <file>, -o <file>

  Write output to the specified <file> 
  \(output-file: <file>\)

-config <file>

  Set configuration options from the specified <file>

-file <file>, -f <file>

  Write errors and warnings to the specified <file> 
  \(error-file: <file>\)

-modify, -m  

  Modify the original input files 
  \(write-back: yes\)
         

;; :TIDY-PROCESSING-DIRECTIVES

-indent, -i

  Indent element content 
  \(indent: auto\)

-wrap <column>, -w <column>

  Wrap text at the specified <column>. 0 is assumed if <column>
  is missing. When this option is omitted, the default of the
  configuration option \"wrap\" applies. 
  \(wrap: <column>\)

-upper, -u

  Force tags to upper case 
  \(uppercase-tags: yes\)

-clean, -c
 
  Replace FONT, NOBR and CENTER tags by CSS 
  \(clean: yes\)

-bare, -b

  Strip out smart quotes and em dashes, etc. 
  \(bare: yes\)

-numeric, -n

  Output numeric rather than named entities 
  \(numeric-entities: yes\)

-errors, -e

  Show only errors and warnings 
  \(markup: no\)

-quiet, -q

  Suppress nonessential output
  \(quiet: yes\)

 -omit           

  Omit optional end tags 
  \(hide-endtags: yes\)

 -xml

  Specify the input is well formed XML 
  \(input-xml: yes\)

-asxml, -asxhtml

  Convert HTML to well formed XHTML 
  \(output-xhtml: yes\)

-ashtml

  Force XHTML to well formed HTML \(output-html: yes\)

-access <level>

  Do additional accessibility checks 
  \(<level> = 0, 1, 2, 3\). 0
  0 is assumed if <level> is missing.
  \(accessibility-check: <level>\)
  

;; :TIDY-CHARACTER-ENCODINGS

-raw            

  Output values above 127 without conversion to entities

-ascii          

  Use ISO-8859-1 for input, US-ASCII for output

-latin0

  Use ISO-8859-15 for input, US-ASCII for output

-latin1

  Use ISO-8859-1 for both input and output

-iso2022

  Use ISO-2022 for both input and output

-utf8          

  Use UTF-8 for both input and output

-mac

  Use MacRoman for input, US-ASCII for output

-win1252

  Use Windows-1252 for input, US-ASCII for output

-ibm858

  Use IBM-858 \(CP850+Euro\) for input, US-ASCII for output

-utf16le

  Use UTF-16LE for both input and output

-utf16be

  Use UTF-16BE for both input and output

-utf16

  Use UTF-16 for both input and output

-big5

  Use Big5 for both input and output

-shiftjis

  Use Shift_JIS for both input and output

-language <lang>

  Set the two-letter language code <lang> \(for future use\) 
  \(language: <lang>\)
 

;; :TIDY-MISCELLANEOUS

-version, -v   Show the version of Tidy
-help, -h, -?  List the command line options
-xml-help      List the command line options in XML format
-help-config   List all configuration options
-xml-config    List all configuration options in XML format
-show-config   List the current configuration settings

;; :TIDY-USAGE

Use --optionX valueX for the detailed configuration option \"optionX\"
with argument \"valueX\". See also below under Detailed Configuration
Options as to how to conveniently group all such options in a single
config file.

Input/Output default to stdin/stdout respectively. Single letter
options apart from -f and -o may be combined as in:

   tidy -f errs.txt -imu foo.html

;; :TIDY-ENVIRONMENT 

`HTML_TIDY`

Name of the default configuration file. This should be an absolute path, since
you will probably invoke tidy from different directories. The value of HTML_TIDY
will be parsed after the compiled-in default \(defined with -DTIDY_CONFIG_FILE\),
but before any of the files specified using -config.

;; :TIDY-EXIT-STATUS

0            All input files were processed successfully.
1            There were warnings.
2            There were errors.

;; :TIDY-DETAILED-CONFIGURATION-OPTIONS

This section describes the Detailed \(i.e., \"expanded\"\) Options, which
may be specified by preceding each option with -- at the command line,
followed by its desired value, OR by placing the options and values in
a configuration file, and telling tidy to read that file with the
-config standard option.

;; :TIDY-CLI-SYNTAX

 tidy --option1 value1 --option2 value2 [standard options ...]
 tidy -config config-file [standard options ...]

;; :TIDY-WARNING

The options detailed here do not include the \"standard\" command-line
options \(i.e., those preceded by a single ’-’\) described above in the
first section of this man page.

;; :TIDY-DESCRIPTION

A list of options for configuring the behavior of Tidy, which can be
passed either on the command line, or specified in a configuration
file.

A Tidy configuration file is simply a text file, where each option is
listed on a separate line in the form:

 <OPTION>: <VALUE>
 option1:  value1
 option2:  value2
 etc.

The permissible values for a given option depend on the option’s Type.
There are five types: Boolean, AutoBool, DocType, Enum, and String.
Boolean types allow any of yes/no, y/n, true/false, t/f, 1/0.
AutoBools allow auto in addition to the values allowed by Booleans.
Integer types take non-negative integers. String types generally have
no defaults, and you should provide them in non-quoted form \(unless
you wish the output to contain the literal quotes\).

Enum, Encoding, and DocType \"types\" have a fixed repertoire of items;
consult the Example[s] provided below for the option[s] in question.

You only need to provide options and values for those whose defaults
you wish to override, although you may wish to include some
already-defaulted options and values for the sake of documentation and
explicitness.

Here is a sample config file, with at least one example of each of the
five Types:

// sample Tidy configuration options output-xhtml: yes add-xml-decl:
no doctype: strict char-encoding: ascii indent: auto wrap: 76
repeated-attributes: keep-last error-file: errs.txt

;; :TIDY-OPTIONS

Below is a summary and brief description of each of the options. They
are listed alphabetically within each category. There are five
categories: HTML, XHTML, XML options, Diagnostics options, Pretty
Print options, Character Encoding options, and Miscellaneous options.

:TIDY-OPTIONS-HTML-XHTML-XML

---
`add-xml-decl`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should add the XML declaration
when outputting XML or XHTML. Note that if the input already
includes an <?xml ... ?> declaration then this option will be
ignored. If the encoding for the output is different from
\"ascii\", one of the utf encodings or \"raw\", the declaration
is always added as required by the XML standard.

:SEE `char-encoding`, `output-encoding`

---
`add-xml-space`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should add xml:space=\"preserve\"
to elements such as <PRE>, <STYLE> and <SCRIPT> when
generating XML. This is needed if the whitespace in such
elements is to be parsed appropriately without having access
to the DTD.

---
`alt-text`

:TYPE <STRING>
:DEFAULT -
:DEFAULT -

This option specifies the default \"alt=\" text Tidy uses for
<IMG> attributes. This feature is dangerous as it suppresses
further accessibility warnings. You are responsible for
making your documents accessible to people who can not see
the images!

---
`anchor-as-name`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option controls the deletion or addition of the name
attribute in elements where it can serve as anchor. If set to
\"yes\", a name attribute, if not already existing, is added
along an existing id attribute if the DTD allows it. If set
to \"no\", any existing name attribute is removed if an id
attribute exists or has been added.

---
`assume-xml-procins`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should change the parsing of
processing instructions to require ?> as the terminator
rather than >. This option is automatically set if the input
is in XML.

---
`bare`
:TYPE <Boolean>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should strip Microsoft specific
HTML from Word 2000 documents, and output spaces rather than
non-breaking spaces where they exist in the input.

          clean             Type: Boolean

:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should strip out surplus
presentational tags and attributes replacing them by style
rules and structural markup as appropriate. It works well on
the HTML saved by Microsoft Office products.

:SEE `drop-font-tags`

---
`css-prefix`

:TYPE <STRING>
:DEFAULT -
:DEFAULT -

This option specifies the prefix that Tidy uses for styles
rules. By default, \"c\" will be used.

---
`decorate-inferred-ul`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should decorate inferred UL
elements with some CSS markup to avoid indentation to the
right.

---
`doctype`

:TYPE <DOCTYPE>
:DEFAULT auto
:EXAMPLE omit, auto, strict, transitional, user

This option specifies the DOCTYPE declaration generated by
Tidy. If set to \"omit\" the output won't contain a DOCTYPE
declaration. If set to \"auto\" \(the default\) Tidy will use an
educated guess based upon the contents of the document. If
set to \"strict\", Tidy will set the DOCTYPE to the strict DTD.
If set to \"loose\", the DOCTYPE is set to the loose
\(transitional\) DTD. Alternatively, you can supply a string
for the formal public identifier \(FPI\).

For example:
 doctype: \"-//ACME//DTD HTML 3.14159//EN\"

If you specify the FPI for an XHTML document, Tidy will set
the system identifier to an empty string. For an HTML
document, Tidy adds a system identifier only if one was
already present in order to preserve the processing mode of
some browsers. Tidy leaves the DOCTYPE for generic XML
documents unchanged. --doctype omit implies
--numeric-entities yes. This option does not offer a
validation of the document conformance.

---
`drop-empty-paras`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should discard empty
paragraphs.

---
`drop-font-tags`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should discard <FONT> and
<CENTER> tags without creating the corresponding style rules.
This option can be set independently of the clean option.

:SEE `clean`

---
`drop-proprietary-attributes`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should strip out proprietary
attributes, such as MS data binding attributes.

---
`enclose-block-text`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should insert a <P> element to
enclose any text it finds in any element that allows mixed
content for HTML transitional but not HTML strict.

---
`enclose-text`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should enclose any text it
finds in the body element within a <P> element. This is
useful when you want to take existing HTML and use it with a
style sheet.

---
`escape-cdata`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should convert <![CDATA[]]>
sections to normal text.

---
`fix-backslash`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should replace backslash
characters \"\\\" in URLs by forward slashes \"/\".

---
`fix-bad-comments`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should replace unexpected
hyphens with \"=\" characters when it comes across adjacent
hyphens. The default is yes. This option is provided for
users of Cold Fusion which uses the comment syntax: 
 <!---  --->

---
`fix-uri`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should check attribute values
that carry URIs for illegal characters and if such are found,
escape them as HTML 4 recommends.

---
`hide-comments`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should print out comments.

---
`hide-endtags`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should omit optional end-tags
when generating the pretty printed markup. This option is
ignored if you are outputting to XML.

---
`indent-cdata`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should indent <![CDATA[]]>
sections.

---
`input-xml`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should use the XML parser
rather than the error correcting HTML parser.

---
`join-classes`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should combine class names to
generate a single new class name, if multiple class
assignments are detected on an element.

:SEE `join-styles`, `repeated-attributes`

---
`join-styles`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should combine styles to
generate a single new style, if multiple style values are
detected on an element.

:SEE `join-classes`, `repeated-attributes`

---
`literal-attributes`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should ensure that whitespace
characters within attribute values are passed through
unchanged.

---
`logical-emphasis`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should replace any occurrence
of <I> by <EM> and any occurrence of <B> by <STRONG>. In both
cases, the attributes are preserved unchanged. This option
can be set independently of the clean and drop-font-tags
options.

---
`lower-literals`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should convert the value of an
attribute that takes a list of predefined values to lower
case. This is required for XHTML documents.

---
`merge-divs`

:TYPE <AUTOBOOL>
:DEFAULT auto
:EXAMPLE auto, y/n, yes/no, t/f, true/false, 1/0

Can be used to modify behavior of -c \(--clean yes\) option.
This option specifies if Tidy should merge nested <div> such
as \"<div><div>...</div></div>\". If set to \"auto\", the
attributes of the inner <div> are moved to the outer one. As
well, nested <div> with ID attributes are not merged. If set
to \"yes\", the attributes of the inner <div> are discarded
with the exception of \"class\" and \"style\".

:SEE `clean`, `merge-spans`

---
`merge-spans`

:TYPE <AUTOBOOL>
:DEFAULT auto
:EXAMPLE auto, y/n, yes/no, t/f, true/false, 1/0

Can be used to modify behavior of -c \(--clean yes\) option.
This option specifies if Tidy should merge nested <span> such
as \"<span><span>...</span></span>\". The algorithm is
identical to the one used by --merge-divs.

:SEE `clean`, `merge-divs`

---
`ncr`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should allow numeric character
references.

---
`new-blocklevel-tags`

:TYPE <TAG NAMES>
:DEFAULT -
:EXAMPLE tagX, tagY, ...

This option specifies new block-level tags. This option takes
a space or comma separated list of tag names. Unless you
declare new tags, Tidy will refuse to generate a tidied file
if the input includes previously unknown tags. Note you can't
change the content model for elements such as <TABLE>, <UL>,
<OL> and <DL>. This option is ignored in XML mode.

:SEE `new-empty-tags`, `new-inline-tags`, `new-pre-tags`

---
`new-empty-tags`

:TYPE <TAG NAMES>
:DEFAULT -
:EXAMPLE tagX, tagY, ...

This option specifies new empty inline tags. This option
takes a space or comma separated list of tag names. Unless
you declare new tags, Tidy will refuse to generate a tidied
file if the input includes previously unknown tags. Remember
to also declare empty tags as either inline or blocklevel.
This option is ignored in XML mode.

:SEE `new-blocklevel-tags`, `new-inline-tags`, `new-pre-tags`

---
`new-inline-tags`

:TYPE <TAG NAMES>
:DEFAULT -
:EXAMPLE tagX, tagY, ...

This option specifies new non-empty inline tags. This option
takes a space or comma separated list of tag names. Unless
you declare new tags, Tidy will refuse to generate a tidied
file if the input includes previously unknown tags. This
option is ignored in XML mode.

:SEE `new-blocklevel-tags`, `new-empty-tags`, `new-pre-tags`

---
`new-pre-tags`

:TYPE <TAG NAMES>
:DEFAULT -
:EXAMPLE tagX, tagY, ...

This option specifies new tags that are to be processed in
exactly the same way as HTML's <PRE> element. This option
takes a space or comma separated list of tag names. Unless
you declare new tags, Tidy will refuse to generate a tidied
file if the input includes previously unknown tags. Note you
can not as yet add new CDATA elements \(similar to <SCRIPT>\).
This option is ignored in XML mode.

:SEE `new-blocklevel-tags`, `new-empty-tags`, `new-inline-tags`

---
`numeric-entities`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output entities other
than the built-in HTML entities \(&amp;, &lt;, &gt; and &
quot;\) in the numeric rather than the named entity form. Only
entities compatible with the DOCTYPE declaration generated
are used. Entities that can be represented in the output
encoding are translated correspondingly.

:SEE `doctype`, `preserve-entities`

---
`output-html`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should generate pretty printed
output, writing it as HTML.

---
`output-xhtml`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should generate pretty printed
output, writing it as extensible HTML. This option causes
Tidy to set the DOCTYPE and default namespace as appropriate
to XHTML. If a DOCTYPE or namespace is given they will
checked for consistency with the content of the document. In
the case of an inconsistency, the corrected values will
appear in the output. For XHTML, entities can be written as
named or numeric entities according to the setting of the
\"numeric-entities\" option. The original case of tags and
attributes will be preserved, regardless of other options.

---
`output-xml`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should pretty print output,
writing it as well-formed XML. Any entities not defined in
XML 1.0 will be written as numeric entities to allow them to
be parsed by a XML parser. The original case of tags and
attributes will be preserved, regardless of other options.

---
`preserve-entities`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should preserve the well-formed
entitites as found in the input.

---
`quote-ampersand`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output unadorned &
characters as &amp;.

---
`quote-marks`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output \" characters as &
quot; as is preferred by some editing environments. The
apostrophe character ' is written out as &#39; since many web
browsers don't yet support &apos;.

---
`quote-nbsp`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output non-breaking
space characters as entities, rather than as the Unicode
character value 160 \(decimal\).

---
`repeated-attributes`

:TYPE <ENUM>
:DEFAULT keep-last
:EXAMPLE keep-first, keep-last

This option specifies if Tidy should keep the first or last
attribute, if an attribute is repeated, e.g. has two align
attributes.

:SEE `join-classes`, `join-styles`

---
`replace-color`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should replace numeric values
in color attributes by HTML/XHTML color names where defined,
e.g. replace \"#ffffff\" with \"white\".

---
`show-body-only`

:TYPE <AUTOBOOL>
:DEFAULT no
:EXAMPLE auto, y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should print only the contents
of the body tag as an HTML fragment. If set to \"auto\", this
is performed only if the body tag has been inferred. Useful
for incorporating existing whole pages as a portion of
another page. This option has no effect if XML output is
requested.

---
`uppercase-attributes`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output attribute names
in upper case. The default is no, which results in lower case
attribute names, except for XML input, where the original
case is preserved.

---
`uppercase-tags`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output tag names in
upper case. The default is no, which results in lower case
tag names, except for XML input, where the original case is
preserved.

---
`word-2000`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should go to great pains to
strip out all the surplus stuff Microsoft Word 2000 inserts
when you save Word documents as \"Web pages\". Doesn't handle
embedded images or VML. You should consider using Word's
\"Save As: Web Page, Filtered\".

---
;; :TIDY-DIAGNOSTICS-OPTIONS

`accessibility-check`

:TYPE <ENUM>
:DEFAULT 0 \(Tidy Classic\)
:EXAMPLE 0 \(Tidy Classic\), 1 \(Priority 1 Checks\), 2 \(Priority
2 Checks\), 3 \(Priority 3 Checks\)

This option specifies what level of accessibility checking,
if any, that Tidy should do. Level 0 is equivalent to Tidy
Classic's accessibility checking. For more information on
Tidy's accessibility checking, visit the Adaptive Technology
Resource Centre at the University of Toronto at http://
www.aprompt.ca/Tidy/accessibilitychecks.html.

---
`show-errors`

:TYPE <INTEGER>
:DEFAULT 6
:EXAMPLE 0, 1, 2, ...

This option specifies the number Tidy uses to determine if
further errors should be shown. If set to 0, then no errors
are shown.

---
`show-warnings`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should suppress warnings. This
can be useful when a few errors are hidden in a flurry of
warnings.

---
;; :TIDY-PRETTY-PRINT-OPTIONS

`break-before-br`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output a line break
before each <BR> element.

---
`indent`

:TYPE <AUTOBOOL>
:DEFAULT no
:EXAMPLE auto, y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should indent block-level tags.
If set to \"auto\", this option causes Tidy to decide whether
or not to indent the content of tags such as TITLE, H1-H6,
LI, TD, TD, or P depending on whether or not the content
includes a block-level element. You are advised to avoid
setting indent to yes as this can expose layout bugs in some
browsers.

:SEE `indent-spaces`

---
`indent-attributes`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should begin each attribute on
a new line.

---
`indent-spaces`

:TYPE <INTEGER>
:DEFAULT 2
:EXAMPLE 0, 1, 2, ...

This option specifies the number of spaces Tidy uses to
indent content, when indentation is enabled.

:SEE `indent`

---
`markup`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should generate a pretty
printed version of the markup. Note that Tidy won't generate
a pretty printed version if it finds significant errors \(see
force-output\).

---
`punctuation-wrap`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap after some
Unicode or Chinese punctuation characters.

---
`sort-attributes`

:TYPE <ENUM>
:DEFAULT none
:EXAMPLE none, alpha

This option specifies that tidy should sort attributes within
an element using the specified sort algorithm. If set to
\"alpha\", the algorithm is an ascending alphabetic sort.

---
`split`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

Currently not used. Tidy Classic only.

---
`tab-size`

:TYPE <INTEGER>
:DEFAULT 8
:EXAMPLE 0, 1, 2, ...

This option specifies the number of columns that Tidy uses
between successive tab stops. It is used to map tabs to
spaces when reading the input. Tidy never outputs tabs.

---
`vertical-space`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should add some empty lines for
readability.

---
`wrap`             
:TYPE <INTEGER>
:DEFAULT 68
:EXAMPLE 0 \(no wrapping\), 1, 2, ...

This option specifies the right margin Tidy uses for line
wrapping. Tidy tries to wrap lines so that they do not exceed
this length. Set wrap to zero if you want to disable line
wrapping.

---
`wrap-asp`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap text contained
within ASP pseudo elements, which look like: <% ... %>.

---
`wrap-attributes`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap attribute
values, for easier editing. This option can be set
independently of wrap-script-literals.

:SEE `wrap-script-literals`

---
`wrap-jste`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap text contained
within JSTE pseudo elements, which look like: <# ... #>.

---
`wrap-php`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap text contained
within PHP pseudo elements, which look like: <?php ... ?>.

---
`wrap-script-literals`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap string
literals that appear in script attributes. Tidy wraps long
script string literals by inserting a backslash character
before the line break.

:SEE `wrap-attributes`

---
`wrap-sections`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap text contained
within <![ ... ]> section tags.

---
:TIDY-CHARACTER-ENCODING-OPTIONS

`ascii-chars`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

Can be used to modify behavior of -c \(--clean yes\) option. If
set to \"yes\" when using -c, &emdash;, &rdquo;, and other
named character entities are downgraded to their closest
ascii equivalents.

:SEE `clean`

---
`char-encoding`

:TYPE <ENCODING>
:DEFAULT ascii
:EXAMPLE raw, ascii, latin0, latin1, utf8, iso2022, mac,
win1252, ibm858, utf16le, utf16be, utf16, big5, shiftjis

This option specifies the character encoding Tidy uses for
both the input and output. For ascii, Tidy will accept
Latin-1 \(ISO-8859-1\) character values, but will use entities
for all characters whose value > 127. For raw, Tidy will
output values above 127 without translating them into
entities. For latin1, characters above 255 will be written as
entities. For utf8, Tidy assumes that both input and output
is encoded as UTF-8. You can use iso2022 for files encoded
using the ISO-2022 family of encodings e.g. ISO-2022-JP. For
mac and win1252, Tidy will accept vendor specific character
values, but will use entities for all characters whose value
> 127. For unsupported encodings, use an external utility to
convert to and from UTF-8.

:SEE `input-encoding`, `output-encoding`

---
`input-encoding`

:TYPE <ENCODING>
:DEFAULT latin1
:EXAMPLE raw, ascii, latin0, latin1, utf8, iso2022, mac,
win1252, ibm858, utf16le, utf16be, utf16, big5, shiftjis

This option specifies the character encoding Tidy uses for
the input. See char-encoding for more info.

:SEE `char-encoding`

---
`language`

:TYPE <STRING>
:DEFAULT -
:DEFAULT -

Currently not used, but this option specifies the language
Tidy uses \(for instance \"en\"\).

---
`newline`

:TYPE <ENUM>
:DEFAULT Platform dependent
:EXAMPLE LF, CRLF, CR

The default is appropriate to the current platform: CRLF on
PC-DOS, MS-Windows and OS/2, CR on Classic Mac OS, and LF
everywhere else \(Unix and Linux\).

---
`output-bom`

:TYPE <AUTOBOOL>
:DEFAULT auto
:EXAMPLE auto, y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should write a Unicode Byte
Order Mark character \(BOM; also known as Zero Width No-Break
Space; has value of U+FEFF\) to the beginning of the output;
only for UTF-8 and UTF-16 output encodings. If set to \"auto\",
this option causes Tidy to write a BOM to the output only if
a BOM was present at the beginning of the input. A BOM is
always written for XML/XHTML output using UTF-16 output
encodings.

---
`output-encoding`

:TYPE <ENCODING>
:DEFAULT ascii
:EXAMPLE raw, ascii, latin0, latin1, utf8, iso2022, mac, 
         win1252, ibm858, utf16le, utf16be, utf16, big5,
         shiftjis

This option specifies the character encoding Tidy uses for
the output. See char-encoding for more info. May only be
different from input-encoding for Latin encodings \(ascii,
latin0, latin1, mac, win1252, ibm858\).

:SEE `char-encoding`

---
;; :TIDY-MISCELLANEOUS-OPTIONS

`error-file`

:TYPE <STRING>
:DEFAULT -
:DEFAULT -

This option specifies the error file Tidy uses for errors and
warnings. Normally errors and warnings are output to
\"stderr\".

:SEE `output-file`

---
`force-output`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should produce output even if
errors are encountered. Use this option with care - if Tidy
reports an error, this means Tidy was not able to, or is not
sure how to, fix the error, so the resulting output may not
reflect your intention.

---
`gnu-emacs`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should change the format for
reporting errors and warnings to a format that is more easily
parsed by GNU Emacs.

---
`gnu-emacs-file`

:TYPE <STRING>
:DEFAULT -
:DEFAULT -

Used internally.

---
`keep-time`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should keep the original
modification time of files that Tidy modifies in place. The
default is no. Setting the option to yes allows you to tidy
files without causing these files to be uploaded to a web
server when using a tool such as SiteCopy. Note this feature
is not supported on some platforms.

---
`output-file`

:TYPE <STRING>
:DEFAULT -
:DEFAULT -

This option specifies the output file Tidy uses for markup.
Normally markup is written to \"stdout\".

:SEE `error-file`

---
`quiet`             
:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output the summary of
the numbers of errors and warnings, or the welcome or
informational messages.

---
`slide-style`

:TYPE <STRING>
:DEFAULT -
:DEFAULT -

Currently not used. Tidy Classic only.

---
`tidy-mark`

:TYPE <BOOLEAN>
:DEFAULT yes
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should add a meta element to
the document head to indicate that the document has been
tidied. Tidy won't add a meta element if one is already
present.

---
`write-back`

:TYPE <BOOLEAN>
:DEFAULT no
:EXAMPLE y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should write back the tidied
markup to the same file it read from. You are advised to keep
copies of important files before tidying them, as on rare
occasions the result may not be what you expect.\n

;; :TIDY-OPTIONS-QUICKREF
 _________________________________________________________
|                              |            |             |
|       :TIDY-OPTION           |  :TYPE     |  :DEFAULT   |
|______________________________|____________|_____________|
|                              |            |             |
| `add-xml-decl`               | <BOOLEAN>  | no          |
| `add-xml-space`              | <BOOLEAN>  | no          |
| `alt-text`                   | <STRING>   | -           |
| `anchor-as-name`             | <BOOLEAN>  | yes         |
| `assume-xml-procins`         | <BOOLEAN>  | no          |
| `bare`                       | <BOOLEAN>  | no          |
| `clean`                      | <BOOLEAN>  | no          |
| `css-prefix`                 | <STRING>   | -           |
| `decorate-inferred-ul`       | <BOOLEAN>  | no          |
| `doctype`                    | <DOCTYPE>  | auto        |
| `drop-empty-paras`           | <BOOLEAN>  | yes         |
| `drop-font-tags`             | <BOOLEAN>  | no          |
| `drop-proprietary-attributes`| <BOOLEAN>  | no          |
| `enclose-block-text`         | <BOOLEAN>  | no          |
| `enclose-text`               | <BOOLEAN>  | no          |
| `escape-cdata`               | <BOOLEAN>  | no          |
| `fix-backslash`              | <BOOLEAN>  | yes         |
| `fix-bad-comments`           | <BOOLEAN>  | yes         |
| `fix-uri`                    | <BOOLEAN>  | yes         |
| `hide-comments`              | <BOOLEAN>  | no          |
| `hide-endtags`               | <BOOLEAN>  | no          |
| `indent-cdata`               | <BOOLEAN>  | no          |
| `input-xml`                  | <BOOLEAN>  | no          |
| `join-classes`               | <BOOLEAN>  | no          |
| `join-styles`                | <BOOLEAN>  | yes         |
| `literal-attributes`         | <BOOLEAN>  | no          |
| `logical-emphasis`           | <BOOLEAN>  | no          |
| `lower-literals`             | <BOOLEAN>  | yes         |
| `merge-divs`                 | <AUTOBOOL> | auto        |
| `merge-spans`                | <AUTOBOOL> | auto        |
| `ncr`                        | <BOOLEAN>  | yes         |
| `new-blocklevel-tags`        | <TAG-NAME> | -           |
| `new-empty-tags`             | <TAG-NAME> | -           |
| `new-inline-tags`            | <TAG-NAME> | -           |
| `new-pre-tags`               | <TAG-NAME> | -           |
| `numeric-entities`           | <BOOLEAN>  | no          |
| `output-html`                | <BOOLEAN>  | no          |
| `output-xhtml`               | <BOOLEAN>  | no          |
| `output-xml`                 | <BOOLEAN>  | no          |
| `preserve-entities`          | <BOOLEAN>  | no          |
| `quote-ampersand`            | <BOOLEAN>  | yes         |
| `quote-marks`                | <BOOLEAN>  | no          |
| `quote-nbsp`                 | <BOOLEAN>  | yes         |
| `repeated-attributes`        | <ENUM>     | keep-last   |
| `replace-color`              | <BOOLEAN>  | no          |
| `show-body-only`             | <AUTOBOOL> | no          |
| `uppercase-attributes`       | <BOOLEAN>  | no          |
| `uppercase-tags`             | <BOOLEAN>  | no          |
| `word-`2000                  | <BOOLEAN>  | no          |
| `accessibility-check`        | <ENUM>     | 0 (classic) |
| `show-errors`                | <INTEGER>  | 6           |
| `show-warnings`              | <BOOLEAN>  | yes         |
| `break-before-br`            | <BOOLEAN>  | no          |
| `indent`                     | <AUTOBOOL> | no          |
| `indent-attributes`          | <BOOLEAN>  | no          |
| `indent-spaces`              | <INTEGER>  | 2           |
| `markup`                     | <BOOLEAN>  | yes         |
| `punctuation-wrap`           | <BOOLEAN>  | no          |
| `sort-attributes`            | <ENUM>     | none        |
| `split`                      | <BOOLEAN>  | no          |
| `tab-size`                   | <INTEGER>  | 8           |
| `vertical-space`             | <BOOLEAN>  | no          |
| `wrap`                       | <INTEGER>  | 68          |
| `wrap-asp`                   | <BOOLEAN>  | yes         |
| `wrap-attributes`            | <BOOLEAN>  | no          |
| `wrap-jste`                  | <BOOLEAN>  | yes         |
| `wrap-php`                   | <BOOLEAN>  | yes         |
| `wrap-script-literals`       | <BOOLEAN>  | no          |
| `wrap-sections`              | <BOOLEAN>  | yes         |
| `ascii-chars`                | <BOOLEAN>  | no          |
| `char-encoding`              | <ENCODING> | ascii       |
| `input-encoding`             | <ENCODING> | latin1      |
| `language`                   | <STRING>   | -           |
| `newline`                    | <ENUM>     | Plat-depend |
| `output-bom`                 | <AUTOBOOL> | auto        |
| `output-encoding`            | <ENCODING> | ascii       |
| `error-file`                 | <STRING>   | -           |
| `force-output`               | <BOOLEAN>  | no          |
| `gnu-emacs`                  | <BOOLEAN>  | no          |
| `gnu-emacs-file`             | <STRING>   | -           |
| `keep-time`                  | <BOOLEAN>  | no          |
| `output-file`                | <STRING>   | -           |
| `quiet`                      | <BOOLEAN>  | no          |
| `slide-style`                | <STRING>   | -           |
| `tidy-mark`                  | <BOOLEAN>  | yes         |
| `write-back`                 | <BOOLEAN>  | no          |
|______________________________|____________|___________59^\n

For more about the HTML Tidy Project:
:SEE (URL `http://tidy.sourceforge.net')\n
:SEE (man \"tidy\")\n
:SEE-ALSO .\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-doc-help-tidy :insertp t)
    (message "Pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-doc-help-tidy)
;;; :TEST-ME (mon-doc-help-tidy t)
;;; :TEST-ME (describe-function 'mon-doc-help-tidy)
;;; :TEST-ME (apply 'mon-doc-help-tidy '(t))


;;; ==============================
;; (while 
;;   (search-forward-regexp "^                  Type: \\(.*\\)$")
;;   (replace-match  (concat "                  :TYPE <"
;;                           (upcase (match-string-no-properties 1)) ">")))
;;
;; (while 
;;   (search-forward-regexp "^                  Default: \\(.*\\)$")
;;   (replace-match  (concat "                  :DEFAULT \\1")))
;;
;; (while 
;;   (search-forward-regexp "^                  Example: \\(.*\\)$")
;;   (replace-match  (concat "                  :EXAMPLE \\1")))
;;                          
;; (while
;;   (search-forward-regexp "^         \\([a-z].*\\)$")
;;   (replace-match  (concat "         `\\1`")))
;;
;; (while
;;   (search-forward-regexp "^                  See also: \\([a-z].*\\)$")
;;   (replace-match  "                  :SEE \\1"))
;;; ==============================

;;; ==============================
(provide 'mon-doc-help-tidy)
;;; ==============================

;;; ====================================================================
;;; mon-doc-help-tidy.el ends here
;;; EOF

