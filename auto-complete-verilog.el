;;; auto-complete-verilog.el --- 

;; Copyright 2009 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee
;; Version: $Id: auto-complete-verilog.el,v 0.0 2009/04/25 17:26:34 coldnew Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-verilog)

;;; Code:

(provide 'auto-complete-verilog)
(eval-when-compile
  (require 'cl))
(require 'auto-complete)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defface ac-verilog-candidate-face
  '((t (:background "snow3" :foreground "black")))
  "Face for verilog candidate")

(defface ac-verilog-selection-face
  '((t (:background "SlateBlue3" :foreground "black")))
  "Face for the verilog selected candidate.")


(defvar ac-verilog-sources
  '(verilog-keywords))

(ac-define-dictionary-source
 verilog-keywords '(
;; verilog-type-font-keywords
	     "and" "bit" "buf" "bufif0" "bufif1" "cmos" "defparam"
	     "event" "genvar" "inout" "input" "integer" "localparam"
	     "logic" "mailbox" "nand" "nmos" "not" "notif0" "notif1" "or"
	     "output" "parameter" "pmos" "pull0" "pull1" "pullup"
	     "rcmos" "real" "realtime" "reg" "rnmos" "rpmos" "rtran"
	     "rtranif0" "rtranif1" "semaphore" "signed" "struct" "supply"
	     "supply0" "supply1" "time" "tran" "tranif0" "tranif1"
	     "tri" "tri0" "tri1" "triand" "trior" "trireg" "typedef"
	     "uwire" "vectored" "wand" "wire" "wor" "xnor" "xor"
;; verilog-p1800-keywords
         "alias" "assert" "assume" "automatic" "before" "bind"
	     "bins" "binsof" "break" "byte" "cell" "chandle" "class"
	     "clocking" "config" "const" "constraint" "context" "continue"
	     "cover" "covergroup" "coverpoint" "cross" "deassign" "design"
	     "dist" "do" "edge" "endclass" "endclocking" "endconfig"
	     "endgroup" "endprogram" "endproperty" "endsequence" "enum"
	     "expect" "export" "extends" "extern" "first_match" "foreach"
	     "forkjoin" "genvar" "highz0" "highz1" "ifnone" "ignore_bins"
	     "illegal_bins" "import" "incdir" "include" "inside" "instance"
	     "int" "intersect" "large" "liblist" "library" "local" "longint"
	     "matches" "medium" "modport" "new" "noshowcancelled" "null"
	     "packed" "program" "property" "protected" "pull0" "pull1"
	     "pulsestyle_onevent" "pulsestyle_ondetect" "pure" "rand" "randc"
	     "randcase" "randsequence" "ref" "release" "return" "scalared"
	     "sequence" "shortint" "shortreal" "showcancelled" "small" "solve"
	     "specparam" "static" "string" "strong0" "strong1" "struct"
	     "super" "tagged" "this" "throughout" "timeprecision" "timeunit"
	     "type" "union" "unsigned" "use" "var" "virtual" "void"
	     "wait_order" "weak0" "weak1" "wildcard" "with" "within"
;; verilog-ams-keywords
         "above" "abs" "absdelay" "acos" "acosh" "ac_stim"
	     "aliasparam" "analog" "analysis" "asin" "asinh" "atan" "atan2" "atanh"
	     "branch" "ceil" "connectmodule" "connectrules" "cos" "cosh" "ddt"
	     "ddx" "discipline" "driver_update" "enddiscipline" "endconnectrules"
	     "endnature" "endparamset" "exclude" "exp" "final_step" "flicker_noise"
	     "floor" "flow" "from" "ground" "hypot" "idt" "idtmod" "inf"
	     "initial_step" "laplace_nd" "laplace_np" "laplace_zd" "laplace_zp"
	     "last_crossing" "limexp" "ln" "log" "max" "min" "nature"
	     "net_resolution" "noise_table" "paramset" "potential" "pow" "sin"
	     "sinh" "slew" "sqrt" "tan" "tanh" "timer" "transition" "white_noise"
	     "wreal" "zi_nd" "zi_np" "zi_zd"
;; verilog-font-keywords
         "assign" "case" "casex" "casez" "randcase" "deassign"
	     "default" "disable" "else" "endcase" "endfunction"
	     "endgenerate" "endinterface" "endmodule" "endprimitive"
	     "endspecify" "endtable" "endtask" "final" "for" "force" "return" "break"
	     "continue" "forever" "fork" "function" "generate" "if" "iff" "initial"
	     "interface" "join" "join_any" "join_none" "macromodule" "module" "negedge"
	     "package" "endpackage" "always" "always_comb" "always_ff"
	     "always_latch" "posedge" "primitive" "priority" "release"
	     "repeat" "specify" "table" "task" "unique" "wait" "while"
	     "class" "program" "endclass" "endprogram"
;; verilog-type-keywords
         "and" "buf" "bufif0" "bufif1" "cmos" "defparam" "inout" "input"
         "integer" "localparam" "logic" "mailbox" "nand" "nmos" "nor" "not" "notif0"
         "notif1" "or" "output" "parameter" "pmos" "pull0" "pull1" "pullup"
         "rcmos" "real" "realtime" "reg" "rnmos" "rpmos" "rtran" "rtranif0"
         "rtranif1" "semaphore" "time" "tran" "tranif0" "tranif1" "tri" "tri0" "tri1"
         "triand" "trior" "trireg" "wand" "wire" "wor" "xnor" "xor"
;; verilog-cpp-keywords
         "module" "macromodule" "primitive" "timescale" "define" 
         "ifdef" "ifndef" "else" "endif"
;; verilog-defun-keywords
         "always" "always_comb" "always_ff" "always_latch" "assign"
         "begin" "end" "generate" "endgenerate" "module" "endmodule"
         "specify" "endspecify" "function" "endfunction" "initial" "final"
;; verilog-block-keywords
         "begin" "break" "case" "continue" "else" "end" "endfunction"
         "endgenerate" "endinterface" "endpackage" "endspecify" "endtask"
         "for" "fork" "if" "join" "join_any" "join_none" "repeat" "return"
         "while"
;; verilog-tf-keywords
         "begin" "break" "fork" "join" "join_any" "join_none" "case" 
         "end" "endtask" "endfunction" "if" "else" "for" "while" "repeat"
;; verilog-case-keywords
         "begin" "fork" "join" "join_any" "join_none" "case" "end" 
         "endcase" "if" "else" "for" "repeat"
;verilog-separator-keywords
         "else" "then" "begin"
))

(defvar ac-source-verilog
      '((candidates . (lambda ()
                        (all-completions ac-target verilog-keywords)))
        (candidate-face . ac-verilog-candidate-face)
        (selection-face . ac-verilog-selection-face)
        (requires . 3))
      "Source for verilog.")

;;; auto-complete-verilog.el ends here
