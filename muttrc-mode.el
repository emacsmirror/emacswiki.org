;;; muttrc-mode.el --- Major mode to edit muttrc under Emacs

;;; Copyright (C) 2000, 2001, 2002 Laurent Pelecq
;;; Copyright (C) 2009 Kumar Appaiah
;;;
;;; Authors: Laurent Pelecq <laurent.pelecq@soleil.org>
;;;          Kumar Appaiah <a.kumar@alumni.iitm.ac.in>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Supported Emacs:
;;; ================
;;; This mode has only been tested on Emacs 21.2. If you
;;; encounter problems with older versions or with Xemacs, let me
;;; know.

;;; Installation:
;;; =============
;;; Add this lines to your .emacs:
;;;   (autoload 'muttrc-mode "muttrc-mode.el"
;;;   	"Major mode to edit muttrc files" t)
;;;   (setq auto-mode-alist
;;;   	    (append '(("muttrc\\'" . muttrc-mode))
;;;   		    auto-mode-alist))
;;; Be sure this file is in a directory that appears in the load-path.
;;;
;;; You mail want to use this mode for other files like the mail
;;; aliases file. In that case just add the following lines at the end
;;; of these files:
;;;   ### Local Variables: ***
;;;   ### mode: muttrc ***
;;;   ### End: ***

;;; Customization:
;;; ==============
;;; Execute: M-x configure-group RET muttrc RET
;;;
;;; By default, help on command/variable is displayed automatically
;;; while executing a command to modify them. Disable this feature if
;;; you have problems with.

;;; Description:
;;; ============
;;; This mode first goal is to provide syntax highlighting with
;;; font-lock. The basic fontification appears on strings, comments,
;;; command names and variables. Additional fontification for commands
;;; arguments can be enabled through the customization buffer.
;;;
;;; Main commands are:
;;; C-x c -- muttrc-insert-command
;;; C-x s -- muttrc-set-variable
;;; C-x S -- muttrc-unset-variable
;;;
;;; Type C-h m for all key bindings.

;;; BUGS:
;;; =====
;;; - Multiline commands are not properly handled and can lead to
;;;   unexpected result.

 

;;; Code:

;;; ------------------------------------------------------------
;;; Requirement
;;; ------------------------------------------------------------

(require 'man)

(defconst muttrc-mode-version "$Revision: 1.2 $")

;;; ------------------------------------------------------------
;;; Configurable stuff
;;; ------------------------------------------------------------

(defgroup muttrc nil
  "Muttrc editing commands for Emacs."
  :group 'files
  :prefix "muttrc-")

(defcustom muttrc-manual-path "/usr/share/doc/mutt/manual.txt.gz"
  "Path to the Mutt manual."
  :type 'string
  :group 'muttrc)

(defcustom muttrc-display-help t
  "Display help for each command/variable modification if set."
  :type 'boolean
  :group 'muttrc)

(defcustom muttrc-folder-abbrev ?+
  "Character used to refer to the folder directory."
  :type '(choice (const :tag "+" ?+)
		 (const :tag "=" ?=))
  :group 'muttrc)

(defcustom muttrc-argument-faces-alist
  '((alias . bold)
    (address . default)
    (face . default)
    (color . default)
    (command . default)
    (path . default)
    (function . default)
    (header . default)
    (hook . default)
    (key . default)
    (map . default)
    (mimetype . default)
    (object . default)
    (regexp . default)
    (sequence . default)
    (string . default)
    (hook-type . default))
  "List of faces for the Muttrc command arguments. Standard faces are
symbols like 'bold, 'underline, ... Muttrc files must be revisited in
order for the modifications to take effect."
  :type '(repeat (cons symbol symbol))
  :group 'muttrc)

;;; ------------------------------------------------------------
;;; For backward compatibility
;;; ------------------------------------------------------------

(or (functionp 'match-string-no-properties)
    (defalias 'match-string-no-properties 'match-string))

;;; ------------------------------------------------------------
;;; Mutt variables and commands
;;; ------------------------------------------------------------

(defconst muttrc-arg-handler-alist
  '((alias muttrc-get-word "Alias")
    (boolean muttrc-get-boolean "Enable")
    (number muttrc-get-number "Number")
    (address muttrc-get-string "Address")
    (face muttrc-get-from-list "Face" muttrc-face-alist t)
    (color muttrc-get-from-list "Color" muttrc-color-alist)
    (command muttrc-get-command "Command")
    (statement muttrc-get-statement "Command")
    (assignment muttrc-get-assignment "Variable" t)
    (variable muttrc-get-assignment "Variable" nil)
    (path muttrc-get-path "Path")
    (function muttrc-get-from-list "Function" muttrc-mutt-function-alist)
    (header muttrc-get-from-list "Header name" muttrc-header-alist)
    (hook-type muttrc-get-from-list "Hook" muttrc-hook-alist t)
    (key muttrc-get-string "Key")
    (map muttrc-get-from-list "Map" muttrc-map-alist t)
    (mimetype muttrc-get-from-list "MIME type" muttrc-mimetype-alist)
    (object muttrc-get-from-list "Object" muttrc-object-alist)
    (regexp muttrc-get-string "Regular expression")
    (sequence muttrc-get-string "Sequence")
    (string muttrc-get-string "String")
    (alias-sort-order muttrc-get-from-list "Sort order"
		      muttrc-alias-sort-order-alist)
    (aux-sort-order  muttrc-get-from-list "Sort order"
		    muttrc-aux-sort-order-alist)
    (browser-sort-order muttrc-get-from-list "Sort order"
			muttrc-browser-sort-order-alist)
    (pgp-sort-order muttrc-get-from-list "Sort order"
		    muttrc-pgp-sort-order-alist)
    (quadoption muttrc-get-from-list "Option" muttrc-quadoption-alist)
    (sort-order muttrc-get-from-list "Sort order"
		muttrc-sort-order-alist))
  "List of handler for each type of argument. The format is:
\(ARG-TYPE FACE HANDLER PROMPT HANDLER-ARGS\).
The PROMPT can be overwritten by in command description.")

(defconst muttrc-face-alist
  '(("none" . 1) ("bold" . 2) ("underline" . 3)
    ("reverse" . 4) ("standout". 5)))

(defconst muttrc-color-alist
  '(("default" . 0)
    ("black" . 1) ("blue" . 2) ("cyan" . 3) ("green" . 4)
    ("magenta" . 5) ("red" . 6) ("white" . 7) ("yellow" . 8)
    ("brightdefault" . 9)
    ("brightblack" . 10) ("brightblue" . 11) ("brightcyan" . 12)
    ("brightgreen" . 13) ("brightmagenta" . 14) ("brightred" . 15)
    ("brightwhite" . 16) ("brightyellow" . 17)))

(defconst muttrc-object-alist
  '(("attachment" . 0)
    ("body" . 1)
    ("bold" . 2)
    ("error" . 3)
    ("hdrdefault" . 4)
    ("header" . 5)
    ("index" . 6)
    ("indicator" . 7)
    ("markers" . 8)
    ("message" . 9)
    ("normal" . 10)
    ("quoted" . 11)
    ("search" . 12)
    ("signature" . 13)
    ("status" . 14)
    ("tilde" . 15)
    ("tree" . 16)
    ("underline" . 17))
  "Mutt object on which color apply.")

(defconst muttrc-header-alist
  '(("content-transfer-encoding" . 0)
    ("content-type" . 1)
    ("date" . 2)
    ("from" . 3)
    ("message-id" . 4)
    ("mime-version" . 5)
    ("organization" . 6)
    ("received" . 7)
    ("reply-to" . 8)
    ("resent-from" . 9)
    ("subject" . 10)
    ("to" . 11)
    ("x-accept-language" . 12)
    ("x-mailer" . 13)
    ("x-mimetrack" . 14)
    ("x-sender" . 15)))

(defconst muttrc-hook-alist
  '(("folder-hook" . 0) ("send-hook" . 1) ("save-hook" . 2)
    ("mbox-hook" . 3) ("fcc-hook" . 4) ("fcc-save-hook" . 5)
    ("message-hook" . 5) ("charset-hook" . 6) ("iconv-hook" . 7)
    ("account-hook" . 8) ("append-hook" . 9) ("close-hook" . 10)
    ("crypt-hook" . 11) ("send2-hook" . 12) ("reply-hook" . 13)
    ("open-hook" . 14)))

(defconst muttrc-map-alist
  '(("alias" . 0) ("attach" . 1) ("browser" . 2) ("compose" . 3)
    ("editor" . 4) ("generic" . 5) ("index" . 6) ("pager" . 7)
    ("pgp" . 8) ("postpone" . 9) ("query" . 10)))

(defconst muttrc-mimetype-alist
  '(("application/andrew-inset" "ez")
    ("application/excel" "xls")
    ("application/fractals" "fif")
    ("application/java-archive" "jar")
    ("application/mac-binhex40" "hqx")
    ("application/msword" "doc" "dot")
    ("application/octet-stream" "exe" "bin")
    ("application/oda" "oda")
    ("application/pdf" "pdf")
    ("application/pdf")
    ("application/pgp" "pgp")
    ("application/postscript" "ai" "eps" "ps" "PS")
    ("application/pre-encrypted" "enc")
    ("application/rtf" "rtf")
    ("application/vnd.lotus-wordpro" "lwp" "sam")
    ("application/vnd.ms-access" "mdb" "mda" "mde")
    ("application/vnd.ms-excel" "xls")
    ("application/vnd.ms-powerpoint" "ppt" "pot" "ppa" "pps" "pwz")
    ("application/vnd.ms-schedule" "scd" "sch" "sc2")
    ("application/wordperfect5.1" "wpd" "wp6")
    ("application/x-arj-compressed" "arj")
    ("application/x-bcpio" "bcpio")
    ("application/x-chess-pgn" "pgn")
    ("application/x-cpio" "cpio")
    ("application/x-csh" "csh")
    ("application/x-debian-package" "deb")
    ("application/x-dvi" "dvi")
    ("application/x-fortezza-ckl" "ckl")
    ("application/x-gtar" "gtar")
    ("application/x-gunzip" "gz")
    ("application/x-hdf" "hdf")
    ("application/x-javascript" "js" "mocha")
    ("application/x-javascript-config" "jsc")
    ("application/x-latex" "latex")
    ("application/x-mif" "mif")
    ("application/x-msdos-program" "com" "exe" "bat")
    ("application/x-netcdf" "cdf" "nc")
    ("application/x-ns-proxy-autoconfig" "pac")
    ("application/x-ns-proxy-autoconfig")
    ("application/x-perl" "pl" "pm")
    ("application/x-pkcs7-crl" "crl")
    ("application/x-pkcs7-mime" "p7m" "p7c")
    ("application/x-pkcs7-signature" "p7s")
    ("application/x-rar-compressed" "rar")
    ("application/x-sh" "sh")
    ("application/x-shar" "shar")
    ("application/x-stuffit" "sit")
    ("application/x-sv4cpio" "sv4cpio")
    ("application/x-sv4crc" "sv4crc")
    ("application/x-tar" "tar")
    ("application/x-tar-gz" "tgz" "tar.gz")
    ("application/x-tcl" "tcl")
    ("application/x-tex" "tex")
    ("application/x-texinfo" "texi" "texinfo")
    ("application/x-troff" "t" "tr" "roff")
    ("application/x-troff-man" "man")
    ("application/x-troff-me" "me")
    ("application/x-troff-ms" "ms")
    ("application/x-ustar" "ustar")
    ("application/x-wais-source" "src")
    ("application/x-zip-compressed" "zip")
    ("audio/basic" "au" "snd")
    ("audio/basic" "snd")
    ("audio/midi" "mid" "midi")
    ("audio/ulaw" "au")
    ("audio/x-aiff" "aif" "aifc" "aiff")
    ("audio/x-aiff" "aif" "aiff" "aifc")
    ("audio/x-wav" "wav")
    ("image/gif" "gif")
    ("image/ief" "ief")
    ("image/jpeg" "jpe" "jpeg" "jpg")
    ("image/png" "png")
    ("image/tiff" "tif" "tiff")
    ("image/tiff")
    ("image/x-MS-bmp" "bmp")
    ("image/x-cmu-raster" "ras")
    ("image/x-photo-cd" "pcd")
    ("image/x-portable-anymap" "pnm")
    ("image/x-portable-bitmap" "pbm")
    ("image/x-portable-graymap" "pgm")
    ("image/x-portable-pixmap" "ppm")
    ("image/x-rgb" "rgb")
    ("image/x-xbitmap" "xbm")
    ("image/x-xpixmap" "xpm")
    ("image/x-xwindowdump" "xwd")
    ("text/html" "html" "htm" "shtml")
    ("text/plain" "txt" "text")
    ("text/richtext" "rtx")
    ("text/tab-separated-values" "tsv")
    ("text/x-setext" "etx")
    ("text/x-vcard" "vcf")
    ("text/x-vcard")
    ("video/dl" "dl")
    ("video/fli" "fli")
    ("video/gl" "gl")
    ("video/mpeg" "mpeg" "mpg" "mpe" "mpv" "vbs" "mpegv")
    ("video/quicktime" "qt" "mov" "moov")
    ("video/x-msvideo" "avi")
    ("video/x-sgi-movie" "movie")
    ("x-world/x-vrml" "vrm" "vrml" "wrl")))

(defconst muttrc-command-alist
  '(
    ("folder-hook"		((string) (statement)) nil nil)
    ("alias"			((alias) (address)) t nil)
    ("unalias"			((alias) (address)) t nil)
    ("alternative_order"	((mimetype)) t nil)
    ("auto_view"		((mimetype)) t nil)
    ("bind"			((map) (key) (function)) nil t)
    ("color"			((object)
				 (color "Foreground")
				 (color "Background")
				 (regexp)) nil t)
    ("charset-hook"		((string "Alias")
				 (string "Charset")) nil nil)
    ("fcc-hook"			((regexp) (path)) nil nil)
    ("fcc-save-hook"		((regexp) (path)) nil nil)
    ("folder-hook"		((regexp) (statement)) nil nil)
    ("ignore"			((header)) t nil)
    ("iconv-hook"		((string "Charset")
				 (string "Local charset")) nil nil)
    ("unignore"			((header)) t nil)
    ("hdr_order"		((header)) t nil)
    ("unhdr_order"		((header)) t nil)
    ("lists"			((address)) t nil)
    ("unlists"			((address)) t nil)
    ("macro"			((map) (key) (sequence)
				 (string "Description")) nil t)
    ("mailboxes"		((path)) t nil)
    ("mono"			((object) (face) (regexp)) nil t)
    ("mbox-hook"		((regexp) (path)) nil nil)
    ("message-hook"		((regexp) (statement)) nil nil)
    ("my_hdr"			((string "Header")) nil nil)
    ("unmy_hdr"			((header)) t nil)
    ("push"			((string)) nil nil)
    ("pgp-hook"			((regexp)
				 (string "Keyid")) nil nil)
    ("save-hook"		((regexp) (path)) nil nil)
    ("score"			((regexp)
				 (number "Value")) nil nil)
    ("unscore"			((regexp)) t nil)
    ("send-hook"		((regexp) (statement)) nil nil)
    ("source"			((path)) nil nil)
    ("subscribe"		((address)) t nil)
    ("unsubscribe"		((address)) t nil)
    ("unhook"			((hook-type)) nil nil)
    ("alternates"		((regexp)) nil nil)
    ("unalternates"		((regexp)) nil nil))
  "List of muttrc commands with their arguments. Format is:
COMMAND '\(ARG1 ARG2 ...\) REPEAT OPTIONAL
REPEAT and OPTIONAL apply to the last argument.
ARGn is the list of arguments for muttrc-call-arg-handler. Each args
is a list \(ARGTYPE \[ARGNAME\]\).")

(defconst muttrc-statement-alist
  (append
   '(("set"			((assignment)) t nil)
     ("unset"			((variable)) t nil))
   muttrc-command-alist)
  "Additional muttrc commands with their arguments that are handled
differently. See muttrc-command-alist")


(defconst muttrc-variables-alist
  '(("abort_nosubject" quadoption "ask-yes")
    ("abort_unmodified" quadoption "yes")
    ("alias_file" path "~/.muttrc")
    ("alias_format" string "%4n %2f %t %-10a   %r")
    ("allow_8bit" boolean t)
    ("allow_ansi" boolean nil)
    ("arrow_cursor" boolean nil)
    ("ascii_chars" boolean nil)
    ("askbcc" boolean nil)
    ("askcc" boolean nil)
    ("assumed_charset" string "us-ascii")
    ("attach_format" string "%u%D%I %t%4n %T%.40d%> [%.7m/%.10M, %.6e%?C?, %C?, %s] ")
    ("attach_sep" string "\\n")
    ("attach_split" boolean t)
    ("attribution" string "On %d, %n wrote:")
    ("autoedit" boolean nil)
    ("auto_tag" boolean nil)
    ("beep" boolean t)
    ("beep_new" boolean nil)
    ("bounce" quadoption "ask-yes")
    ("bounce_delivered" boolean t)
    ("braille_friendly" boolean nil)
    ("charset" string "")
    ("check_new" boolean t)
    ("collapse_unread" boolean t)
    ("uncollapse_jump" boolean nil)
    ("compose_format" string "-- Mutt: Compose  [Approx. msg size: %l   Atts: %a]%>-")
    ("config_charset" string "")
    ("confirmappend" boolean t)
    ("confirmcreate" boolean t)
    ("connect_timeout" number 30)
    ("content_type" string "text/plain")
    ("copy" quadoption "yes")
    ("crypt_use_gpgme" boolean nil)
    ("crypt_autopgp" boolean t)
    ("crypt_autosmime" boolean t)
    ("date_format" string "!%a, %b %d, %Y at %I:%M:%S%p %Z")
    ("default_hook" string "~f %s !~P | (~P ~C %s)")
    ("delete" quadoption "ask-yes")
    ("delete_untag" boolean t)
    ("digest_collapse" boolean t)
    ("display_filter" path "")
    ("dotlock_program" path "/usr/bin/mutt_dotlock")
    ("dsn_notify" string "")
    ("dsn_return" string "")
    ("duplicate_threads" boolean t)
    ("edit_headers" boolean nil)
    ("editor" path "")
    ("encode_from" boolean nil)
    ("envelope_from_address" e-mail "")
    ("escape" string "~")
    ("fast_reply" boolean nil)
    ("fcc_attach" boolean t)
    ("fcc_clear" boolean nil)
    ("file_charset" string "")
    ("folder" path "~/Mail")
    ("folder_format" string "%2C %t %N %F %2l %-8.8u %-8.8g %8s %d %f")
    ("followup_to" boolean t)
    ("force_name" boolean nil)
    ("forward_decode" boolean t)
    ("forward_edit" quadoption "yes")
    ("forward_format" string "[%a: %s]")
    ("forward_quote" boolean nil)
    ("from" e-mail "")
    ("gecos_mask" regular "^[^,]*")
    ("hdrs" boolean t)
    ("header" boolean nil)
    ("help" boolean t)
    ("hidden_host" boolean nil)
    ("hide_limited" boolean nil)
    ("hide_missing" boolean t)
    ("hide_thread_subject" boolean t)
    ("hide_top_limited" boolean nil)
    ("hide_top_missing" boolean t)
    ("history" number 10)
    ("honor_followup_to" quadoption "yes")
    ("hostname" string "")
    ("ignore_list_reply_to" boolean nil)
    ("imap_authenticators" string "")
    ("imap_check_subscribed" boolean nil)
    ("imap_delim_chars" string "/.")
    ("imap_headers" string "")
    ("imap_home_namespace" string "")
    ("imap_idle" boolean nil)
    ("imap_keepalive" number 900)
    ("imap_list_subscribed" boolean nil)
    ("imap_login" string "")
    ("imap_pass" string "")
    ("imap_passive" boolean t)
    ("imap_peek" boolean t)
    ("imap_servernoise" boolean t)
    ("imap_user" string "")
    ("implicit_autoview" boolean nil)
    ("include" quadoption "ask-yes")
    ("include_onlyfirst" boolean nil)
    ("indent_string" string "> ")
    ("index_format" string "%4C %Z %{%b %d} %-15.15L (%?l?%4l&%4c?) %s")
    ("hdr_format" string "%4C %Z %{%b %d} %-15.15L (%?l?%4l&%4c?) %s")
    ("ispell" path "ispell")
    ("keep_flagged" boolean nil)
    ("locale" string "C")
    ("mail_check" number 5)
    ("mailcap_path" string "")
    ("mailcap_sanitize" boolean t)
    ("maildir_mtime" boolean nil)
    ("header_cache" path "")
    ("maildir_header_cache_verify" boolean t)
    ("header_cache_pagesize" string "16384")
    ("maildir_trash" boolean nil)
    ("mark_old" boolean t)
    ("markers" boolean t)
    ("mask" regular "!^\.[^.]")
    ("mbox" path "~/mbox")
    ("mbox_type" folder mbox)
    ("metoo" boolean nil)
    ("menu_context" number 0)
    ("menu_move_off" boolean t)
    ("menu_scroll" boolean nil)
    ("meta_key" boolean nil)
    ("mh_purge" boolean nil)
    ("mh_seq_flagged" string "flagged")
    ("mh_seq_replied" string "replied")
    ("mh_seq_unseen" string "unseen")
    ("mime_forward" quadoption "no")
    ("mime_forward_decode" boolean nil)
    ("mime_forward_rest" quadoption "yes")
    ("pgp_mime_signature_filename" string "signature.asc")
    ("pgp_mime_signature_description" string "Digital signature")
    ("mix_entry_format" string "%4n %c %-16s %a")
    ("mixmaster" path "mixmaster")
    ("move" quadoption "ask-no")
    ("message_cachedir" path "")
    ("message_format" string "%s")
    ("narrow_tree" boolean nil)
    ("net_inc" number 10)
    ("pager" path "builtin")
    ("pager_context" number 0)
    ("pager_format" string "-%Z- %C/%m: %-20.20n   %s")
    ("pager_index_lines" number 0)
    ("pager_stop" boolean nil)
    ("crypt_autosign" boolean nil)
    ("crypt_autoencrypt" boolean nil)
    ("pgp_ignore_subkeys" boolean t)
    ("crypt_replyencrypt" boolean t)
    ("crypt_replysign" boolean nil)
    ("crypt_replysignencrypted" boolean nil)
    ("crypt_timestamp" boolean t)
    ("pgp_use_gpg_agent" boolean nil)
    ("crypt_verify_sig" quadoption "yes")
    ("pgp_verify_sig" quadoption "yes")
    ("smime_is_default" boolean nil)
    ("smime_ask_cert_label" boolean t)
    ("smime_decrypt_use_default_key" boolean t)
    ("pgp_entry_format" string "%4n %t%f %4l/0x%k %-4a %2c %u")
    ("pgp_good_sign" regular "")
    ("pgp_check_exit" boolean t)
    ("pgp_long_ids" boolean nil)
    ("pgp_retainable_sigs" boolean nil)
    ("pgp_autoinline" boolean nil)
    ("pgp_replyinline" boolean nil)
    ("pgp_show_unusable" boolean t)
    ("pgp_sign_as" string "")
    ("pgp_strict_enc" boolean t)
    ("pgp_timeout" number 300)
    ("pgp_sort_keys" sort address)
    ("pgp_mime_auto" quadoption "ask-yes")
    ("pgp_auto_decode" boolean nil)
    ("pgp_decode_command" string "")
    ("pgp_getkeys_command" string "")
    ("pgp_verify_command" string "")
    ("pgp_decrypt_command" string "")
    ("pgp_clearsign_command" string "")
    ("pgp_sign_command" string "")
    ("pgp_encrypt_sign_command" string "")
    ("pgp_encrypt_only_command" string "")
    ("pgp_import_command" string "")
    ("pgp_export_command" string "")
    ("pgp_verify_key_command" string "")
    ("pgp_list_secring_command" string "")
    ("pgp_list_pubring_command" string "")
    ("forward_decrypt" boolean t)
    ("smime_timeout" number 300)
    ("smime_encrypt_with" string "")
    ("smime_keys" path "")
    ("smime_ca_location" path "")
    ("smime_certificates" path "")
    ("smime_decrypt_command" string "")
    ("smime_verify_command" string "")
    ("smime_verify_opaque_command" string "")
    ("smime_sign_command" string "")
    ("smime_sign_opaque_command" string "")
    ("smime_encrypt_command" string "")
    ("smime_pk7out_command" string "")
    ("smime_get_cert_command" string "")
    ("smime_get_signer_cert_command" string "")
    ("smime_import_cert_command" string "")
    ("smime_get_cert_email_command" string "")
    ("smime_default_key" string "")
    ("ssl_force_tls" boolean nil)
    ("ssl_starttls" quadoption "yes")
    ("certificate_file" path "~/.mutt_certificates")
    ("ssl_use_sslv3" boolean t)
    ("ssl_use_tlsv1" boolean t)
    ("ssl_min_dh_prime_bits" number 0)
    ("ssl_ca_certificates_file" path "")
    ("pipe_split" boolean nil)
    ("pipe_decode" boolean nil)
    ("pipe_sep" string "\\n")
    ("pop_authenticators" string "")
    ("pop_auth_try_all" boolean t)
    ("pop_checkinterval" number 60)
    ("pop_delete" quadoption "ask-no")
    ("pop_host" string "")
    ("pop_last" boolean nil)
    ("pop_reconnect" quadoption "ask-yes")
    ("pop_user" string "")
    ("pop_pass" string "")
    ("post_indent_string" string "")
    ("postpone" quadoption "ask-yes")
    ("postponed" path "~/postponed")
    ("preconnect" string "")
    ("print" quadoption "ask-no")
    ("print_command" path "lpr")
    ("print_decode" boolean t)
    ("print_split" boolean nil)
    ("prompt_after" boolean t)
    ("query_command" path "")
    ("quit" quadoption "yes")
    ("quote_regexp" regular "^([ \t]*[|>:}#])+")
    ("read_inc" number 10)
    ("read_only" boolean nil)
    ("realname" string "")
    ("recall" quadoption "ask-yes")
    ("record" path "~/sent")
    ("reply_regexp" regular "^(re([\[0-9\]+])*|aw):[ \t]*")
    ("reply_self" boolean nil)
    ("reply_to" quadoption "ask-yes")
    ("resolve" boolean t)
    ("reverse_alias" boolean nil)
    ("reverse_name" boolean nil)
    ("reverse_realname" boolean t)
    ("rfc2047_parameters" boolean nil)
    ("save_address" boolean nil)
    ("save_empty" boolean t)
    ("save_name" boolean nil)
    ("score" boolean t)
    ("score_threshold_delete" number -1)
    ("score_threshold_flag" number 9999)
    ("score_threshold_read" number -1)
    ("send_charset" string "us-ascii:iso-8859-1:utf-8")
    ("sendmail" path "/usr/sbin/sendmail -oem -oi")
    ("sendmail_wait" number 0)
    ("shell" path "")
    ("sig_dashes" boolean t)
    ("sig_on_top" boolean nil)
    ("signature" path "~/.signature")
    ("simple_search" string "~f %s | ~s %s")
    ("smart_wrap" boolean t)
    ("smileys" regular "(>From )|(:[-^]?[][)(><}{|/DP])")
    ("sleep_time" number 1)
    ("sort" sort date)
    ("sort_alias" sort alias)
    ("sort_aux" sort date)
    ("sort_browser" sort alpha)
    ("sort_re" boolean t)
    ("spam_separator" string ",")
    ("spoolfile" path "")
    ("status_chars" string "-*%A")
    ("status_format" string "-%r-Mutt: %f [Msgs:%?M?%M/?%m%?n? New:%n?%?o? Old:%o?%?d? Del:%d?%?F? Flag:%F?%?t? Tag:%t?%?p? Post:%p?%?b? Inc:%b?%?l? %l?]---(%s/%S)-%>-(%P)---")
    ("status_on_top" boolean nil)
    ("strict_mime" boolean t)
    ("strict_threads" boolean nil)
    ("suspend" boolean t)
    ("text_flowed" boolean nil)
    ("thread_received" boolean nil)
    ("thorough_search" boolean nil)
    ("tilde" boolean nil)
    ("timeout" number 600)
    ("tmpdir" path "")
    ("to_chars" string " +TCFL")
    ("tunnel" string "")
    ("use_8bitmime" boolean nil)
    ("use_domain" boolean t)
    ("use_envelope_from" boolean nil)
    ("use_from" boolean t)
    ("use_idn" boolean t)
    ("use_ipv6" boolean t)
    ("user_agent" boolean t)
    ("visual" path "")
    ("wait_key" boolean t)
    ("weed" boolean t)
    ("wrap_search" boolean t)
    ("wrapmargin" number 0)
    ("write_inc" number 10)
    ("write_bcc" boolean t)
    ("xterm_icon" string "M%?n?AIL&ail?")
    ("xterm_set_titles" boolean nil)
    ("xterm_title" string "Mutt with %?m?%m messages&no messages?%?n? [%n NEW]?"))
  "List of muttrc variables. Format is:
VARIABLE TYPE DEFAULT"
  )

(defconst muttrc-mutt-function-alist
  '(("attach-file" . 0)
    ("attach-key" . 1)
    ("attach-message" . 2)
    ("backspace" . 3)
    ("backward-char" . 4)
    ("bol" . 5)
    ("bottom-page" . 6)
    ("bounce-message" . 7)
    ("buffy-cycle" . 8)
    ("change-dir" . 9)
    ("change-folder" . 10)
    ("change-folder-readonly" . 11)
    ("check-new" . 12)
    ("clear-flag" . 13)
    ("complete" . 14)
    ("complete-query" . 15)
    ("copy-file" . 16)
    ("copy-message" . 17)
    ("create-alias" . 18)
    ("current-bottom" . 19)
    ("current-middle" . 20)
    ("current-top" . 21)
    ("decode-copy" . 22)
    ("decode-save" . 23)
    ("delete-char" . 24)
    ("delete-entry" . 25)
    ("delete-message" . 26)
    ("delete-pattern" . 27)
    ("delete-subthread" . 28)
    ("delete-thread" . 29)
    ("detach-file" . 30)
    ("display-address" . 31)
    ("display-message" . 32)
    ("display-toggle-weed" . 33)
    ("edit" . 34)
    ("edit-bcc" . 35)
    ("edit-cc" . 36)
    ("edit-description" . 37)
    ("edit-encoding" . 38)
    ("edit-fcc" . 39)
    ("edit-file" . 40)
    ("edit-from" . 41)
    ("edit-headers" . 42)
    ("edit-message" . 43)
    ("edit-mime" . 44)
    ("edit-reply-to" . 45)
    ("edit-subject" . 46)
    ("edit-to" . 47)
    ("edit-type" . 48)
    ("enter-command" . 49)
    ("enter-mask" . 50)
    ("eol" . 51)
    ("exit" . 52)
    ("extract-keys" . 53)
    ("fetch-mail" . 54)
    ("filter-entry" . 55)
    ("first-entry" . 56)
    ("flag-message" . 57)
    ("forget-passphrase" . 58)
    ("forward-char" . 59)
    ("forward-message" . 60)
    ("group-reply" . 61)
    ("half-down" . 62)
    ("half-up" . 63)
    ("help" . 64)
    ("history-down" . 65)
    ("history-up" . 66)
    ("ispell" . 67)
    ("jump" . 68)
    ("kill-eol" . 69)
    ("kill-line" . 70)
    ("kill-word" . 71)
    ("last-entry" . 72)
    ("limit" . 73)
    ("list-reply" . 74)
    ("mail" . 75)
    ("mail-key" . 76)
    ("mark-as-new" . 77)
    ("middle-page" . 78)
    ("new-mime" . 79)
    ("next-entry" . 80)
    ("next-line" . 81)
    ("next-new" . 82)
    ("next-page" . 83)
    ("next-subthread" . 84)
    ("next-thread" . 85)
    ("next-undeleted" . 86)
    ("next-unread" . 87)
    ("parent-message" . 88)
    ("pgp-menu" . 89)
    ("pipe-entry" . 90)
    ("pipe-message" . 91)
    ("postpone-message" . 92)
    ("previous-entry" . 93)
    ("previous-line" . 94)
    ("previous-new" . 95)
    ("previous-page" . 96)
    ("previous-subthread" . 97)
    ("previous-thread" . 98)
    ("previous-undeleted" . 99)
    ("previous-unread" . 100)
    ("print-entry" . 101)
    ("print-message" . 102)
    ("query" . 103)
    ("query-append" . 104)
    ("quit" . 105)
    ("quote-char" . 106)
    ("read-subthread" . 107)
    ("read-thread" . 108)
    ("recall-message" . 109)
    ("redraw-screen" . 110)
    ("refresh" . 111)
    ("rename-file" . 112)
    ("reply" . 113)
    ("save-entry" . 114)
    ("save-message" . 115)
    ("search" . 116)
    ("search-next" . 117)
    ("search-opposite" . 118)
    ("search-reverse" . 119)
    ("search-toggle" . 120)
    ("select-entry" . 121)
    ("select-new" . 122)
    ("send-message" . 123)
    ("set-flag" . 124)
    ("shell-escape" . 125)
    ("show-limit" . 126)
    ("show-version" . 127)
    ("skip-quoted" . 128)
    ("sort" . 129)
    ("sort-mailbox" . 130)
    ("sort-reverse" . 131)
    ("subscribe" . 132)
    ("sync-mailbox" . 133)
    ("tag-entry" . 134)
    ("tag-message" . 135)
    ("tag-pattern" . 136)
    ("tag-prefix" . 137)
    ("tag-thread" . 138)
    ("toggle-mailboxes" . 139)
    ("toggle-new" . 140)
    ("toggle-quoted" . 141)
    ("toggle-subscribed" . 142)
    ("toggle-unlink" . 143)
    ("toggle-write" . 144)
    ("top" . 145)
    ("top-page" . 146)
    ("undelete-entry" . 147)
    ("undelete-message" . 148)
    ("undelete-pattern" . 149)
    ("undelete-subthread" . 150)
    ("undelete-thread" . 151)
    ("unsubscribe" . 152)
    ("untag-pattern" . 153)
    ("verify-key" . 154)
    ("view-attach" . 155)
    ("view-attachments" . 156)
    ("view-file" . 157)
    ("view-mailcap" . 158)
    ("view-name" . 159)
    ("view-text" . 160)
    ("write-fcc" . 161))
  "List of Mutt command (not muttrc!)")

(defconst muttrc-alias-sort-order-alist
  '(("address" . 0) ("alias" . 1)  ("unsorted" . 2)))

(defconst muttrc-aux-sort-order-alist
  '(("date-sent" . 0) ("reverse-date-sent" . 1) ("last-date-sent" . 2)
    ("date-received" . 3) ("reverse-date-received" . 4)
    ("last-date-received" . 5)
    ("from" . 6) ("reverse-from" . 7) ("last-from" . 8)
    ("mailbox-order" . 9) ("reverse-mailbox-order" . 10)
    ("last-mailbox-order" . 11)
    ("score" . 12) ("reverse-score" . 13) ("last-score" . 14)
    ("size" . 15) ("reverse-size" . 16) ("last-size" . 17)
    ("subject" . 18) ("reverse-subject" . 19) ("last-subject" . 20)
    ("threads" . 21) ("reverse-threads" . 22) ("last-threads" . 23)
    ("to" . 24) ("reverse-to" . 25) ("last-to" . 26)))

(defconst muttrc-browser-sort-order-alist
  '(("alpha" . 0) ("date" . 1) ("size" . 2) ("unsorted" . 3)))

(defconst muttrc-pgp-sort-order-alist
  '(("address" . 0) ("date" . 1) ("keyid" . 2)
    ("reverse-address" . 3) ("reverse-date" . 4)
    ("reverse-keyid" . 5) ("reverse-trust" . 6)
    ("trust" . 7)))

(defconst muttrc-quadoption-alist
  '(("yes" .0) ("no" .1) ("ask-yes" .2) ("ask-no" .3)))

(defconst muttrc-sort-order-alist
  '(("date-sent" . 0) ("reverse-date-sent" . 1)
    ("date-received" . 2) ("reverse-date-received" . 3)
    ("from" . 4) ("reverse-from" . 5)
    ("mailbox-order" . 6) ("reverse-mailbox-order" . 7)
    ("score" . 8) ("reverse-score" . 9)
    ("size" . 10) ("reverse-size" . 11)
    ("subject" . 12) ("reverse-subject" . 13)
    ("threads" . 14) ("reverse-threads" . 15)
    ("to" . 16) ("reverse-to" . 17)))

;;; ------------------------------------------------------------
;;; Font-lock definitions
;;; ------------------------------------------------------------

(defun muttrc-string-regexp (quote-char)
  (let ((c (char-to-string quote-char)))
    (format "%s\\([^\n%s]\\|[\\].\\)*%s" c c c)))

(defvar muttrc-generic-arg-regexp
  (concat "\\("
	  (muttrc-string-regexp ?\")
	  "\\|"
	  "'\\([^']*\\)'"
	  "\\|"
	  (muttrc-string-regexp ?\`)
	  "\\|"
	  "\\([^\n\t \"'`#;\\]\\|[\\].\\)+"
	  "\\)"))

(defvar muttrc-generic-arg-sequence-regexp
  (concat "\\(\\s-*" muttrc-generic-arg-regexp "+\\)*"))

(defvar muttrc-non-command-keyword-regexp
  "\\(^\\|;\\)\\s-*\\<\\(set\\|unset\\|toggle\\|reset\\)\\>")

(defvar muttrc-variable-regexp
  (concat "\\<\\(\\(no\\|inv\\)?\\("
	  (mapconcat 'car muttrc-variables-alist "\\|")
	  "\\)\\)\\>"))

(defvar muttrc-assignement-regexp
  (concat muttrc-variable-regexp
	  "\\s-*\\(=\\s-*" muttrc-generic-arg-regexp "\\)?"))

(defun muttrc-search-command-forward (command &optional limit)
  (let ((cmd-desc (assoc command muttrc-command-alist)))
    (if cmd-desc
	(let ((cmd-match-data '())
	      (cmd-args (cadr cmd-desc))
	      (origin (point))
	      beg-0 end-0)
	  (catch 'done
	    (while (and (not cmd-match-data)
			(re-search-forward
			 (concat "\\(;\\|^\\)\\s-*\\(" command "\\)")
			 limit t))
	      (let ((beg (nth 4 (match-data)))
		    (end (nth 5 (match-data))))
		(setq beg-0 beg)
		(setq cmd-match-data (list beg end)))
	      (let ((args cmd-args))
		(while args
		  (let ((arg-type (caar args))
			(arg-re (if (null (cdr args))
				    muttrc-generic-arg-sequence-regexp
				  muttrc-generic-arg-regexp)))
		    (skip-syntax-forward "-")
		    (if (looking-at arg-re)
			(let ((beg (nth 0 (match-data)))
			      (end (nth 1 (match-data))))
			  (goto-char end)
			  (setq cmd-match-data (append cmd-match-data
						       (list beg end)))
			  (setq end-0 end)
			  (setq args (cdr args)))
		      (progn
			(setq args nil)
			(setq cmd-match-data nil)))))
		(when cmd-match-data
		  (set-match-data (cons beg-0
					(cons end-0
					      cmd-match-data)))
		  (throw 'done t))))
	    (goto-char origin)
	    nil)))))


(defun muttrc-font-lock-keywords ()
  (let ((command-alist muttrc-command-alist)
	keywords)
    (while command-alist
      (let* ((cmd (caar command-alist))
	     (args (cadr (car command-alist)))
	     (regexp (eval ; Simulate a closure
		      (list
		       'lambda '(&optional limit)
		       (list 'muttrc-search-command-forward cmd 'limit))))
	     (hilighters '((1 font-lock-keyword-face)))
	     (n 2))
	(while args
	  (let ((arg-type (caar args))
		(last-arg-p (null (cdr args))))
	    (setq hilighters
		  (append hilighters
			  (let ((face
				 (or (cdr-safe
				      (assoc arg-type
					     muttrc-argument-faces-alist))
				     'default)))
			    (list (append (list n (list 'quote face))
					  (if last-arg-p '(nil t))))))))
	  (setq n (1+ n))
	  (setq args (cdr args)))
	(setq keywords (append keywords (list (cons regexp hilighters))))
	(setq command-alist (cdr command-alist))))
     (append keywords
	     (list
	      (list muttrc-non-command-keyword-regexp 2
		    font-lock-keyword-face)
	      (list muttrc-assignement-regexp 1
		    font-lock-variable-name-face)))
    ))

;;; ------------------------------------------------------------
;;; Mode specific customization
;;; ------------------------------------------------------------

(defconst muttrc-mode-map nil
  "The keymap that is used in Muttrc mode.")
(if (null muttrc-mode-map)
    (setq muttrc-mode-map
	  (let ((map (make-sparse-keymap))
		(help-map (make-sparse-keymap))
		(ctrl-c-map (make-sparse-keymap)))
	    (define-key map "\C-c" ctrl-c-map)
	    (define-key ctrl-c-map "c" 'muttrc-insert-command)
	    (define-key ctrl-c-map "C" 'comment-region)
	    (define-key ctrl-c-map "s" 'muttrc-set-variable)
	    (define-key ctrl-c-map "S" 'muttrc-unset-variable)
	    (define-key ctrl-c-map "f" 'muttrc-find-variable-in-buffer)
	    (define-key ctrl-c-map "h" help-map)
	    (define-key help-map "m" 'muttrc-find-manual-file)
	    (define-key help-map "v" 'muttrc-find-variable-help)
	    (define-key help-map "c" 'muttrc-find-command-help)
	    map)))

(defvar muttrc-mode-syntax-table nil)
(when (null muttrc-mode-syntax-table)
  (setq muttrc-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?#  "<     " muttrc-mode-syntax-table)
  (modify-syntax-entry ?\n ">     " muttrc-mode-syntax-table)
  (modify-syntax-entry ?\' "$     " muttrc-mode-syntax-table)
  (modify-syntax-entry ?\' "$     " muttrc-mode-syntax-table)
  (modify-syntax-entry ?_  "w     " muttrc-mode-syntax-table)
  (modify-syntax-entry ?-  "w     " muttrc-mode-syntax-table)
  )

;;; ------------------------------------------------------------
;;; The mode function itself.
;;; ------------------------------------------------------------

;;;###autoload
(defun muttrc-mode ()
  "Major mode for editing Muttrc files.
This function ends by invoking the function(s) `muttrc-mode-hook'.

\\{muttrc-mode-map}
"

  (interactive)
  (kill-all-local-variables)

  ;; Font lock.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'('muttrc-font-lock-keywords
	  nil nil nil nil
	  (font-lock-syntactic-keywords . (("'[^'\n]*'" 0 "\"")))))

  ;; Comment stuff.
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+[ \t]*")

  ;; become the current major mode
  (setq major-mode 'muttrc-mode)
  (setq mode-name "Muttrc")

  ;; Activate keymap and syntax table.
  (use-local-map muttrc-mode-map)
  (set-syntax-table muttrc-mode-syntax-table)

  (run-hooks 'muttrc-mode-hook))

 

;;; ------------------------------------------------------------
;;; Other functions
;;; ------------------------------------------------------------

(defun muttrc-perform-nonreg-test ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^# Begin\\s-+\\(.*\\)$" nil t)
      (let ((test-name (match-string-no-properties 1))
	    (expr ""))
	(catch 'loop
	  (while t
	    (or (= (forward-line 1) 0)
		(throw 'loop t))
	    (if (looking-at (format "^# End\\s-+%s\\s-*"
				    (regexp-quote test-name)))
		(throw 'loop t))
	    (if (looking-at "^# End\\s-+\\(.*\\)$")
		(error "Found end of %s before %s"
		       (match-string-no-properties 1) test-name))
	    (if (looking-at "^[^#]")
		(error "End of %s not found" test-name))
	    (if (looking-at "^#\\s-*\\(.*\\)$")
		(setq expr (concat expr (match-string-no-properties 1))))))
	(if (eval (read expr))
	    (message "Passed: %s" test-name)
	  (error "Failed: %s" test-name))))))

(defun muttrc-quote-string (s)
  "Add a backslash on quotes and surround by quotes if needed."
  (save-match-data
    (cond ((or (not s) (equal s "")) "''")
	  ((string-match "^[^']*\\s-[^']*$" s) (format "'%s'" s))
	  ((string-match "\\s-" s)
	   (concat "\""
		   (mapconcat (lambda (c)
				(if (eq c ?\") "\\\""
				  (char-to-string c)))
			      s "")
		   "\""))
	  (t s))))

(defun muttrc-prompt-string (prompt-base &optional default)
  (if default
      (format "%s [%s]: " prompt-base default)
    (format "%s: " prompt-base)))

(defun muttrc-token-around-point (alist &optional strip-fun)
  (let ((word (and (functionp 'thing-at-point)
		   (funcall (or strip-fun 'identity)
			    (funcall 'thing-at-point 'word)))))
    (if (and word (assoc word alist))
	word)))

(defun muttrc-assignement (varname modifier &optional value)
  (concat (format "%s%s" (or modifier "") varname)
	  (if (stringp value)
	      (format "=%s"
		      (muttrc-quote-string value))
	    "")))

(defun muttrc-split-next-set-line ()
  "Returns the current line splitted into tokens. The result is a list
of tokens like:
\((CMD START END) ((VAR1 MODIFIER1 ASSIGNMENT1 START END) ... REST)).
Last element REST is one string that is the rest of the line."
  (if (re-search-forward
       "^\\s-*\\(set\\|unset\\|toggle\\|reset\\)\\s-+" nil t)
      (let ((line (list (list (match-string-no-properties 1)
			      (match-beginning 1)
			      (match-end 1))))
	    (limit (save-excursion
		     (end-of-line)
		     (point))))
	(catch 'done
	  (while (< (point) limit)
	    (or (looking-at
		 (format "\\<\\(inv\\|no\\)?\\([a-z][a-z_]*\\)\\>"))
		(throw 'done t))
	    (let ((modifier (match-string-no-properties 1))
		  (varname (match-string-no-properties 2))
		  (assignment nil))
	      (goto-char (match-end 0))
	      (skip-syntax-forward "-" limit)
	      (if (or (looking-at		; Set without quote
		       "=\\s-*\\([^'\" \t\n#]+\\)")
		      (looking-at		; Set with double quote (")
		       "=\\s-*\"\\(\\([^\"\\]\\|\\\\.\\)*\\)\"")
		      (looking-at		; Set with single quote (')
		       "=\\s-*'\\([^']*\\)'"))
		  (let ((type (let ((desc (assoc varname
						 muttrc-variables-alist)))
				(if desc (cadr desc)))))
		    (if type
			(and (eq type 'boolean)
			     (message "%s: can't assign a boolean" varname))
		      (message "%s: unknown Muttrc variable"
			       varname))
		    (setq assignment (match-string-no-properties 1))
		    (goto-char (match-end 0))))
	      (nconc line (list (list varname modifier
				      assignment
				      (match-beginning 0)
				      (match-end 0))))
	      (skip-syntax-forward "-" limit))))
	(skip-syntax-backward "-")
	(if (looking-at ".+$")
	    (nconc line (list (list (match-string-no-properties 0)))))
	(end-of-line)
	line)))

(defun muttrc-splice-assignment (line varname)
  "Returns a list where assignements for VARNAME are separated from
assignment for other variables."
  (let ((l (cdr line))
	(in '())
	(out '()))
    (while (and l (consp (car l)))
      (let ((arg (car l)))
	(if (string= (car arg) varname)
	    (setq in (append in (list arg)))
	  (setq out (append out (list arg)))))
      (setq l (cdr l)))
    (list in out)))

(defun muttrc-new-value (cmd varname type modifier value default)
  (if (eq type 'boolean)
      (cond ((string= cmd "set")
	     (cond ((null modifier) t)
		   ((string= modifier "no") nil)
		   ((string= modifier "inv") (not value))))
	    ((string= cmd "unset")
	     (cond ((null modifier) nil)
		   ((string= modifier "no") t)
		   ((string= modifier "inv") value)))
	    ((string= cmd "toggle") (not value))
	    ((string= cmd "reset")
	     (cond ((null modifier) default)
		   ((string= modifier "no") (not default))
		   ((string= modifier "inv") (not default)))))
      (cond ((string= cmd "set") value)
	    ((string= cmd "unset") default)
	    ((string= cmd "toggle")
	     (error "%s: can't toggle non boolean" varname))
	    ((string= cmd "reset") default))))

(defun muttrc-get-value-and-point (varname)
  "Fetch the value of VARIABLE from the current buffer. It returns a
cons (VALUE . POINT) where POINT is the beginning of the line defining
VARNAME."
  (save-excursion
    (let ((var-descriptor (assoc varname muttrc-variables-alist)))
      (or var-descriptor
	  (error "%s: unknown variable." varname))
      (goto-char (point-min))
      (let ((type (nth 0 (cdr var-descriptor)))
	    (default (nth 1 (cdr var-descriptor)))
	    (pos nil))
	(let ((value default))
	  ;; We search all the definitions in the buffer because some
	  ;; users may use toggle or set inv...
	  (catch 'done
	    (while t
	      (let ((line (muttrc-split-next-set-line)))
		(or line (throw 'done t))
		(let ((cmd (caar line))
		      (assignments
		       (car (muttrc-splice-assignment line varname))))
		  (if assignments
		      (setq pos (save-excursion
				  (beginning-of-line)
				  (point))))
		  (while assignments
		    (let ((modifier (nth 1 (car assignments)))
			  (new-value (nth 2 (car assignments))))
		      (setq value
			    (muttrc-new-value cmd varname type modifier
					      (or new-value value)
					      default)))
		    (setq assignments (cdr assignments)))))))
	  (cons value pos))))))

(defun muttrc-get-value (varname)
  "Fetch the value of VARIABLE from the current buffer."
  (let ((value (muttrc-get-value-and-point varname)))
    (and value (car value))))

;;; ------------------------------------------------------------
;;; Viewing manual
;;; ------------------------------------------------------------

(defvar muttrc-manual-buffer-name "*Mutt Manual*")

(defun muttrc-find-manual-file-no-select ()
  "Convert overstriking and underlining to the correct fonts in a
file. The buffer does not visit the file."
  (interactive)
  (or (file-readable-p muttrc-manual-path)
      (error "%s: file not found" muttrc-manual-path))
  (let ((buf (get-buffer-create muttrc-manual-buffer-name)))
    (save-excursion
      (set-buffer buf)
      (if (not buffer-read-only)
	  (let ((insert-contents-fun
		 (condition-case nil
		     (and (require 'jka-compr)
			  'jka-compr-insert-file-contents)
		   (error 'insert-file-contents))))
	    (funcall insert-contents-fun muttrc-manual-path nil nil nil t)
	    (buffer-disable-undo buf)
	    (Man-fontify-manpage)
	    (set-buffer-modified-p nil)
	    (toggle-read-only)
	    (goto-char (point-min))))
      buf)))

(defun muttrc-find-manual-file ()
  "Convert overstriking and underlining to the correct fonts in a
file. The buffer does not visit the file."
  (interactive)
  (switch-to-buffer-other-window
   (muttrc-find-manual-file-no-select) t))

(defun muttrc-search-command-help-forward (command)
  (when (re-search-forward
	 (format "^[ \t]*Usage:\\s-*\\(\\[un\\]\\)?%s" command)
	 nil t)
    (goto-char (match-beginning 0))
    (forward-line -2)
    (point)))

(defun muttrc-search-variable-help-forward (command)
  (when (and (re-search-forward
	      (format "^[ \t]*%s\\.?\\s-*%s\\s-*$"
		      "\\([1-9][0-9.]*\\)"
		      (regexp-quote variable))
	      nil t)
	     (re-search-forward
	      (format "^[ \t]*%s\\.?\\s-*%s\\s-*$"
		      "\\([1-9][0-9.]*\\)"
		      (regexp-quote variable))
	      nil t)	     
	     (re-search-forward
	      (format "^[ \t]*%s\\.?\\s-*%s\\s-*$"
		      (regexp-quote (match-string-no-properties 1))
		      (regexp-quote variable))
	      nil t))
    (goto-char (match-beginning 0))
    (point)))

(defun muttrc-find-help (search-fun topic)
  "Find an help topic in the manual and display it. Returns the manual
buffer."
  (let ((buf (muttrc-find-manual-file-no-select)))
    (let ((win (get-buffer-window buf))
	  help-start)
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(or (funcall search-fun topic)
	    (error "%s: entry not found in Mutt manual." command))
	(setq help-start (point))
	(unless (get-buffer-window buf)
	  (switch-to-buffer-other-window buf t))
	(set-window-start win help-start)))
    buf))

(defun muttrc-find-command-help (&optional command)
  (interactive
   (let ((word (muttrc-token-around-point muttrc-command-alist)))
     (list (muttrc-get-from-list "Command" word 'muttrc-command-alist t))))
  (muttrc-find-help 'muttrc-search-command-help-forward
		    (if (string-match "^un\\(.*\\)$" command)
			(match-string-no-properties 1 command)
		      command)))

(defun muttrc-find-variable-help (&optional variable)
  (interactive
   (list
    (let ((word (muttrc-token-around-point
		 muttrc-variables-alist
		 (function
		  (lambda (word)
		    (if (and word
			     (string-match "^\\(no\\|inv\\)\\(.*\\)$" word))
			(match-string-no-properties 2 word)
		      word))))))
      (muttrc-get-from-list "Variable" word 'muttrc-variables-alist))))
  (muttrc-find-help 'muttrc-search-variable-help-forward variable))

(defun muttrc-bury-manual-buffer ()
  (let ((buf (get-buffer muttrc-manual-buffer-name)))
    (if buf (bury-buffer buf))))

;;; ------------------------------------------------------------
;;; Argument handlers
;;; ------------------------------------------------------------

(defun muttrc-call-arg-handler (key default &optional prompt)
  "Call the function that properly prompts for an argument type."
  (let ((handler-args (assoc key muttrc-arg-handler-alist)))
    (or handler-args
	(error "%s: unknown argument type." (symbol-name key)))
    (let ((cmd (nth 0 (cdr handler-args)))
	  (default-prompt (nth 1 (cdr handler-args)))
	  (args (cdr (cddr handler-args))))
      (apply cmd (or prompt default-prompt) default args))))

(defun muttrc-get-boolean (prompt &optional default)
  "Prompt for a boolean."
  (y-or-n-p (format "%s? " prompt)))

(defun muttrc-get-number (prompt default)
  "Prompt for a string and return DEFAULT if the string is empty"
  (or (read-from-minibuffer (muttrc-prompt-string prompt default))
      default))

(defun muttrc-get-string (prompt default)
  "Prompt for a string and return DEFAULT if the string is empty"
  (let ((s (read-from-minibuffer (muttrc-prompt-string prompt default))))
    (if (> (length s) 0) s default)))

(defun muttrc-get-word (prompt default)
  "Prompt for a word and return DEFAULT if it is empty"
  (let ((s (read-from-minibuffer (muttrc-prompt-string prompt default))))
    (or (string-match "^\\w*$" s)
	(error "%s: invalid entry, expecting a word" s))
    (if (> (length s) 0) s default)))

(defun muttrc-get-from-list (prompt default list &optional require-match)
  "Prompt for a string from list and return DEFAULT if the string is empty"
  (let ((s (completing-read (muttrc-prompt-string prompt default)
			    (symbol-value list)
			    nil require-match)))
    (if (> (length s) 0) s default)))

(defun muttrc-get-path (prompt default)
  "Prompt for a path and return DEFAULT if the string is empty. The
muttrc folder prefix is replaced by MUTTRC-FOLDER-ABBREV."
  (let* ((folder (muttrc-get-value "folder"))
	 (path (read-file-name (muttrc-prompt-string prompt default)
			       folder folder)))
    (let ((compacted-path
	   (if (string-match (format "^%s/?\\(.*\\)$" (regexp-quote folder))
			     path)
	       (format "%s%s"
		       (char-to-string muttrc-folder-abbrev)
		       (match-string-no-properties 1 path))
	     path)))
      (if (not (string= compacted-path
			(char-to-string muttrc-folder-abbrev)))
	  compacted-path
	default))))

(defun muttrc-get-assignment (&optional prompt default
					with-value-p)
  (let ((varname (completing-read (muttrc-prompt-string prompt default)
				  muttrc-variables-alist)))
    (if (assoc varname muttrc-variables-alist)
	(let* ((type (cadr (assoc varname muttrc-variables-alist)))
	       (default (car-safe (muttrc-get-value-and-point varname)))
	       (value (if with-value-p
			(muttrc-call-arg-handler type default "Value"))))
	  (if with-value-p
	      (muttrc-assignement varname
				  (and (eq type 'boolean)
				       (not value)
				       "no")
				  value)
	    varname))
      default)))

;;; ------------------------------------------------------------
;;; Commands insertion
;;; ------------------------------------------------------------

(defun muttrc-get-command (&optional prompt default)
  "Prompts the usr for a command to enter and asks for all the arguments."
  (let* ((cmd (muttrc-get-from-list "Command" nil 'muttrc-command-alist t))
	 (cmd-descriptor (cdr (assoc cmd muttrc-command-alist)))
	 (arg-list-type (nth 0 cmd-descriptor))
	 (repeat-p (nth 1 cmd-descriptor))
	 (optional-p (nth 2 cmd-descriptor))
	 (arg-list-value (list cmd)))
    (save-window-excursion
      (if (and muttrc-display-help)
	  (save-excursion
	    (muttrc-find-command-help cmd)))
      (while arg-list-type
	(let* ((arg-type (caar arg-list-type))
	       (arg (apply 'muttrc-call-arg-handler
			   (append (list arg-type nil)
				   (cdar arg-list-type)))))
	  (if arg
	      (progn
		(nconc arg-list-value
		       (list (if (eq arg-type 'assignment)
				 arg ; assignment are quoted by handler
			       (muttrc-quote-string arg))))
		(if (and repeat-p
			 (null (cdr arg-list-type)))
		    (setq optional-p t)
		  (setq arg-list-type (cdr arg-list-type))))
	    (if (and (null (cdr arg-list-type))
		     optional-p)
		(setq arg-list-type nil)
	      (error "Argument required"))))))
    (muttrc-bury-manual-buffer)
    (mapconcat 'identity arg-list-value " ")))

(defun muttrc-get-statement (&optional prompt default)
  (let ((muttrc-command-alist muttrc-statement-alist))
    (muttrc-get-command prompt default)))

(defun muttrc-insert-command ()
  "Insert a muttrc command on the current line."
  (interactive)
  (let ((cmd-line (muttrc-get-command)))
    (beginning-of-line)
    (or (eolp) (forward-line 1))
    (insert cmd-line)
    (newline)))

;;; ------------------------------------------------------------
;;; Setting variables
;;; ------------------------------------------------------------

(defun muttrc-update-current-line (varname type &optional value)
  "Rewrites the current line by setting VARNAME to VALUE. If the
statement is not \"set\", the variable is removed. In set statement,
it is removed if the value is NIL and the variable is not a boolean.
The function returns t is the variable is really assigned in the line."
  (let* ((line (muttrc-split-next-set-line))
	 (cmd (caar line))
	 (kill-whole-line t)
	 (args "")
	 (set-p nil))
    (beginning-of-line)
    (kill-line)
    (let ((l (cdr line)))
      (while l
	(let ((elt (car l)))
	  (if (consp elt)
	      (let ((this-var (nth 0 elt))
		    (this-modifier (nth 1 elt))
		    (this-value (nth 2 elt)))
		(let ((assignement
		       (if (string= this-var varname)
			   (when (string= cmd "set")
			     (setq set-p t)
			     (cond ((eq type 'boolean)
				    (muttrc-assignement varname
							(if (not value) "no")
							value))
				   (value
				    (muttrc-assignement varname nil value))
				   (t (setq set-p nil))))
			 (muttrc-assignement this-var
					     this-modifier
					     this-value))))
		  (if assignement
		      (setq args (concat args " " assignement)))))
	    (setq args (concat args elt))))
	(setq l (cdr l))))
    (when (not (string= args ""))
      (insert cmd)
      (insert args)
      (newline))
    (backward-char 1)
    set-p))

(defun muttrc-update-variable (varname type value pos)
  (catch 'done
    (when pos
      (goto-char pos)
      (if (muttrc-update-current-line varname type value)
	  (throw 'done t)))
    (end-of-line)
    (let ((cr-after-p (bolp))
	  (cmd (if (or value (eq type 'boolean)) "set" "unset"))
	  (modifier (if (and (not value) (eq type 'boolean)) "no")))
      (or cr-after-p (newline))
      (insert cmd " "
	      (muttrc-assignement varname modifier value))
      (if cr-after-p (newline))))
  t)

(defun muttrc-set-variable (&optional varname type value pos)
  (interactive
   (let* ((varname (muttrc-get-from-list "Variable" nil
					 'muttrc-variables-alist t))
	  (type (cadr (assoc varname muttrc-variables-alist)))
	  (default (muttrc-get-value-and-point varname)))
     (list varname type
	   (save-window-excursion
	     (if muttrc-display-help
		 (save-excursion
		   (muttrc-find-variable-help varname)))
	     (muttrc-call-arg-handler type (car default)))
	   (cdr default))))
  (muttrc-bury-manual-buffer)
  (muttrc-update-variable varname type value pos))

(defun muttrc-unset-variable (&optional varname type pos)
  (interactive
   (let* ((varname (muttrc-get-from-list "Variable" nil
					 'muttrc-variables-alist t))
	  (type (cadr (assoc varname muttrc-variables-alist)))
	  (default (muttrc-get-value-and-point varname)))
     (list varname type (cdr default))))
  (muttrc-update-variable varname type nil pos))

(defun muttrc-find-variable-in-buffer (&optional varname)
  (interactive
   (list (muttrc-get-from-list "Variable" nil
			       'muttrc-variables-alist t)))
  (let* ((var-info (muttrc-get-value-and-point varname))
	 (value (car var-info))
	 (pos (cdr-safe var-info)))
    (if pos
	(goto-char pos)
      (progn
	(message "%s: variable not set (default: %s)" varname value)))))

;;; ------------------------------------------------------------
;;; Almost the end
;;; ------------------------------------------------------------

(provide 'muttrc-mode)

;;; muttrc-mode.el ends here
