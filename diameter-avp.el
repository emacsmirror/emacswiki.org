;;; diameter-avp.el --- Compute diameter-avp in hexadecimal-form
;;
;; Filename: diameter-avp.el
;; -----------------------------------------------------------------------------
;; $Author: ffrances $
;; $Date: $
;; $Revision: $
;; $Id: $
;; -----------------------------------------------------------------------------
;;
;; This is not part of Gnu emacs.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;
;; -----------------------------------------------------------------------------
;;
;; Utility to compute diameter-avp in hexadecimal-form
;;
;; diameter-avp-encode             \ Encode a single avp
;; diameter-avp-encode-insert      / (that can be a grouped avp)
;;
;; diameter-avp-encode-list        \  Encode a list of avp
;; diameter-avp-encode-list-insert /
;;
;; Limitation(s):
;;   32 bits number are not fully supported by lisp (25 bits + tags),
;;           so it is not possible to encode correctly a value like
;;           (-1)       0xFFFFFFFF
;;           wich becomes 1FFFFFFF
;;
;;   64 bits number are not supported.
;;
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Requirement
;; -----------------------------------------------------------------------------
(require 'hex-util) ;; used to convert string to hexadecimal values

;; -----------------------------------------------------------------------------
;; Constant
;; -----------------------------------------------------------------------------
(defconst avp-code           0
  "Position of avp-code in a diameter-avp")

(defconst avp-vendor-flag    1
  "Position of avp-vendor-flag in a diameter-avp")

(defconst avp-mandatory-flag 2
  "Position of avp-mandatory-flag in a diameter-avp")

(defconst avp-protected-flag 3
  "Position of avp-protected-flag in a diameter-avp")

(defconst avp-vendor-id      4
  "Position of avp-vendor-id in a diameter-avp")

(defconst avp-data           5
  "Position of avp-data in a diameter-avp")

;; -----------------------------------------------------------------------------
;; Basic utility function
;; -----------------------------------------------------------------------------

;; padded addition
(defun diameter-avp-sum-list (list)
  "Return the addition of all element in list,

Each element is rounded up to the next multiple of 4 before adding
to respect 32 bit padding while encoding an avp.

this function is useed by `diameter-avp-compute-len'

rouding up is due to the fact that RFC-3588 defined diameter protocol
as a 32 bit padded protocol
"
  (let ((sum 0))
    (setq sum
          (cond
           ((eq nil list)
            (message "diameter-avp-sum-list (nil): (%d)" 0)
            0)
           ((numberp list)
            (message "diameter-avp-sum-list (elem) (padd-elem): (%d) (%d)"
                     list
                     (* (/ (+ list 3) 4) 4))
            (* (/ (+ list 3) 4) 4)) ;; to pad 4 bytes
           ((listp list) ;; List do the sum of elements
            (message "diameter-avp-sum-list (listp) (padded car): (%s) (%d)"
                     list
                     (* (/ (+ (car list) 3) 4) 4)
                     )
            (+ (* (/ (+ (car list) 3) 4) 4)
               (diameter-avp-sum-list (cdr list))))
           (t (message "diameter-avp-sum-list (t): Error not a number nor a list"))
           ));; cond
    (message "diameter-avp-sum-list (sum): %d" sum)
    sum)
  );;defun

;; concatenation of list oof string
(defun diameter-avp-concat-list (list)
  "Return the concatenation of all element in a list,

This function is used by `diameter-avp-encode-list' to encode grouped avp
"
  (let ((result ""))
    (setq result
          (cond
            ((eq nil list)
             "")
            ((listp list) ;; List do the concat of elements
             (concat (car list) (diameter-avp-concat-list (cdr list))))
            );; cond
          )
    );; let
  );;defun

;; -----------------------------------------------------------------------------
;; Basic encoding function
;; -----------------------------------------------------------------------------

;; encode avp flags
(defun diameter-avp-encode-flag (diameter-avp-vendor-flag
                      diameter-avp-mandatory-flag

                      diameter-avp-protected-flag)
  "Evaluate an diameter-avp flag according to
vendor-flag, mandatory-flag and protected flag

this function is used by `diameter-avp-encode-standard-header'
                         `diameter-avp-encode-vendor-header'

RFC-3588 11.1.2. AVP Flags

There are 8 bits in the AVP Flags field of the AVP header, defined in
Section 4.  This document assigns bit 0 'V'endor Specific, bit 1
'M'andatory and bit 2 'P'rotected.  The remaining bits should
 only be assigned via a Standards Action [IANA].
"
  (interactive "ndiameter-avp-vendor-flag:
ndiameter-avp-mandatory-flag:
ndiameter-avp-protected-flag:")
  (+
   (if (/= diameter-avp-vendor-flag    0) ;; assume t if > 0
       128  ; 1000 0000
     0)     ; 0000 0000
   (if (/= diameter-avp-mandatory-flag 0) ;; assume t if > 0
       64   ; 0100 0000
     0)     ; 0000 0000
   (if (/= diameter-avp-protected-flag 0) ;; assume t if > 0
       32   ; 0010 0000
     0)     ; 0000 0000
   )
  )

(defun diameter-avp-encode-standard-header(diameter-avp-code
                                  ;;diameter-avp-vendor-flag
                                  diameter-avp-mandatory-flag
                                  diameter-avp-protected-flag
                                  diameter-avp-len)
  "Encode a standard diameter-avp header,

this function assume that this avp is not vendor specific so it
doesn't require vendor-specific information.

this function is used by `diameter-avp-encode-header'

this function use `diameter-avp-encode-flag'

RFC-3588 4.1.  AVP Header

   The fields in the AVP header MUST be sent in network byte order.  The
   format of the header is:

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                           AVP Code                            |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |V M P r r r r r|                  AVP Length                   |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                        Vendor-ID (opt)                        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |    Data ...
   +-+-+-+-+-+-+-+-+

   AVP Code
      The AVP Code, combined with the Vendor-Id field, identifies the
      attribute uniquely.  AVP numbers 1 through 255 are reserved for
      backward compatibility with RADIUS, without setting the Vendor-Id
      field.  AVP numbers 256 and above are used for Diameter, which are
      allocated by IANA (see Section 11.1).

   AVP Flags
      The AVP Flags field informs the receiver how each attribute must
      be handled.  The 'r' (reserved) bits are unused and SHOULD be set
      to 0.  Note that subsequent Diameter applications MAY define
      additional bits within the AVP Header, and an unrecognized bit
      SHOULD be considered an error.  The 'P' bit indicates the need for
      encryption for end-to-end security.

      The 'M' Bit, known as the Mandatory bit, indicates whether support
      of the AVP is required.  If an AVP with the 'M' bit set is
      received by a Diameter client, server, proxy, or translation agent
      and either the AVP or its value is unrecognized, the message MUST
      be rejected.  Diameter Relay and redirect agents MUST NOT reject
      messages with unrecognized AVPs.

      The 'M' bit MUST be set according to the rules defined for the AVP
      containing it.  In order to preserve interoperability, a Diameter
      implementation MUST be able to exclude from a Diameter message any
      Mandatory AVP which is neither defined in the base Diameter
      protocol nor in any of the Diameter Application specifications
      governing the message in which it appears.  It MAY do this in one
      of the following ways:

      1) If a message is rejected because it contains a Mandatory AVP
         which is neither defined in the base Diameter standard nor in
         any of the Diameter Application specifications governing the
         message in which it appears, the implementation may resend the
         message without the AVP, possibly inserting additional standard
         AVPs instead.

      2) A configuration option may be provided on a system wide, per
         peer, or per realm basis that would allow/prevent particular
         Mandatory AVPs to be sent.  Thus an administrator could change
         the configuration to avoid interoperability problems.

      Diameter implementations are required to support all Mandatory
      AVPs which are allowed by the message's formal syntax and defined
      either in the base Diameter standard or in one of the Diameter
      Application specifications governing the message.

      AVPs with the 'M' bit cleared are informational only and a
      receiver that receives a message with such an AVP that is not
      supported, or whose value is not supported, MAY simply ignore the
      AVP.

      The 'V' bit, known as the Vendor-Specific bit, indicates whether
      the optional Vendor-ID field is present in the AVP header.  When
      set the AVP Code belongs to the specific vendor code address
      space.

      Unless otherwise noted, AVPs will have the following default AVP
      Flags field settings:

         The 'M' bit MUST be set.  The 'V' bit MUST NOT be set.

   AVP Length
      The AVP Length field is three octets, and indicates the number of
      octets in this AVP including the AVP Code, AVP Length, AVP Flags,
      Vendor-ID field (if present) and the AVP data.  If a message is
      received with an invalid attribute length, the message SHOULD be
      rejected.
"
  (interactive "ndiameter-avpCode:
ndiameter-avp-mandatory-flag:
ndiameter-avp-protected-flag:
ndiameter-avp-len:")
  (format "%08.8X%02.2X%06.6X"
          diameter-avp-code
          (diameter-avp-encode-flag 0
                         diameter-avp-mandatory-flag
                         diameter-avp-protected-flag)
          diameter-avp-len)
  )

(defun diameter-avp-encode-vendor-header(diameter-avp-code
                                ;; diameter-avp-vendor-flag
                                diameter-avp-mandatory-flag
                                diameter-avp-protected-flag
                                diameter-avp-len
                                diameter-avp-vendor)
  "Encode a vendor specific diameter-avp header

this function assume that this avp is a vendor specific specific avp
so it require vendor-specific information.

this function is used by `diameter-avp-encode-header'

this function use `diameter-avp-encode-flag'

refer to `diameter-avp-encode-standard-header' for non optional information
in header

this function create an avp-header with optional information.

RFC-3588 4.1.1.  Optional Header Elements

   The AVP Header contains one optional field.  This field is only
   present if the respective bit-flag is enabled.

   Vendor-ID
      The Vendor-ID field is present if the 'V' bit is set in the AVP
      Flags field.  The optional four-octet Vendor-ID field contains the
      IANA assigned \"SMI Network Management Private Enterprise Codes\"
      [ASSIGNNO] value, encoded in network byte order.  Any vendor
      wishing to implement a vendor-specific Diameter AVP MUST use their
      own Vendor-ID along with their privately managed AVP address
      space, guaranteeing that they will not collide with any other
      vendor's vendor-specific AVP(s), nor with future IETF
      applications.

      A vendor ID value of zero (0) corresponds to the IETF adopted AVP
      values, as managed by the IANA.  Since the absence of the vendor
      ID field implies that the AVP in question is not vendor specific,
      implementations MUST NOT use the zero (0) vendor ID.
"
  (interactive "ndiameter-avpCode:
ndiameter-avp-mandatory-flag:
ndiameter-avp-protected-flag:
ndiameter-avp-len:
ndiameter-avp-vendor")
  (format "%08.8X%02.2X%06.6X%08.8X"
           diameter-avp-code
           (diameter-avp-encode-flag 1
                          diameter-avp-mandatory-flag
                          diameter-avp-protected-flag)
           diameter-avp-len
           diameter-avp-vendor)
  )


(defun diameter-avp-encode-header
  (diameter-avp-code
   diameter-avp-vendor-flag
   diameter-avp-mandatory-flag
   diameter-avp-protected-flag
   diameter-avp-len
   diameter-avp-vendor
   )
  "Encode a diameter diameter-avp header,

this function use  `diameter-avp-encode-vendor-header'
                   `diameter-avp-encode-standard-header'

this function is used by `diameter-avp-encode'

RFC-3588 4.1. AVP Header

   The fields in the AVP header MUST be sent in network byte order.  The
   format of the header is:

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                           AVP Code                            |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |V M P r r r r r|                  AVP Length                   |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                        Vendor-ID (opt)                        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |    Data ...
   +-+-+-+-+-+-+-+-+

   AVP Code
      The AVP Code, combined with the Vendor-Id field, identifies the
      attribute uniquely.  AVP numbers 1 through 255 are reserved for
      backward compatibility with RADIUS, without setting the Vendor-Id
      field.  AVP numbers 256 and above are used for Diameter, which are
      allocated by IANA (see Section 11.1).

   AVP Flags
      The AVP Flags field informs the receiver how each attribute must
      be handled.  The 'r' (reserved) bits are unused and SHOULD be set
      to 0.  Note that subsequent Diameter applications MAY define
      additional bits within the AVP Header, and an unrecognized bit
      SHOULD be considered an error.  The 'P' bit indicates the need for
      encryption for end-to-end security.

      The 'M' Bit, known as the Mandatory bit, indicates whether support
      of the AVP is required.  If an AVP with the 'M' bit set is
      received by a Diameter client, server, proxy, or translation agent
      and either the AVP or its value is unrecognized, the message MUST
      be rejected.  Diameter Relay and redirect agents MUST NOT reject
      messages with unrecognized AVPs.

      The 'M' bit MUST be set according to the rules defined for the AVP
      containing it.  In order to preserve interoperability, a Diameter
      implementation MUST be able to exclude from a Diameter message any
      Mandatory AVP which is neither defined in the base Diameter
      protocol nor in any of the Diameter Application specifications
      governing the message in which it appears.  It MAY do this in one
      of the following ways:

      1) If a message is rejected because it contains a Mandatory AVP
         which is neither defined in the base Diameter standard nor in
         any of the Diameter Application specifications governing the
         message in which it appears, the implementation may resend the
         message without the AVP, possibly inserting additional standard
         AVPs instead.

      2) A configuration option may be provided on a system wide, per
         peer, or per realm basis that would allow/prevent particular
         Mandatory AVPs to be sent.  Thus an administrator could change
         the configuration to avoid interoperability problems.

      Diameter implementations are required to support all Mandatory
      AVPs which are allowed by the message's formal syntax and defined
      either in the base Diameter standard or in one of the Diameter
      Application specifications governing the message.

      AVPs with the 'M' bit cleared are informational only and a
      receiver that receives a message with such an AVP that is not
      supported, or whose value is not supported, MAY simply ignore the
      AVP.

      The 'V' bit, known as the Vendor-Specific bit, indicates whether
      the optional Vendor-ID field is present in the AVP header.  When
      set the AVP Code belongs to the specific vendor code address
      space.

      Unless otherwise noted, AVPs will have the following default AVP
      Flags field settings:

         The 'M' bit MUST be set.  The 'V' bit MUST NOT be set.

   AVP Length
      The AVP Length field is three octets, and indicates the number of
      octets in this AVP including the AVP Code, AVP Length, AVP Flags,
      Vendor-ID field (if present) and the AVP data.  If a message is
      received with an invalid attribute length, the message SHOULD be
      rejected.

4.1.1.  Optional Header Elements

   The AVP Header contains one optional field.  This field is only
   present if the respective bit-flag is enabled.

   Vendor-ID
      The Vendor-ID field is present if the 'V' bit is set in the AVP
      Flags field.  The optional four-octet Vendor-ID field contains the
      IANA assigned \"SMI Network Management Private Enterprise Codes\"
      [ASSIGNNO] value, encoded in network byte order.  Any vendor
      wishing to implement a vendor-specific Diameter AVP MUST use their
      own Vendor-ID along with their privately managed AVP address
      space, guaranteeing that they will not collide with any other
      vendor's vendor-specific AVP(s), nor with future IETF
      applications.

      A vendor ID value of zero (0) corresponds to the IETF adopted AVP
      values, as managed by the IANA.  Since the absence of the vendor
      ID field implies that the AVP in question is not vendor specific,
      implementations MUST NOT use the zero (0) vendor ID.

"
  (interactive "ndiameter-avp-code:
ndiameter-avp-vendor-flag:
ndiameter-avp-mandatory-flag:
ndiameter-avp-protected-flag:
ndiameter-avp-len:
ndiameter-avp-vendor:")
  (let
      (
       (encoded)
       )
    (setq encoded
          (if (and (/= diameter-avp-vendor-flag 0)
                   (/= diameter-avp-vendor 0))
              (diameter-avp-encode-vendor-header
               diameter-avp-code
               diameter-avp-mandatory-flag
               diameter-avp-protected-flag
               diameter-avp-len
               diameter-avp-vendor
               )
            (diameter-avp-encode-standard-header
             diameter-avp-code
             ;;   diameter-avp-vendor-flag
             diameter-avp-mandatory-flag
             diameter-avp-protected-flag
             diameter-avp-len
             ))
          )
    ))

(defun diameter-avp-compute-len (diameter-avp)
  "Compute length of a grouped diameter-avp

this function use `diameter-avp-sum-list'

this function is used by `diameter-avp-encode-list'
                         `diameter-avp-encode'

Limitation:
  * 64bits avp are not managed (float/integer)

RFC-3588 4.1.  AVP Header

 ...

  AVP Length
      The AVP Length field is three octets, and indicates the number of
      octets in this AVP including the AVP Code, AVP Length, AVP Flags,
      Vendor-ID field (if present) and the AVP data.  If a message is
      received with an invalid attribute length, the message SHOULD be
      rejected.

 ...

RFC-3588 4.2.  Basic AVP Data Formats

   The Data field is zero or more octets and contains information
   specific to the Attribute.  The format and length of the Data field
   is determined by the AVP Code and AVP Length fields.  The format of
   the Data field MUST be one of the following base data types or a data
   type derived from the base data types.  In the event that a new Basic
   AVP Data Format is needed, a new version of this RFC must be created.

...

   Grouped
      The Data field is specified as a sequence of AVPs.  Each of these
      AVPs follows - in the order in which they are specified -
      including their headers and padding.  The AVP Length field is set
      to 8 (12 if the 'V' bit is enabled) plus the total length of all
      included AVPs, including their headers and padding.  Thus the AVP
      length field of an AVP of type Grouped is always a multiple of 4.
"
  (interactive "xdiameter-avp:")
  (message "diameter-avp-compute-len (avp) (vendor-specific): (%s) (%s)"
           diameter-avp
           (if (and (/=  (nth avp-vendor-flag diameter-avp) 0)
                    (/= (nth avp-vendor-id diameter-avp) 0))
               "yes"
             "no"))
  (cond
   ((eq (length diameter-avp) 6)
    (+
     8 ;; diameter-version + avp-code + avp-flags + avp-lentgh
     ;;   1                + 3        + 1         + 3
     ;;   optional vendor-id
     ;;   4
     (if (and (/= (nth avp-vendor-flag diameter-avp) 0)
              (/= (nth avp-vendor-id diameter-avp) 0))
         4 ;; 32 bits for optional vendor id
       0)  ;; base protocol avp
     ;;   data 32 bits padded
     (cond
      ((stringp   (nth avp-data diameter-avp))
       (length    (nth avp-data diameter-avp)))
      ((numberp   (nth avp-data diameter-avp))
       4) ;; 32 bits integer or float
      ((listp                     (nth avp-data diameter-avp))
       (diameter-avp-sum-list (mapcar (lambda (diameter-grouped-avp)
                           (diameter-avp-compute-len  diameter-grouped-avp))
                         (nth avp-data diameter-avp))))
      (t
       0));; cond
     ))
   (nil
    (message "diameter-avp-compute-len (len): (%d) wrong list length"
             (length diameter-avp))
    0));;cond
  )

;; -----------------------------------------------------------------------------
;; Diameter AVP encoding function
;; -----------------------------------------------------------------------------

(defun diameter-avp-encode-list (diameter-avp-list)
  "this function return an hexadecimal string that represent encoded avp.

this function use `diameter-avp-encode-header'
                  `diameter-avp-compute-len'
                  `diameter-avp-encode'
                  `diameter-avp-concat-list'
"
  (interactive "xdiameter-avp-list:")
  ;; todo check content to see the type of the diameter-avp.
  ;; we can use predicates listp,integerp,stringp,floatp
  ;; execute 32bits padding for string
  (let ((encoded
         (cond ((listp diameter-avp-list)
                (message "diameter-avp-encode-list (not-encoded): (%s)"
                         diameter-avp-list)
                (concat
                        (diameter-avp-concat-list
                         (mapcar (lambda (diameter-grouped-avp-list)
                                   (diameter-avp-encode
                                    diameter-grouped-avp-list))
                                 diameter-avp-list))

                        ))
               ((numberp diameter-avp-list)
                (format "%08.8X" diameter-avp-list))  ;; return
               ((stringp diameter-avp-list)
                (format "%s"
                        (concat
                         (encode-hex-string diameter-avp-list)
                         (cond ((eq (% (length diameter-avp-list) 4) 0)
                                "")
                               ((eq (% (length diameter-avp-list) 4) 1)
                                "000000")
                               ((eq (% (length diameter-avp-list) 4) 2)
                                "0000")
                               ((eq (% (length diameter-avp-list) 4) 3)
                                "00")))))
               (t
                (format "nil")) ;; (t
               )))
    (cond ((listp diameter-avp-list)
           (message "diameter-avp-encode-list (encoded): (%s)" encoded)))
    encoded) ;; let
  );; defun

(defun diameter-avp-encode (diameter-avp)
  "this function return an hexadecimal string that represent encoded avp.

this function use `diameter-avp-encode-header'
                  `diameter-avp-compute-len'
                  `diameter-avp-encode-list'

Lisp construct of an diameter-avp is

diameter-avp-list ::= ({diameter-avp} {diameter-avp} {diameter-avp} ...)

diameter-avp ::= {header data}

header ::= code,                  4 bytes
           vendor-flag,       \
           mandatory-flag,     }- 1 byte
           protected-flag,    /
           vendor,                4 byte


data ::= string
         number
         diameter-avp

An encoded diameter-avp have the following representation:

encoded-diameter-avp ::=
   {code}               4 bytes
   {flags}{length}      1 byte + 3 bytes
   [vendor]             4 bytes (optional)
   [data]               x bytes


So to compute a complete encoding we have to calculate length of a diameter-avp.

Limitation (elisp):

  * 64 bits number are not managed (try string represenation of them).

  * Lisp integer representation:
     The range of values for integers is -8388608 to 8388607
     (24 bits; i.e., to on most machines, but is 25 or 26 bits on some systems.

    So it is not possible to encode an integer like
    0xFFFFFFFF = 4294967295 (unsigned) = -1 (singed)
"
  (interactive "xdiameter-avp:")
  (message "diameter-avp-encode (not-encoded): (%s)" diameter-avp)
  ;; Header is in car of diameter-avp
  (let ((encoded
         (cond ((eq (length diameter-avp) 6)
                (concat
                 (diameter-avp-encode-header
                  (nth avp-code diameter-avp)
                  (nth avp-vendor-flag diameter-avp)
                  (nth avp-mandatory-flag diameter-avp)
                  (nth avp-protected-flag diameter-avp)
                  (cond
                   ((stringp (nth avp-data diameter-avp))
                    (+ 8
                       (if (and (/=  (nth avp-vendor-flag diameter-avp) 0)
                                (/= (nth avp-vendor-id diameter-avp) 0))
                           4 ;; 32 bits for optional vendor id
                         0)  ;; base protocol avp
                       (length (nth avp-data diameter-avp))))
                   (t
                    ;; take padding account for string inside groupe
                    (diameter-avp-compute-len diameter-avp)))
                  (nth avp-vendor-id diameter-avp) ;; diameter-avp-vendor
                  )
                 (diameter-avp-encode-list (nth avp-data diameter-avp))
                 );; concat
                );; ((eq (length diameter-avp) 6)
               (t (message
                   "%s Wrong list size for a diameter avp get (%d) expected 5"
		   diameter-avp
                   (length diameter-avp)))
               );; cond
         ))
    (message "diameter-avp-encode (encoded): (%s)" encoded)
    encoded) ;; let
   );; defun


(defun diameter-avp-encode-insert (diameter-avp)
  "Encode a diameter-avp and insert string in current buffer

this function use `diameter-avp-encode'
"
   (interactive "xdiameter-avp:")
   (insert (diameter-avp-encode diameter-avp)))

(defun diameter-avp-encode-list-insert (diameter-avp)
  "Encode a list of  diameter-avp and insert string in current buffer

this function use `diameter-avp-encode-list'
"
   (interactive "xdiameter-avp:")
   (insert (diameter-avp-encode-list diameter-avp)))

(provide 'diameter-avp)
