;;; corba.el --- A Client Side CORBA Implementation for Emacs

;; Copyright (C) 1998 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Version: $Id: corba.el,v 1.23 2001/03/06 21:02:51 lenst Exp $
;; Keywords: 
;; Created: 1998-01-25 11:03:10

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to lenst@lysator.liu.se) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
 
;; LCD Archive Entry:
;; corba|Lennart Staflin|lenst@lysator.liu.se|
;; A Client Side CORBA Implementation for Emacs|
;; $Date: 2001/03/06 21:02:51 $|$Revision: 1.23 $||

;;; Commentary:

;; Provides an implementation of CORBA Dynamic Invocation interface
;; using the IIOP protocol.

;;; TODO:

;; Marshaling code for: longlong, ulonglong, floats, fixed,
;; union, array, wchar, wstring, better handling of enum

;; How should overflow in long/ulong be handled?

;; The typeid in an IOR is optional, handle that case by asking the remote
;; object for the interface.

;; Separate the internal repository in interface repository and type
;; repository. In case corba-get-typecode is ever called with an interface id.

;; Server side:
;; probably need a helper program that handles the
;; sockets and multiplexes messages.

;; Generation of static stubs.

;; Saving the internal interface repository to a Lisp file for later
;; use without a Repository service.

;; Allow nil in a parameter to represent the CORBA NIL object?

 
;;; Code:

(provide 'corba)

(eval-when-compile (require 'cl))
(eval-when-compile (load "cl-extra"))   ; This seems to fix some strange autoloading
                                        ; problem.

(defvar corba-name-service "/tmp/NameService"
  "*Reference to the CORBA NameService.
This should be the name of a file where the name service IOR is stored
or the IOR.")

(defvar corba-interface-repository "/tmp/InterfaceRepository"
  "*Reference to the CORBA InterfaceRepository.
This should be the name of a file where the service IOR is stored
or the IOR.")

(defvar corba-principal ""
  "*Octet sequence used for the principal field in the GIOP message.
Used by ORBit for its cookie.")

(defvar corba-explicit-any nil
  "*If non-nil, an explicit any struct will be returned for any result.
If nil, the actual value will be returned.")

 
;;;; Exceptions

(put 'corba-system-exception 'error-conditions
     '(corba-system-exception corba-exception error))
(put 'corba-system-exception 'error-message "CORBA System Exception")

(put 'corba-user-exception 'error-conditions
     '(corba-user-exception corba-exception error))
(put 'corba-user-exception 'error-message "CORBA User Exception")

 
;;;; Structures

;; Interface: corba-object-id ?
(defstruct corba-object 
  (id nil)
  (host nil)
  (port nil)
  (key nil)
  (profiles nil)
  (forward nil))

;; Interface:
(defstruct corba-any
  (typecode nil)
  (value nil))

(defstruct (corba-opdef (:type list))
  name
  inparams
  outparams
  raises)

(defstruct corba-interface
  id
  operations
  inherit)

 
;;;; TypeCodes

(defconst corba-tc-kind
  [
   tk_null tk_void tk_short tk_long tk_ushort tk_ulong
   tk_float tk_double tk_boolean tk_char
   tk_octet tk_any tk_TypeCode tk_Principal tk_objref
   tk_struct tk_union tk_enum tk_string
   tk_sequence tk_array tk_alias tk_except
   tk_longlong tk_ulonglong tk_longdouble
   tk_wchar tk_wstring tk_fixed
   ]
  "The symbols for the TCKind enum")

(eval-when (load eval)
  (loop for i from 0 below (length corba-tc-kind)
	do (setf (symbol-value (elt corba-tc-kind i)) i)))

(put 'tk_fixed 'tk-params '(tk_ushort tk_short))
(put 'tk_objref 'tk-params '(complex string string))
(put 'tk_struct 'tk-params '(complex string string
				     (sequence (anon-struct string tk_TypeCode))))
(put 'tk_union 'tk-params t)
(put 'tk_enum 'tk-params t)
(put 'tk_sequence 'tk-params '(complex tk_TypeCode tk_ulong))
(put 'tk_string 'tk-params '(tk_ulong))
(put 'tk_wstring 'tk-params '(tk_ulong))
(put 'tk_array 'tk-params t)
(put 'tk_alias 'tk-params '(complex string string tk_TypeCode))
(put 'tk_except 'tk-params '(complex string string
                             (sequence (anon-struct string tk_TypeCode))))


(defsubst make-corba-typecode (kind &optional params)
  (if params (cons kind params) kind))

(defsubst corba-typecode-kind (tc)
  (if (symbolp tc) tc (car tc)))

(defsubst corba-typecode-params (tc)
  (if (symbolp tc) nil (cdr tc)))

(defsubst corba-lispy-name (string)
  (cond ((symbolp string)
	 string)
	(t
         (setq string (copy-sequence string))
         (loop for c across-ref string
               if (eq c ?_) do (setf c ?-))
	 (intern string))))
 
;;;; Misc utilities

(defsubst corba-hex-to-int (ch)
  (cdr (assq ch '((?0 . 0)
		  (?1 . 1)
		  (?2 . 2)
		  (?3 . 3)
		  (?4 . 4)
		  (?5 . 5)
		  (?6 . 6)
		  (?7 . 7)
		  (?8 . 8)
		  (?9 . 9)
		  (?a . 10) (?A . 10)
		  (?b . 11) (?B . 11)
		  (?c . 12) (?C . 12)
		  (?d . 13) (?D . 13)
		  (?e . 14) (?E . 14)
		  (?f . 15) (?F . 15)))))
 
;;;; Work buffer managing

(defvar corba-work-buffer nil)

(defun corba-get-work-buffer ()
  (unless (and corba-work-buffer (buffer-live-p corba-work-buffer))
    (setq corba-work-buffer
          (generate-new-buffer " *CDR*"))
    (let ((ob (current-buffer)))
      (set-buffer corba-work-buffer)
      (make-local-variable 'corba-work-buffer)
      (setq corba-work-buffer nil)
      (setq buffer-undo-list t)
      (set-buffer ob)))
  corba-work-buffer)

(defun corba-set-work-buffer ()
  (set-buffer (corba-get-work-buffer))
  (erase-buffer))

(defmacro corba-in-work-buffer (&rest body)
  (let ((cb-var (gensym)))
      `(let ((,cb-var (current-buffer)))
         (unwind-protect
             (progn (corba-set-work-buffer) ,@body)
           (set-buffer ,cb-var)))))

(put 'corba-in-work-buffer 'lisp-indent-function 0)
 
;;;; Marshal

(defun corba-write-octet (n)
  (insert n))

(defun corba-write-bool (s)
  (insert (if s 1 0)))

(defun corba-write-align (n)
  (while (/= 1 (% (point) n))
    (insert 0)))

(defun corba-write-short (n)
  (corba-write-align 2)
  (insert n
          (logand (ash n -8)  255)))

(defun corba-write-ulong (n)
  (corba-write-align 4)
  (insert (logand n           255)
          (logand (ash n -8)  255)
          (logand (ash n -16) 255)
          (logand (ash n -24) 255)))

(defun corba-write-string (s)
  (corba-write-ulong (1+ (length s)))
  (insert s 0))

(defun corba-write-osequence (s)
  (corba-write-ulong (length s))
  (insert s))

(defun corba-write-sequence (s el-cdr)
  (corba-write-ulong (length s))
  (mapcar el-cdr s))

(defun corba-make-encapsulation (closure &rest args)
  (corba-in-work-buffer
    (insert 1)                        ; Byte order
    (apply closure args)
    (buffer-substring (point-min) (point-max))))

(defun corba-write-typecode (tc)
  (let ((kind (corba-typecode-kind tc))
	(params (corba-typecode-params tc)))
    (case kind                          ; We somtimes munge typcodes, try
                                        ; to supply reasonable defaults for
                                        ; lost info
      ((tk_string) (setq params (append params (list 0))))
      ((tk_sequence) (setq params (append params (list 0))))
      ((tk_objref)
       (setq params (append params
                            (list "IDL:omg.org/CORBA/Object:1.0"))))
      ((tk_enum) (error "Can't marshal ENUM TypeCode")))
    (corba-write-ulong (symbol-value kind))
    (let ((pspec (get kind 'tk-params)))
      (cond ((null pspec))
	    ((eq 'complex (car pspec))
             (corba-write-osequence
              (corba-make-encapsulation
               (lambda (params spec)
                 (mapcar* 'corba-marshal params spec))
               params (cdr pspec))))
	    (t
             (mapcar* 'corba-marshal params pspec))))))


(defun corba-write-ior (objref)
  (corba-write-string (if objref (or (corba-object-id objref) "") ""))
  (corba-write-sequence (if objref (corba-object-profiles objref) nil)
		(lambda (tagpair)
		  (corba-write-ulong (car tagpair))
		  (corba-write-osequence (cdr tagpair)))))


(defun corba-typecode-of (arg)
  (etypecase arg
    (number 'tk_long)
    (string 'tk_string)
    (corba-object 'tk_objref)
    (corba-struct (corba-struct-typecode (car arg)))
    ((or cons vector)
     (let ((elem (corba-typecode-of (elt arg 0))))
       (make-corba-typecode 'tk_sequence
                            (list elem 0))))))

(defun corba-marshal-any (arg)
  (let ((type-code
         (if (corba-any-p arg)
             (prog1 (corba-any-typecode arg)
               (setq arg (corba-any-value arg)))
           (corba-typecode-of arg))))
    (if nil
        type-code
      (corba-marshal type-code 'tk_TypeCode)
      (corba-marshal arg type-code))))



(defun corba-marshal (arg type)
  (let (kind params)
    (cond ((consp type)
	   (setq kind (car type)
		 params (cdr type)))
	  (t (setq kind type)))
    (case kind
      ((tk_any) (corba-marshal-any arg))
      ((tk_octet tk_char) (corba-write-octet arg))
      ((tk_boolean bool) (corba-write-bool arg))
      ((tk_ushort tk_short) (corba-write-short arg))
      ((tk_ulong tk_long tk_enum) (corba-write-ulong arg))
      ((tk_string string) (corba-write-string arg))
      ((osequence) (corba-write-osequence arg))
      ((tk_objref object) (corba-write-ior arg))
      ((tk_alias) (corba-marshal arg (third params)))
      ((tk_TypeCode) (corba-write-typecode arg))
      ((sequence tk_sequence)
       (let ((eltype (first params)))
	 (if (eq eltype 'tk_octet)
	     (corba-write-osequence arg)
           (corba-write-sequence arg (lambda (d) (corba-marshal d eltype))))))
      ((tk_struct)
       (mapcar (lambda (el)
                  (corba-marshal (cdr (assq (corba-lispy-name (first el)) arg))
                               (second el)))
            (third params)))
      ((anon-struct)
       (loop for type in params
	     for arg in arg
	     collect (corba-marshal arg type)))
      (t
       (corba-marshal arg (or (get type 'corba-typecode)
                            (error "MARSHAL: %s" type)))))))

 
;;;; UnMarshal

(defvar corba-byte-order 1)
(make-variable-buffer-local 'corba-byte-order)

(defsubst corba-read-octet ()
  (prog1 (following-char) (forward-char 1)))

(defun corba-read-bool ()
  (/= (corba-read-octet) 0))

(defsubst corba-read-align (n)
  (while (/= 1 (% (point) n))
    (forward-char 1)))


(defun corba-in-encapsulation (obj closure &rest args)
  (corba-in-work-buffer
    (insert obj)
    (goto-char (point-min))
    (setq corba-byte-order (corba-read-octet))
    (apply closure args)))


(defmacro corba-read-number (size signed)
  `(progn
     (corba-read-align ,size)
     (if (= corba-byte-order 1)
         (+
          ,@(loop for c below size collect
                  `(* ,(expt 2 (* c 8))
                      ,(if (and signed (= c (- size 1)))
                           '(let ((b (corba-read-octet)))
                              (if (> b 127) (- b 256) b))
                         '(corba-read-octet)))))
       (+
        ,@(loop for c from (1- size) downto 0 collect
                `(* ,(expt 2 (* c 8))
                    ,(if (and signed (= c (1- size)))
                         '(let ((b (corba-read-octet)))
                            (if (> b 127) (- b 256) b))
                       '(corba-read-octet))))))))

(defun corba-read-ushort ()
  (corba-read-number 2 nil))

(defun corba-read-short ()
  (corba-read-number 2 t))

(defsubst corba-read-ulong ()
  (corba-read-number 4 nil))

(defun corba-read-sequence (el-reader)
  (let ((len (corba-read-ulong)))
    (loop for i from 1 upto len
	  collect (funcall el-reader))))

(defun corba-read-string ()
  (let* ((len (corba-read-ulong))
	 (start (point)))
    (forward-char len)
    (buffer-substring start (1- (point)))))

(defun corba-read-osequence ()
  (let* ((len (corba-read-ulong))
	 (start (point)))
    (forward-char len)
    (buffer-substring start (point))))

(defun corba-read-typecode ()
  (let* ((tki (corba-read-ulong))
	 (tk (aref corba-tc-kind tki))
	 (params (get tk 'tk-params)))
    (cond ((null params)
           tk)
	  ((eq t params)
	   (make-corba-typecode tk (corba-read-osequence)))
	  ((eq 'complex (car params))
	   (corba-in-encapsulation
            (corba-read-osequence)
            (lambda (tk types)
              (make-corba-typecode tk (mapcar #'corba-unmarshal types)))
            tk (cdr params)))
	  (t
	   (make-corba-typecode tk (mapcar #'corba-unmarshal params))))))


(defun corba-read-any ()
  (let ((tc (corba-read-typecode)))
    (if corba-explicit-any
        (make-corba-any
         :typecode tc
         :value (corba-unmarshal tc))
      (corba-unmarshal tc))))

(defun corba-unmarshal (type)
  (let (kind params)
    (cond ((consp type)
	   (setq kind (car type)
		 params (cdr type)))
	  (t (setq kind type)))
    (case kind
      ((char octet tk_char tk_octet) (corba-read-octet))
      ((bool tk_boolean) (corba-read-bool))
      ((ushort tk_ushort) (corba-read-ushort))
      ((tk_short) (corba-read-short))
      ((ulong tk_ulong tk_enum) (corba-read-ulong))
      ((tk_long) (corba-read-number 4 t))
      ((string tk_string) (corba-read-string))
      ((tk_any) (corba-read-any))
      ((tk_sequence sequence)
       (let ((_ElType_ (car params)))
	 (if (eq _ElType_ 'tk_octet)
	     (corba-read-osequence)
	   (corba-read-sequence (lambda () (corba-unmarshal _ElType_))))))
      ((tk_alias)
       (corba-unmarshal (third params)))
      ((tk_struct)
       (cons (first params)
	     (mapcar (lambda (nt-pair)
                       (cons (corba-lispy-name (first nt-pair))
                             (corba-unmarshal (second nt-pair))))
                     (third params))))
      ((tk_except)
       (mapcar (lambda (nt-pair) (corba-unmarshal (second nt-pair)))
               (third params)))
      ((object tk_objref) (corba-read-ior))
      ((anon-struct)
       (mapcar #'corba-unmarshal params))
      ((tk_TypeCode) (corba-read-typecode))
      ((tk_null) nil)
      (t
       (corba-unmarshal (or (get kind 'corba-typecode)
                          (error "Can't handle TypeCode of kind %s" kind)))))))

 
;;;; GIOP / IIOP stuff

(defvar corba-message-size 0)
(make-variable-buffer-local 'corba-message-size)
(defvar corba-giop-version )
(make-variable-buffer-local 'corba-giop-version)

(defun corba-write-giop-header (type)
  (insert "GIOP" 1 0 1
	  (cond ((numberp type) type)
		((eq type 'request) 0)
		((eq type 'reply) 1)
		(t (error "Message type %s" type))))
  ;; Place for message length to be patched in later
  (corba-write-ulong 0))

(defun corba-write-giop-set-message-length ()
  (goto-char 9)
  (corba-write-ulong (- (point-max) 13))
  (delete-char 4))


(defun corba-read-giop-header ()
  (unless (looking-at "GIOP")
    (error "Not a GIOP message"))
  (forward-char 4)
  (let* ((major (corba-read-octet))
	 (minor (corba-read-octet))
	 (byte-order (corba-read-octet))
	 (msgtype (corba-read-octet)))
    (setq corba-giop-version (+ (* 100 major) minor))
    (setq corba-byte-order byte-order)
    msgtype))

(defun corba-read-tagged-component ()
  (cons (corba-read-ulong)
	(corba-read-osequence)))

(defun corba-read-service-context ()
  (corba-read-sequence #'corba-read-tagged-component))

(defun corba-read-iiop-profile-body (reference)
   (corba-read-octet)			;Version (ignored for now)
   (corba-read-octet)
   (setf (corba-object-host reference) (corba-read-string))
   (setf (corba-object-port reference) (corba-read-ushort))
   (setf (corba-object-key reference) (corba-read-osequence)))

(defun corba-read-ior ()
  (let* ((type-id (corba-read-string))
	 (reference (make-corba-object :id type-id)))
    (loop repeat (corba-read-ulong)
          for tag = (corba-read-ulong)
          for encaps = (corba-read-osequence)
	  if (= tag 0)
	  do (corba-in-encapsulation encaps
                                     #'corba-read-iiop-profile-body reference)
          do (push (cons tag encaps) (corba-object-profiles reference)))
    (if (or (not (equal type-id ""))
            (corba-object-key reference))
        reference
      nil)))

 
;;;; Connection handling

(defvar corba-iiop-connections nil)

(defun corba-get-connection (host port)
  (let* ((hp (assoc host corba-iiop-connections))
	 (pp (assq port (cdr hp))))
    (unless (and pp (eq (process-status (cdr pp)) 'open))
      (unless hp
	(push (setq hp (cons host nil)) corba-iiop-connections))
      (when pp
        (let ((proc (cdr pp)))
          (let ((buffer (process-buffer proc)))
            (when buffer (kill-buffer buffer)))
          (delete-process proc)))
      (let ((buffer (generate-new-buffer " *IIOP*")))
        (save-excursion
          (set-buffer buffer)
          (setq buffer-undo-list nil)
          (setq corba-message-size nil)
          (erase-buffer))
        (let ((proc
               (condition-case errinfo
                   (open-network-stream "iiop" buffer host port)
                 (error (kill-buffer buffer)
                        (signal (car errinfo) (cdr errinfo))))))
          ;; FIXME: should I check if open
          (if pp
              (setcdr pp proc)
            (setq pp (cons port proc))
            (push pp (cdr hp))))))
    (cdr pp)))

(defun corba-get-clients ()
  (loop for hp in corba-iiop-connections
	nconc (loop for pp in (cdr hp) collect (cdr pp))))

 
;;;; Requests

;; Interface: make-corba-request corba-request-result
(defstruct corba-request
  (object nil)
  (operation nil)
  (arguments nil)
  (req-id nil)
  (client nil)
  (result nil))

(defvar corba-request-id-seq 0)
(defvar corba-waiting-requests nil)

;; Interface:
(defun corba-request-send (req &optional flags)
  "Send the request to preform the remote CORBA operation defined by REQ.
To get the response from the server use `corba-request-get-response'
or `corba-get-next-respons'. The result from the operation will then
be available with `corba-request-result'. Several requests can be sent
before the getting the response. The flags argument is a list of
symbols. The only recognized symbol is `no-response' that indicates to
the server that no response is excpected."
  (let ((object (corba-request-object req)))
    (setq object (or (corba-object-forward object)
		     object))
    (condition-case exc
        (corba-request-send-to req object flags)
     (system-exception
       (setq object (corba-request-object req))
       (cond ((and (corba-object-forward object)
		   (member (car exc)
			   '("IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0"
			     "IDL:omg.org/CORBA/COMM_FAILURE:1.0")))
	      (setf (corba-object-forward object) nil)
	      (corba-request-send-to req object flags))
	     (t
	      (signal exc)))))))

(defun corba-request-send-to (req object &optional flags)
  (let* ((client (corba-get-connection
		  (corba-object-host object)
		  (corba-object-port object)))
	 (operation (corba-request-operation req)))
    (setf (corba-request-req-id req) (incf corba-request-id-seq))
    (setf (corba-request-client req) client)
    (setf (corba-request-result req) t)
    (corba-in-work-buffer
      (cond
       ((eq operation 'locate)
        (corba-write-giop-header 3)		;LocateRequest
        (corba-write-ulong (corba-request-req-id req))
        (corba-write-osequence (corba-object-key object)))
       (t
        (corba-write-giop-header 'request)
        (corba-write-ulong 0)			;context
        (corba-write-ulong (corba-request-req-id req))
        (corba-write-octet (if (memq 'no-response flags) 0 1)) ;respons expected
        (corba-write-osequence (corba-object-key object))
        (corba-write-string (corba-opdef-name operation))
        (corba-write-osequence corba-principal)		;principal
        (loop for arg in (corba-request-arguments req)
              for desc in (corba-opdef-inparams operation)
              do (corba-marshal arg (cdr desc)))))
      (corba-write-giop-set-message-length)
      (process-send-region client (point-min) (point-max))
      ;;(message "Request %d sent" (corba-request-req-id req))
      ;;(accept-process-output)
      (push req corba-waiting-requests))))


(defun corba-read-reply (req)
  (setf (corba-request-result req) nil)
  (ecase (corba-read-ulong)
    ((0)				; No Exception
     (setf (corba-request-result req)
	   (loop for desc in (corba-opdef-outparams
			      (corba-request-operation req))
		 collect (corba-unmarshal (cdr desc))))
     t)
    ((1)				; User Exception
     (let* ((id (corba-read-string)))
       (signal 'corba-user-exception
               (cons id (corba-unmarshal (corba-get-typecode id))))))
    ((2)				; System Exception
     (let* ((id (corba-read-string))
	    (minor (corba-read-ulong))
	    (status (corba-read-ulong)))
       (signal 'corba-system-exception
	       (list id minor status))))
    ((3)				; Forward
     (setf (corba-object-forward (corba-request-object req))
	   (corba-read-ior))
     (corba-request-send req)
     nil)))


(defun corba-get-next-respons-1 (client)
  (save-excursion
    (set-buffer (process-buffer client))
    (when corba-message-size
      (goto-char (point-min))
      (delete-char corba-message-size)
      (setq corba-message-size nil))
    (cond
     ((>= (point-max) 12)
      (goto-char (point-min))
      (let ((msgtype (corba-read-giop-header)))
        (setq corba-message-size (+ 12 (corba-read-ulong)))
        (cond
         ((<= (point-max) corba-message-size)
          (setq corba-message-size nil))
         ((memq msgtype '(1 4))         ;Reply
          (when (= msgtype 1)
            ;; Ignore service context
            (corba-read-service-context))
          (let* ((request-id (corba-read-ulong))
                 (req
                  (loop for req in corba-waiting-requests
                        if (= request-id (corba-request-req-id req))
                        return req)))
            (cond
             (req
              (setq corba-waiting-requests (delq req corba-waiting-requests))
              (if (= msgtype 1)
                  (and (corba-read-reply req)
                       req)
                (let ((status (corba-read-ulong)))
                  (cond ((= status 2)
                         (setf (corba-object-forward (corba-request-object req))
                               (corba-read-ior))
                         (corba-request-send req)
                         nil)
                        (t
                         (setf (corba-request-result req) status)
                         req)))))
             (t
              (message "Unexpected respons for id %s" request-id)))))
         ((= msgtype 5)                 ;Close Connection
          (delete-process client)
          (error "Connection closed"))))))))


;; Interface:
(defun corba-get-next-respons (&optional flags)
  (let ((req nil))
    (loop
     do (setq req (loop for client in (corba-get-clients)
			thereis (corba-get-next-respons-1 client)))
     until (or req (not (memq 'no-wait flags)))
     do (accept-process-output))
    req))

;; Interface:
(defun corba-request-get-response (request &optional flags)
  "Get the response for the REQUEST sent earlier with `corba-request-send'.
If FLAGS is list containing the symbols `no-wait', the function will
not wait for the response if it is not immediately available. Returns
`t' if the response has arrived otherwise returns `nil' (will always
return `t' unless flags contains `no-wait'.)"
  (loop while (eq t (corba-request-result request))
        do (corba-get-next-respons-1 (corba-request-client request))
        until (memq 'no-wait flags)
        do (accept-process-output))
  (not (eq t (corba-request-result request))))


;; Interface:
(defun corba-request-invoke (req &optional flags)
  "Invoke the CORBA operation defined by the corba-request REQ.
Result is the list of the values of the out parameters."
  (corba-request-send req)
  (corba-request-get-response req)
  (corba-request-result req))

 
;;;; The ORB Interface

;;;###autoload
(defun corba-orb-init (&optional args orbid)
  nil)

;; Interface:
(defun corba-orb-resolve-initial-references (orb name)
  (cond
   ((string-equal name "NameService")
    (corba-file-to-object corba-name-service))
   ((string-equal name "InterfaceRepository")
    (corba-file-to-object corba-interface-repository))))


;; Interface:
(defun corba-orb-string-to-object (orb str)
  (if (string-match "IOR:" str)
      (corba-in-encapsulation
       (loop for i from 4 below (length str) by 2
	     concat (char-to-string
		     (+ (* 16 (corba-hex-to-int (aref str i)))
			(corba-hex-to-int (aref str (1+ i))))))
       #'corba-read-ior)
    (error "Illegal string object")))

;; Interface:
(defun corba-orb-object-to-string (orb object)
  (let ((str (corba-make-encapsulation #'corba-write-ior object)))
    (concat "IOR:"
	    (upcase (loop for c across str
			  concat (format "%02x" c))))))


(defun corba-file-to-object (file)
  (corba-orb-string-to-object
   nil					; No orb-struct yet
   (if (string-match "IOR:" file)
       file				; Looks like the IOR itself
     (save-excursion
       (set-buffer (get-buffer-create "*REQ*"))
       (erase-buffer)
       (insert-file-contents file)
       (goto-char (point-min))
       (end-of-line 1)
       (buffer-substring (point-min) (point))))))

 
;;;; The Object Interface

;; Interface:
(defun corba-object-is-a (obj id)
  (car (corba-request-invoke 
        (make-corba-request
         :object obj
         :operation (make-corba-opdef :name "_is_a"
                                      :inparams '(("id" . tk_string))
                                      :outparams '(("" . tk_boolean)))
         :arguments (list id)))))

;; Interface:
(defun corba-object-is-nil (obj)
  (or (null obj)
      (and (null (corba-object-key obj))
           (zerop (length (corba-object-profiles obj))))))


;; Interface:
(defun corba-object-narrow (obj id)
  (unless (corba-object-is-a obj id)
    (error "Cannot narrow to '%s', object %s" id obj))
  (setf (corba-object-id obj) id))

(defun corba-object-auto-narrow (obj)
  (let ((interface
         (corba-get-objects-interface obj)))
    (unless interface
      (error "The object cannot tell us its interface: %s" obj))
    (let ((new-id (corba-interface-id interface)))
      (unless (equal new-id (corba-object-id obj))
        (setf (corba-object-id obj) new-id)))))

 
;;;; Interfaces and operations

(defconst corba-object-interface
  (make-corba-interface
   :id "IDL:omg.org/CORBA/Object:1.0"
   :operations (list
                (make-corba-opdef :name "_is_a"
                            :inparams '(("id" . tk_string))
                            :outparams '(("" . tk_boolean))
                            :raises '())
                (make-corba-opdef :name "_interface"
                            :outparams '(("" . tk_objref)))
                (make-corba-opdef :name "_non_existent"
                            :outparams '(("" . tk_boolean))))))

(defun corba-find-opdef (interface operation)
  "Find in INTERFACE the OPERATION and return the opdef struct."
  (or (find operation
	    (corba-interface-operations interface)
	    :test #'equal
	    :key #'corba-opdef-name)
      (loop for pint in (corba-interface-inherit interface)
	    thereis (corba-find-opdef pint operation))))

 
;;;; Internal Interface Repository

(defvar corba-local-repository
  (make-hash-table :test #'equal))


;; Interface:
(defun corba-add-interface (interface)
  (setf (gethash (corba-interface-id interface)
		 corba-local-repository)
	interface))

(defun corba-has-interface-p (id)
  (gethash id corba-local-repository))

(defun corba-get-interface (id)
  (or (gethash id corba-local-repository)
      (setf (gethash id corba-local-repository)
	    (corba-interface-from-id id))))


(defun corba-get-typecode (id)
  (or (gethash id corba-local-repository)
      (setf (gethash id corba-local-repository)
	    (corba-typecode-from-def id))))


(defun corba-get-objects-interface (object)
  (condition-case exc
      (let ((idef
             (car (corba-invoke object "_interface"))))
        (if idef
            (corba-interface-from-def-cached nil idef)))
    (corba-system-exception
     (message "_interface: %s" exc)
     nil)
    (end-of-buffer
     ;; Work around ORBit bug in exception marshaling
     (message "_interface: %s" exc)
     nil)))


;; Interface:
(defun corba-object-create-request (object op args)
  "Create a request object for an operation on a CORBA object.
OBJECT is the CORBA object, OP the name of the operation and arguments
ARGS. The arguments are the in-paramenters of the operation in the
IDL-defition.
    This functions requires that the interface for the
object is known to the ORB, either from an explicit definition of the
interface or from an Interface Repository.
    OP can also be a list (INTERFACE-ID OP-NAME) to use the operation
definition from a specific interface inditified by INTERFACE-ID,
the interface repository ID."
  (let* ((interface-id (if (consp op) (first op) (corba-object-id object)))
         (operation (if (consp op) (second op) op))
	 (opdef
          (or (corba-find-opdef corba-object-interface operation)
              (corba-find-opdef (or (corba-has-interface-p interface-id)
                                    (corba-get-objects-interface object)
                                    (corba-get-interface interface-id))
                                operation))))
    (unless opdef
      (error "Undefined operation %s for interface %s"
             op interface-id))
    (unless (= (length args)
	       (length (corba-opdef-inparams opdef)))
      (error "Wrong number of arguments to operation"))
    (make-corba-request :object object
                        :arguments args
                        :operation opdef)))


;; Interface:
(defun corba-invoke (obj op &rest args)
  "Invoke operation OP on object OBJ with arguments ARGS.
Returns the list of result and out parameters."
  (corba-request-invoke
   (corba-object-create-request obj op args)))


(defun corba-locate (obj)
  "Send a Locate Request for the object OBJ.
The result is status for response, 1 - ?, 3 - ?."
  (let ((req (make-corba-request :object obj
                                 :operation 'locate)))
    (corba-request-invoke req)))


(defun corba-simplify-type (typecode)
  ;; Simplify a TYPECODE returning a simplified version. Usually used
  ;; for typecodes gotten from the Interface Repository. Some kinds of
  ;; typecodes are stored in the internal repository. These always
  ;; have a repository id as key. The simplified typecode will be the
  ;; version stored in the internal repository.
  (macrolet ((mush (id def)
               `(or (gethash ,id corba-local-repository)
                 (setf (gethash ,id corba-local-repository) ,def)))
             (simplifyf (var)
               `(progn (setf ,var (corba-simplify-type ,var))
                 typecode))
             (simplifyv (vec fun)
               `(progn (loop for el in ,vec do (simplifyf (,fun el)))
                 typecode)))
    (let ((params (corba-typecode-params typecode)))
      (case (corba-typecode-kind typecode)
        ((tk_objref tk_string tk_enum) (corba-typecode-kind typecode))
        ((tk_alias) (mush (first params) (corba-simplify-type (third params))))
        ((tk_sequence) (simplifyf (first params)))
        ((tk_struct) (mush (first params) (simplifyv (third params) second)))
        ((tk_except) (mush (first params) (simplifyv (third params) second)))
        (t  typecode)))))


 
;;;; CORBA Structure support

;; Interface:
(defun corba-struct-typecode (id  &optional name fields)
  "Create a corba-typecode for a Corba struct of type ID.
ID should be a repository id. If optional NAME and FIELDS are given
these will be used as the idl-name and the field specifications. If
not FILEDS is given the Interface Repository will be used to get the
typecode."
  (if (null fields)
      (corba-get-typecode id)
    (corba-simplify-type
     (make-corba-typecode 'tk_struct
                    (list id (format "%s" (or name ""))
                          (if (consp fields)
                              (apply #'vector fields)
                              fields))))))

;; Interface:
(defsubst corba-struct-get (struct key)
  "Get field with KEY from the STRUCT."
  (cdr (assq key struct)))

;; Interface:
(defun corba-struct (id &rest nv-pairs)
  "Make a CORBA structure of type ID.
NV-PAIRS is a list field names and field values.
If ID is nil, then all fields must be supplied. Otherwise some types
of fields can be defaulted (numbers and strings)."
  (cond
   ((null id)
    (cons "" (loop for nv on nv-pairs by #'cddr collect
                   (cons (first nv) (second nv)))))
   (t
    (let ((tc (corba-get-typecode id)))
      (destructuring-bind (id name fields)
          (corba-typecode-params tc)
        (cons id
              (mapcar (lambda (nv)
                        (let* ((fname (corba-lispy-name (first nv)))
                               (val (getf nv-pairs fname nv)))
                          (cons fname
                                (if (eq val nv)
                                    (corba-default-from-type (second nv))
                                  val))))
                      fields)))))))

(defun corba-struct-p (sexp)
  (and (consp sexp)
       (stringp (car sexp))
       (loop for x = (cdr sexp) then (cdr x)
             always (and (consp x)
                         (and (consp (car x))
                              (symbolp (caar x))))
             until (null (cdr x)))))

(defun corba-default-from-type (typecode)
  ;; Return a suitable default value for the given TYPECODE.
  ;; Some typecodes have no suitable value, these will result in an error.
  (ecase (corba-typecode-kind typecode)
    ((tk_ushort tk_short tk_ulong tk_long tk_char tk_octet tk_enum) 0)
    ((tk_boolean) nil)
    ((tk_string) "")
    ((tk_sequence) nil)
    ((tk_objref) (make-corba-object))))
 
;;;; IR -- initial repository contents

(corba-add-interface corba-object-interface)

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/IRObject:1.0"
  :inherit (list corba-object-interface)
  :operations (list
	       (make-corba-opdef :name "_get_def_kind"
                                 :outparams '(("" . tk_ulong))))))


(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/Contained:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/IRObject:1.0"))
  :operations
  (list
   (make-corba-opdef :name "_get_id"
                     :outparams '(("" . tk_string)))
   (make-corba-opdef :name "_get_name"
                     :outparams '(("" . tk_string)))
   (make-corba-opdef :name "_get_defined_in"
                     :outparams '(("" . tk_objref)))
   (make-corba-opdef :name "describe"
                     :outparams `(("" .
                                   ,(corba-struct-typecode
                                     "IDL:omg.org/CORBA/Contained/Description:1.0"
                                     "Description"
                                     [(kind tk_ulong) (value tk_any)])))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/Container:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/IRObject:1.0"))
  :operations
  (list
   (make-corba-opdef :name "lookup"
                     :inparams '(("search_name" . tk_string))
                     :outparams '(("" . tk_objref)))
   (make-corba-opdef :name "contents"
                     :inparams '(("limit_type" . tk_ulong)
                                 ("exclude_inherit" . tk_boolean))
                     :outparams '(("" sequence tk_objref)))
   (make-corba-opdef :name "lookup_name"
                     :inparams '(("search_name" . tk_string)
                                 ("levels_to_search" . tk_long)
                                 ("limit_type" . tk_ulong)
                                 ("exclude_inherit" . tk_boolean))
                     :outparams '(("" . (sequence tk_objref)))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/IDLType:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/IRObject:1.0"))
  :operations (list
	       (make-corba-opdef :name "_get_type"
                                 :outparams '(("" . tk_TypeCode))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/Repository:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/Container:1.0"))
  :operations (list
	       (make-corba-opdef :name "lookup_id"
                                 :inparams '(("search_id" . tk_string))
                                 :outparams '(("" . tk_objref))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/OperationDef:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/Contained:1.0"))
  :operations
  (list
   (make-corba-opdef :name "_get_result"
                     :outparams '(("" . tk_TypeCode)))
   (make-corba-opdef
    :name "_get_params"
    :outparams `(("" sequence ,(corba-struct-typecode
                                "IDL:omg.org/CORBA/ParameterDescription:1.0"
                                "ParameterDescription"
                                [(name tk_string )
                                 (type tk_TypeCode)
                                 (type-def tk_objref)
                                 (mode tk_ulong)       ])))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/InterfaceDef:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/Container:1.0"
                                           "IDL:omg.org/CORBA/Contained:1.0"
                                           "IDL:omg.org/CORBA/IDLType:1.0"))
  :operations
  (list
   (make-corba-opdef :name "_get_base_interfaces"
                     :outparams '(("" . (sequence tk_objref))))
   (make-corba-opdef :name "is_a"
                     :inparams '(("interface_id" . tk_string))
                     :outparams '(("" . tk_boolean))))))


(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/ExceptionDef:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/Contained:1.0"
                                           "IDL:omg.org/CORBA/IDLType:1.0"))
  :operations
  (list
   (make-corba-opdef :name "_get_members"
                     :outparams '(("" sequence
                                   (anon-struct string tk_TypeCode tk_objref))))
   (make-corba-opdef :name "_get_type"
                     :outparams '(("" . tk_TypeCode))))))
 
;;;; Using real IR

(defun corba-get-ir ()
  "Get an object reference to the Interface Repository"
  (corba-orb-resolve-initial-references nil "InterfaceRepository"))

(defun corba-opdef-from-ir (irdef)
  "Obtain a Operation Definition (corba-opdef) from the IRDEF.
IRDEF can be an Operation Definition object or the interface repository
id for the operation."
  (when (stringp irdef)
    (setq irdef (car (corba-invoke (corba-get-ir) "lookup_id" irdef))))

  (let ((name (car (corba-invoke irdef "_get_name"))) 
	(inpars nil)
	(outpars nil)
	(result (car (corba-invoke irdef "_get_result"))))

    (unless (eq 'tk_void (corba-typecode-kind result))
      (push (cons "" (corba-simplify-type result)) outpars))

    (loop for pardesc in (car (corba-invoke irdef "_get_params"))
	  for mydesc = (cons (corba-struct-get pardesc 'name)
			     (corba-simplify-type (corba-struct-get pardesc 'type)))
	  for mode = (corba-struct-get pardesc 'mode)
	  do (cond ((memq mode '(0 2))
		    (push mydesc inpars)))
	  do (cond ((memq mode '(1 2))
		    (push mydesc outpars))))

    (make-corba-opdef
     :name name
     :inparams (nreverse inpars)
     :outparams (nreverse outpars))))


(defun corba-interface-from-id (id)
  (let ((def (car (corba-invoke (corba-get-ir) "lookup_id" id))))
    (when (corba-object-is-nil def)
      (error "InterfaceRepository do not know about %s" id))
    (corba-interface-from-def id def)))

(defun corba-interface-from-def-cached (id def)
  (unless id
    (setq id (car (corba-invoke def "_get_id"))))
  (or (corba-has-interface-p id)
      (corba-add-interface (corba-interface-from-def id def))))

(defun corba-interface-from-def (id def)
  (unless id
    (setq id (car (corba-invoke def "_get_id"))))
  (let ((mess "Getting interface %s %s")
        (progress ""))
    (message mess id progress)
    (make-corba-interface
     :id id
     :inherit (or (mapcar #'(lambda (idef)
                              (corba-interface-from-def-cached nil idef))
                          (car (corba-invoke def "_get_base_interfaces")))
                  (list corba-object-interface))
     :operations (mapcar (lambda (o)
                           (prog1 (corba-opdef-from-ir o)
                             (setq progress (concat progress "."))
                             (message mess id progress)))
                         (car (corba-invoke def "contents" 7 t))))))

(defun corba-enter-interface-def (def)
  (let ((id (car (corba-invoke def "_get_id"))))
    (unless (corba-has-interface-p id)
      (corba-add-interface (corba-interface-from-def id def)))))


(defun corba-typecode-from-def (def)
  (when (stringp def)
    (let ((id def))
      (message "Getting type %s" id)
      (setq def (car (corba-invoke (corba-get-ir) "lookup_id" id)))
      (when (corba-object-is-nil def)
	(error "InterfaceRepository do not know about %s" id))))
  (let ((typecode
         (car (corba-invoke def
                            '("IDL:omg.org/CORBA/IDLType:1.0" "_get_type")))))
    (corba-simplify-type typecode)))
 
;;;; Name Service Shortcuts

(defun corba-resolve (&rest names)
  (let ((nsid "IDL:omg.org/CosNaming/NamingContext:1.0")
	(n (mapcar (lambda (id)
                     (corba-struct "IDL:omg.org/CosNaming/NameComponent:1.0"
                                   'id id 'kind ""))
                   names))
        (ns (corba-orb-resolve-initial-references (corba-orb-init)
                                                  "NameService")))
    (assert (corba-object-is-a ns nsid))
    (first (corba-invoke ns (list nsid "resolve") n))))

 
;;;; ORBit hacks

(defun corba-setup-orbit-cookie ()
  (setq corba-principal
        (let ((cookie-file
               (format "/tmp/orbit-%s/cookie" user-login-name)))
          (unless (file-exists-p cookie-file)
            (error "No ORBit cookie file"))
          (save-excursion
            (set-buffer (find-file-noselect cookie-file))
            (concat (buffer-string) "\0")))))

 
;;;;

(defun corba-reset-all ()
  (loop for c in (corba-get-clients)
        do (delete-process c))
  (setq corba-iiop-connections nil)
  (setq corba-waiting-requests nil))


 (provide 'corba)
;;; corba.el ends here
