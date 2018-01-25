;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(define-foreign-library unbound
  (t (:default "libunbound")))

(handler-case
    (use-foreign-library unbound)
  (load-foreign-library-error ()
    (warn "The unbound library was not found.")))

(defcstruct ub-result
  (qname :pointer)
  (qtype :int)
  (qclass :int)
  (data :pointer)
  (len :pointer)
  (canonname :pointer)
  (rcode :int)
  (answer-packet :pointer)
  (answer-len :int)
  (havedata :int)
  (nxdomain :int)
  (secure :int)
  (bogus :int)
  (why-bogus :pointer)
  (ttl :int))

(defparameter *dnssec-trust-anchors*
  (asdf:system-relative-pathname "monero-tools" "conf/dnssec-trust-anchors.txt"))

(defun get-monero-txt-record (name)
  "Make a DNS query for NAME and return the Monero TXT record.
The second returned value is T if the DNS answer was validated by
DNSSEC, and NIL otherwise. The DNSSEC keys are taken from the file
specified in the *DNSSEC-TRUST-ANCHORS* parameter."
  (if (foreign-library-loaded-p 'unbound)
      (let* ((context (foreign-funcall "ub_ctx_create" :pointer))
             (text nil)
             (validated nil))
        (with-foreign-object (result :pointer)
          (when (uiop:file-exists-p *dnssec-trust-anchors*)
            (foreign-funcall "ub_ctx_add_ta_autr"
                             :pointer context
                             :string (namestring *dnssec-trust-anchors*)
                             :int))
          (foreign-funcall "ub_resolve"
                           :pointer context
                           :string name
                           :int 16 ; type TXT
                           :int 1 ; class IN
                           :pointer result
                           :int)
          (with-foreign-slots ((data len secure bogus) (mem-ref result :pointer) (:struct ub-result))
            (setf validated (= secure 1))
            (setf text (do* ((i 0 (1+ i))
                             (record (mem-aref data :pointer i) (mem-aref data :pointer i)))
                            ((null-pointer-p record))
                         (let* ((size (1- (mem-aref len :int i)))
                                (record-bytes (c-array->lisp-array (inc-pointer record 1) size))
                                (record-text (bytes->utf-8-string record-bytes)))
                           (when (and (>= size 8)
                                      (string= record-text "oa1:xmr " :end1 8))
                             (return (subseq record-text 8)))))))
          (foreign-funcall "ub_resolve_free"
                           :pointer (mem-ref result :pointer)))
        (foreign-funcall "ub_ctx_delete"
                         :pointer context)
        (values text validated))
      (error "DNS queries not supported because the unbound library was not found.")))
