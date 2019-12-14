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

(defparameter *dns-server* nil
  "Server to forward DNS queries to. If NIL, use what the operating
system uses")

(defparameter *dnssec-trust-anchors*
  (list ". IN DS 19036 8 2 49AAC11D7B6F6446702E54A1607371607A1A41855200FD2CE1CDDE32F24E8FB5"
        ". IN DS 20326 8 2 E06D44B80B8F1D39A95C0B0D7C65D08458E880409BBC683457104237C7F8EC8D")
  "Trust anchors used for DNSSEC validation. The format is a list of strings,
similar to the zone-file format, [domainname] [type] [rdata contents]. Both DS
and DNSKEY records are accepted.")

(defun get-monero-txt-record (name)
  "Make a DNS query for NAME and return the Monero TXT record. The second
returned value is T if the DNS answer was validated by DNSSEC, and NIL
otherwise. The DNS query is forwarded to *DNS-SERVER*. The trust anchors used
for DNSSEC validation are specified in the *DNSSEC-TRUST-ANCHORS* parameter."
  (if (foreign-library-loaded-p 'unbound)
      (let* ((context (foreign-funcall "ub_ctx_create" :pointer))
             (text nil)
             (validated nil))
        (unless (pointer-eq context (null-pointer))
          (with-foreign-object (result :pointer)
            (if *dns-server*
                (progn
                  (foreign-funcall "ub_ctx_set_fwd"
                                   :pointer context
                                   :string *dns-server*
                                   :int)
                  (foreign-funcall "ub_ctx_set_option"
                                   :pointer context
                                   :string "do-udp:"
                                   :string "no")
                  (foreign-funcall "ub_ctx_set_option"
                                   :pointer context
                                   :string "do-tcp:"
                                   :string "yes"))
                (progn
                  (foreign-funcall "ub_ctx_resolvconf"
                                   :pointer context
                                   :pointer (null-pointer)
                                   :int)
                  (foreign-funcall "ub_ctx_hosts"
                                   :pointer context
                                   :pointer (null-pointer)
                                   :int)))
            (dolist (trust-anchor *dnssec-trust-anchors*)
              (foreign-funcall "ub_ctx_add_ta"
                               :pointer context
                               :string trust-anchor
                               :int))
            (foreign-funcall "ub_resolve"
                             :pointer context
                             :string name
                             :int 16 ; type TXT
                             :int 1 ; class IN
                             :pointer result
                             :int)
            (unless (pointer-eq result (null-pointer))
              (with-foreign-slots ((data len secure) (mem-ref result :pointer) (:struct ub-result))
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
                               :pointer (mem-ref result :pointer))))
          (foreign-funcall "ub_ctx_delete"
                           :pointer context))
        (values text validated))
      (error "DNS queries not supported because the unbound library was not found.")))
