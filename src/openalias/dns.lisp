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

(defun get-monero-txt-record (name)
  "Make a DNS query for NAME and return the Monero TXT record.
The second returned value is T if the DNS answer was validated
(DNSSEC), and NIL otherwise."
  (if (foreign-library-loaded-p 'unbound)
      (let* ((name-bytes (utf-8-string->bytes name))
             (context (foreign-funcall "ub_ctx_create" :pointer))
             (text nil)
             (validated nil))
        (with-foreign-objects ((raw-name :unsigned-char (length name-bytes))
                               (result :pointer))
          (lisp-array->c-array name-bytes raw-name)
          (foreign-funcall "ub_resolve"
                           :pointer context
                           :pointer raw-name
                           :int 16 ; type TXT
                           :int 1 ; class IN
                           :pointer result
                           :int)
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
                           :pointer (mem-ref result :pointer)))
        (foreign-funcall "ub_ctx_delete"
                         :pointer context)
        (values text validated))
      (error "DNS queries not supported because the unbound library was not found.")))
