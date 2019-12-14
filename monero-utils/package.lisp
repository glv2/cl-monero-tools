;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-utils
  (:use :cl)
  (:import-from :alexandria
                #:define-constant)
  (:import-from :babel
                #:octets-to-string
                #:string-to-octets)
  (:import-from :cffi
                #:mem-aref)
  (:import-from :ironclad
                #:integer-to-octets
                #:octets-to-integer)
  (:export
   #:octet-vector
   #:+base58-checksum-size+
   #:base58-encoded-length #:base58-decoded-length
   #:base58-encode #:base58-decode
   #:integer->bytes #:bytes->integer
   #:string->bytes #:bytes->string
   #:utf-8-string->bytes #:bytes->utf-8-string
   #:hex-string->bytes #:bytes->hex-string
   #:geta
   #:read-float #:format-float
   #:lisp-array->c-array #:c-array->lisp-array
   #:json-name->lisp-name #:lisp-name->json-name
   #:decode-json-from-string #:encode-json-to-string))
