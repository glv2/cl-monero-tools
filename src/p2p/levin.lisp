;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-p2p)


(defun make-request (command payload)
  (concatenate 'octet-vector
               (integer->bytes +levin-signature+ :size 8)
               (integer->bytes (length payload) :size 8)
               #(1)
               (integer->bytes command :size 4)
               (integer->bytes 0 :size 4)
               (integer->bytes +levin-packet-request+ :size 4)
               (integer->bytes +levin-protocol-version-1+ :size 4)
               payload))

(defun make-response (command payload return-code)
  (concatenate 'octet-vector
               (integer->bytes +levin-signature+ :size 8)
               (integer->bytes (length payload) :size 8)
               #(0)
               (integer->bytes command :size 4)
               (integer->bytes return-code :size 4)
               (integer->bytes +levin-packet-response+ :size 4)
               (integer->bytes +levin-protocol-version-1+ :size 4)
               payload))

(defun parse-levin-header (header)
  (unless (and (= (length header) 33)
               (= (bytes->integer header :end 8) +levin-signature+))
    (error "Bad header"))
  (let ((payload-length (bytes->integer header :start 8 :end 16))
        (return-data-p (plusp (aref header 16)))
        (command (bytes->integer header :start 17 :end 21))
        (return-code (bytes->integer header :start 21 :end 25))
        (flags (bytes->integer header :start 25 :end 29))
        (protocol-version (bytes->integer header :start 29 :end 33)))
    (unless (and (<= payload-length +levin-default-max-packet-size+)
                 (= protocol-version +levin-protocol-version-1+))
      (error "Bad header"))
    (values payload-length return-data-p command return-code flags)))

(defun read-levin-packet (stream)
  (let ((header (coerce (loop repeat 33
                              for b = (read-byte stream nil nil)
                              while b
                              collect b)
                        'octet-vector)))
    (multiple-value-bind (payload-length return-data-p command return-code flags)
        (parse-levin-header header)
      (print return-data-p)
      (print command)
      (print return-code)
      (print flags)
      (coerce (loop repeat payload-length
                    for b = (read-byte stream nil nil)
                    while b
                    collect b)
              'octet-vector))))
