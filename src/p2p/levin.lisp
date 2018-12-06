;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-p2p)


(defun write-levin-packet (stream return-data-p command return-code flags &optional payload)
  (write-sequence +levin-signature+ stream)
  (write-sequence (integer->bytes (length payload) :size 8) stream)
  (write-byte (if return-data-p 1 0) stream)
  (write-sequence (integer->bytes command :size 4) stream)
  (write-sequence (integer->bytes return-code :size 4) stream)
  (write-sequence (integer->bytes flags :size 4) stream)
  (write-sequence (integer->bytes +levin-protocol-version-1+ :size 4) stream)
  (when payload
    (write-sequence payload stream))
  (finish-output stream))

(defun read-levin-packet (stream)
  (let ((header (coerce (loop repeat 33
                              for b = (read-byte stream nil nil)
                              while b
                              collect b)
                        'octet-vector)))
    (when (or (/= (length header) 33)
              (mismatch header +levin-signature+ :end1 8))
      (error "Bad packet header"))
    (let ((payload-length (bytes->integer header :start 8 :end 16))
          (return-data-p (plusp (aref header 16)))
          (command (bytes->integer header :start 17 :end 21))
          (return-code (bytes->integer header :start 21 :end 25))
          (flags (bytes->integer header :start 25 :end 29))
          (protocol-version (bytes->integer header :start 29 :end 33)))
      (unless (and (<= payload-length +levin-default-max-packet-size+)
                   (= protocol-version +levin-protocol-version-1+))
        (error "Bad packet header"))
      (let ((payload (coerce (loop repeat payload-length
                                   for b = (read-byte stream nil nil)
                                   while b
                                   collect b)
                             'octet-vector)))
        (values return-data-p command return-code flags payload)))))

(defun write-request (stream command payload)
  (write-levin-packet stream t command 0 +levin-packet-request+ payload))

(defun write-response (stream command payload return-code)
  (write-levin-packet stream nil command return-code +levin-packet-response+ payload))
