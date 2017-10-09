;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun make-qr-code (png-file address &key payment-id recipient-name amount description)
  "Write the QR code containing the information for a payment to
a PNG-FILE."
  (let ((uri (make-uri address
                       :payment-id payment-id
                       :recipient-name recipient-name
                       :amount amount
                       :description description)))
    (cl-qrencode:encode-png uri :fpath png-file :pixsize 4)))

;; (defun decode-qr-code (file)
;;   (let ((uri (decode-png file))) ; ffi for zbar ?
;;     (decode-uri uri)))
