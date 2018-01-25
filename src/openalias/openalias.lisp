;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun normalize-openalias-address (address)
  (substitute #\. #\@ address))

(defun get-openalias-info (address)
  "Return an alist containing the openalias information of an ADDRESS.
The second returned value is T is the validity of the information has
been verified by DNSSEC, and NIL otherwise. The DNSSEC keys are taken
from the file specified in the *DNSSEC-TRUST-ANCHORS* parameter."
  (multiple-value-bind (text validated)
      (get-monero-txt-record (normalize-openalias-address address))
    (flet ((get-field (name)
             (let* ((regexp (format nil "~a=(.*?);" name))
                    (fields (nth-value 1 (cl-ppcre:scan-to-strings regexp text))))
               (when fields
                 (aref fields 0)))))
      (let ((address (get-field "recipient_address"))
            (recipient-name (get-field "recipient_name"))
            (description (get-field "tx_description"))
            (payment-id (get-field "tx_payment_id"))
            (amount (get-field "tx_amount")))
        (values (append (when address
                          (list (cons :address address)))
                        (when recipient-name
                          (list (cons :recipient-name recipient-name)))
                        (when description
                          (list (cons :description description)))
                        (when payment-id
                          (list (cons :payment-id payment-id)))
                        (when amount
                          (list (cons :amount amount))))
                validated)))))
