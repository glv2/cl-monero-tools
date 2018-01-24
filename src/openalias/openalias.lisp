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
been verified (DNSSEC), and NIL otherwise."
  (let* ((normalized-address (normalize-openalias-address address))
         ;; TODO: use Lisp code instead of external program for DNS query
         (answer (uiop:run-program (format nil "nslookup -q=TXT ~a" normalized-address)
                                   :output :string
                                   :ignore-error-status t))
         ;; TODO: DNSSEC validation
         (valid-dnssec nil)
         (fields (nth-value 1 (cl-ppcre:scan-to-strings "text = \"oa1:xmr (.*)\"" answer)))
         (info (when fields
                 (aref fields 0))))
    (flet ((get-field (name)
             (let* ((regexp (format nil "~a=(.*?);" name))
                    (fields (nth-value 1 (cl-ppcre:scan-to-strings regexp info))))
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
                valid-dnssec)))))