;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun make-uri (address &key payment-id recipient-name amount description)
  "Return an URI containing the information for a payment."
  (flet ((encode (string)
           (with-output-to-string (out)
             (with-input-from-string (in string)
               (do ((c (read-char in nil) (read-char in nil)))
                   ((null c))
                 (if (member c '(#\space #\% #\: #\/ #\? #\# #\[ #\] #\@
                                 #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))
                     (format out "%~2,'0x" (char-code c))
                     (write-char c out)))))))
    (let ((parameters (append (when payment-id
                                (list (format nil "tx_payment_id=~a" (bytes->hex-string payment-id))))
                              (when recipient-name
                                (list (format nil "recipient_name=~a" (encode recipient-name))))
                              (when amount
                                (list (format nil "tx_amount=~a" (format-float amount))))
                              (when description
                                (list (format nil "tx_description=~a" (encode description)))))))
      (format nil "monero:~a~@[?~{~a~^&~}~]" address parameters))))

(defun decode-uri (uri)
  "Return an alist containing the information encoded in a payment
URI."
  (with-input-from-string (in uri)
    (flet ((decode (string)
             (with-output-to-string (out)
               (with-input-from-string (in string)
                 (do ((c (read-char in nil) (read-char in nil)))
                     ((null c))
                   (if (char/= c #\%)
                       (write-char c out)
                       (let* ((c1 (read-char in nil))
                              (c2 (read-char in nil))
                              (x1 (when c1
                                    (digit-char-p c1 16)))
                              (x2 (when c2
                                    (digit-char-p c2 16))))
                         (if (and x1 x2)
                             (write-char (code-char (+ (* 16 x1) x2)) out)
                             (error "Invalid URI: ~a." uri))))))))
           (read-until (character)
             (iter (for c next (read-char in nil nil))
                   (until (or (null c) (char= c character)))
                   (collect c result-type 'string)))
           (get-parameter (parameters key)
             (car (geta parameters key :test #'string=))))
      (let ((header (make-string 7)))
        (read-sequence header in)
        (unless (string= header "monero:")
          (error "Invalid URI: ~a." uri))
        (let ((address (read-until #\?))
              (parameters (iter (while (listen in))
                                (collect (read-until #\&) into parameters)
                                (finally (return (mapcar (lambda (string)
                                                           (split-sequence #\= string))
                                                         parameters))))))
          (unless (plusp (length address))
            (error "Invalid URI: ~a." uri))
          (append (list (cons :address address))
                  (let ((payment-id (get-parameter parameters "tx_payment_id")))
                    (when payment-id
                      (list (cons :payment-id (hex-string->bytes payment-id)))))
                  (let ((recipient-name (get-parameter parameters "recipient_name")))
                    (when recipient-name
                      (list (cons :recipient-name (decode recipient-name)))))
                  (let ((amount (get-parameter parameters "tx_amount")))
                    (when amount
                      (list (cons :amount (read-float amount)))))
                  (let ((description (get-parameter parameters "tx_description")))
                    (when description
                      (list (cons :description (decode description)))))))))))
