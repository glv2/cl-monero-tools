;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun get-address-from-wallet (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((answer (json-rpc "getaddress"
                          :host host
                          :port port
                          :user user
                          :password password)))
    (geta answer :address)))

(defun get-balance-from-wallet (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((answer (json-rpc "getbalance"
                           :host host
                           :port port
                           :user user
                           :password password))
         (balance (geta answer :balance))
         (unlocked-balance (geta answer :unlocked--balance)))
    (append (when balance
              (acons :balance (/ balance +monero-unit+) '()))
            (when unlocked-balance
              (acons :unlocked-balance (/ unlocked-balance +monero-unit+) '())))))

(defun get-block-height-from-wallet (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((answer (json-rpc "getheight"
                          :host host
                          :port port
                          :user user
                          :password password)))
    (geta answer :height)))

(defun get-transfers-from-wallet (&key (in t) (out t) (pending t) (failed t) (pool t) (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((parameters (append (when in '((:in . t)))
                             (when out '((:out . t)))
                             (when pending '((:pending . t)))
                             (when failed '((:failed . t)))
                             (when pool '((:pool . t)))))
         (answer (json-rpc "get_transfers"
                           :parameters parameters
                           :host host
                           :port port
                           :user user
                           :password password))
         (transactions (loop for x in answer
                             append (cdr x))))
    (dolist (transaction transactions)
      (setf (geta transaction :amount) (/ (geta transaction :amount) +monero-unit+)
            (geta transaction :fee) (/ (geta transaction :fee) +monero-unit+)))
    (sort transactions (lambda (x y) (< (geta x :timestamp) (geta y :timestamp))))))
