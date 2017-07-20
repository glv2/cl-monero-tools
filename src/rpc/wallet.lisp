;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun wallet-get-balance (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (json-rpc "getbalance"
            :host host
            :port port
            :user user
            :password password))

(defun wallet-get-block-height (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((answer (json-rpc "getheight"
                          :host host
                          :port port
                          :user user
                          :password password)))
    (geta answer :height)))

(defun wallet-get-transfers (&key (in t) (out t) (pending t) (failed t) (pool t) (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((parameters (append (when pool '(:pool . t))
                            (when failed '(:failed . t))
                            (when pending '(:pending . t))
                            (when out '(:out . t))
                            (when in '(:in . t)))))
    (json-rpc "get_transfers"
              :parameters parameters
              :host host
              :port port
              :user user
              :password password)))
