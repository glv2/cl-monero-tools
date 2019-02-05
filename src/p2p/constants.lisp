;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-p2p)


;;; Levin protocol

(defconstant +levin-ok+ 0)
(define-constant +levin-signature+
    (integer->bytes #x0101010101012101 :size 8)
  :test #'equalp)
(defconstant +levin-packet-request+ 1)
(defconstant +levin-packet-response+ 2)
(defconstant +levin-max-packet-size+ 100000000)
(defconstant +levin-protocol-version-0+ 0)
(defconstant +levin-protocol-version-1+ 1)
(defconstant +levin-ok+ 0)
(defconstant +levin-error-connection+ (logand -1 #xffffffff))
(defconstant +levin-error-connection-not-found+ (logand -2 #xffffffff))
(defconstant +levin-error-connection-destroyed+ (logand -3 #xffffffff))
(defconstant +levin-error-connection-timedout+ (logand -4 #xffffffff))
(defconstant +levin-error-connection-no-duplex-protocol+ (logand -5 #xffffffff))
(defconstant +levin-error-connection-handler-not-defined+ (logand -6 #xffffffff))
(defconstant +levin-error-format+ (logand -7 #xffffffff))


;;; Peer-to-peer

(define-constant +p2p-network-id-mainnet+
    #(18 48 241 113 97 4 65 97 23 49 0 130 22 161 161 16)
  :test #'equalp)
(define-constant +p2p-network-id-testnet+
    #(18 48 241 113 97 4 65 97 23 49 0 130 22 161 161 17)
  :test #'equalp)
(define-constant +p2p-network-id-stagenet+
    #(18 48 241 113 97 4 65 97 23 49 0 130 22 161 161 18)
  :test #'equalp)
(defconstant +p2p-support-flag-fluffy-blocks+ 1)
(defconstant +p2p-support-flags+ (logior +p2p-support-flag-fluffy-blocks+))
(defconstant +p2p-command-handshake+ 1001)
(defconstant +p2p-command-timed-sync+ 1002)
(defconstant +p2p-command-ping+ 1003)
(defconstant +p2p-command-request-stat-info+ 1004)
(defconstant +p2p-command-request-network-state+ 1005)
(defconstant +p2p-command-request-peer-id+ 1006)
(defconstant +p2p-command-request-support-flags+ 1007)


;;; Seed nodes

(define-constant +mainnet-seed-nodes+
    '(("5.9.100.248" 18080)
      ("107.152.130.98" 18080)
      ("161.67.132.39" 18080)
      ("163.172.182.165" 18080)
      ("195.154.123.123" 18080)
      ("198.74.231.92" 18080)
      ("212.83.172.165" 18080)
      ("212.83.175.67" 18080))
  :test #'equalp)
(define-constant +stagenet-seed-nodes+
    '(("162.210.173.150" 38080)
      ("162.210.173.151" 38080))
  :test #'equalp)
(define-constant +testnet-seed-nodes+
    '(("5.9.100.248" 28080)
      ("163.172.182.165" 28080)
      ("195.154.123.123" 28080)
      ("212.83.172.165" 28080)
      ("212.83.175.67" 28080))
  :test #'equalp)
