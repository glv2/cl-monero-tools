;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun compute-transaction-hash (transaction-data)
  (multiple-value-bind (prefix prefix-size)
      (read-transaction-prefix transaction-data 0)
    (let* ((version (geta prefix :version))
           (vin (geta prefix :vin))
           (vin-size (length vin))
           (ring-size (length (geta (geta (aref vin 0) :key) :key-offsets)))
           (vout-size (length (geta prefix :vout))))
      (if (= 1 version)
          (let ((signatures-size (nth-value 1 (read-signatures transaction-data
                                                               prefix-size
                                                               (list ring-size vin-size)))))
            (fast-hash (subseq transaction-data 0 (+ prefix-size signatures-size))))
          (let ((base nil)
                (prunable nil))
            (multiple-value-bind (type s0)
                (read-single-byte transaction-data prefix-size)
              (if (eq type +rct-type-null+)
                  (setf base (coerce (vector type) '(simple-array (unsigned-byte 8) (*))))
                  (progn
                    ;; TODO: set base
                    ;; TODO: set prunable
                    (read-single-byte transaction-data (+ prefix-size s0)))))
            (let ((hash-prefix (fast-hash (subseq transaction-data 0 prefix-size)))
                  (hash-base (fast-hash base))
                  (hash-prunable (if prunable
                                     (fast-hash prunable)
                                     (make-array +hash-length+
                                                 :element-type '(unsigned-byte 8)
                                                 :initial-element 0))))
              (bytes->hex-string (fast-hash (concatenate '(simple-array (unsigned-byte 8) (*))
                                                         hash-prefix
                                                         hash-base
                                                         hash-prunable)))))))))

(defun compute-miner-transaction-hash (block-data)
  (let* ((header-size (nth-value 1 (read-block-header block-data 0)))
         (miner-tx-size (nth-value 1 (read-transaction block-data header-size))))
    (compute-transaction-hash (subseq block-data header-size (+ header-size miner-tx-size)))))
