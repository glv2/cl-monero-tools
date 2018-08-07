;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(define-constant +multisig-info-header+ "MultisigV1" :test #'string=)
(define-constant +multisig-extra-info-header+ "MultisigxV1" :test #'string=)

(defun make-multisig-info (secret-view-key secret-spend-key)
  "Return the info of a multi-signature wallet encoded as a string."
  (let* ((blinded-secret-view-key (compute-multisig-blinded-secret secret-view-key))
         (secret-signer-key (compute-multisig-blinded-secret secret-spend-key))
         (public-signer-key (secret-key->public-key secret-signer-key))
         (data (concatenate 'octet-vector blinded-secret-view-key public-signer-key))
         (hash (fast-hash data))
         (signature (generate-signature hash secret-signer-key))
         (data (concatenate 'octet-vector data signature)))
    (concatenate 'string +multisig-info-header+ (base58-encode data))))

(defun decode-multisig-info (multisig-info)
  "Return an alist containing the components of the MULTISIG-INFO string."
  (let ((header-length (length +multisig-info-header+))
        (encoded-data-length (base58-encoded-length (* 4 +key-length+))))
    (if (not (and (= (length multisig-info) (+ header-length encoded-data-length))
                  (string= multisig-info +multisig-info-header+ :end1 header-length)))
        (error "Invalid multisig info.")
        (let* ((data (base58-decode (subseq multisig-info header-length)))
               (secret-view-key (subseq data 0 +key-length+))
               (public-signer-key (subseq data +key-length+ (* 2 +key-length+)))
               (hash (fast-hash (subseq data 0 (* 2 +key-length+))))
               (signature (subseq data (* 2 +key-length+))))
          (if (not (valid-signature-p hash public-signer-key signature))
              (error "Invalid multisig info.")
              (list (cons :secret-view-key secret-view-key)
                    (cons :public-signer-key public-signer-key)))))))

(defun make-multisig-extra-info (multisig-keys)
  "Return the extra info of a multi-signature wallet encoded as a string."
  (let* ((secret-signer-key (compute-multisig-secret-spend-key multisig-keys))
         (public-signer-key (secret-key->public-key secret-signer-key))
         (multisig-public-keys (compute-multisig-public-keys multisig-keys))
         (data (concatenate 'octet-vector
                            public-signer-key
                            (with-octet-output-stream (result)
                              (map nil (lambda (x) (write-sequence x result)) multisig-public-keys))))
         (hash (fast-hash data))
         (signature (generate-signature hash secret-signer-key))
         (data (concatenate 'octet-vector data signature)))
    (concatenate 'string +multisig-extra-info-header+ (base58-encode data))))

(defun decode-multisig-extra-info (multisig-extra-info)
  "Return an alist containing the components of the MULTISIG-EXTRA-INFO string."
  (let ((header-length (length +multisig-extra-info-header+))
        (min-encoded-data-length (base58-encoded-length (* 4 +key-length+))))
    (if (not (and (>= (length multisig-extra-info) (+ header-length min-encoded-data-length))
                  (string= multisig-extra-info +multisig-extra-info-header+ :end1 header-length)))
        (error "Invalid multisig extra info.")
        (let* ((data (base58-decode (subseq multisig-extra-info header-length)))
               (public-signer-key (subseq data 0 +key-length+))
               (keys-data-length (- (length data) (* 3 +key-length+)))
               (multisig-public-keys-data (subseq data +key-length+ (+ +key-length+ keys-data-length)))
               (hash (fast-hash (subseq data 0 (+ +key-length+ keys-data-length))))
               (signature (subseq data (+ +key-length+ keys-data-length))))
          (if (not (and (zerop (mod keys-data-length +key-length+))
                        (valid-signature-p hash public-signer-key signature)))
              (error "Invalid multisig extra info.")
              (let ((keys (loop for i from 0 below keys-data-length by +key-length+
                                collect (subseq multisig-public-keys-data i (+ i +key-length+)))))
                (list (cons :public-signer-key public-signer-key)
                      (cons :multisig-public-keys (coerce keys 'vector)))))))))

(defun make-multisig-seed (threshold total secret-spend-key public-spend-key secret-view-key public-view-key multisig-keys multisig-signers)
  "Encode the all the info about a multisig wallet as a hex string."
  (with-octet-output-stream (result)
    (write-sequence (integer->bytes threshold :size 4) result)
    (write-sequence (integer->bytes total :size 4) result)
    (write-sequence secret-spend-key result)
    (write-sequence public-spend-key result)
    (write-sequence secret-view-key result)
    (write-sequence public-view-key result)
    (map nil (lambda (x) (write-sequence x result)) multisig-keys)
    (map nil (lambda (x) (write-sequence x result)) multisig-signers)))

(defun decode-multisig-seed (multisig-seed)
  "Return an alist containing the components of the MULTISIG-SEED hex string."
  (let ((seed (hex-string->bytes multisig-seed)))
    (if (< (length seed) 8)
        (error "Invalid multisig seed.")
        (let* ((threshold (bytes->integer seed :end 4))
               (total (bytes->integer seed :start 4 :end 8))
               (multisig-keys-length (if (= threshold total) 1 (1- total))))
          (if (not (and (<= 2 threshold total)
                        (= (length seed) (+ 8 (* +key-length+ (+ 4 multisig-keys-length total))))))
              (error "Invalid multisig seed.")
              (let* ((secret-spend-key (subseq seed 8 (+ 8 +key-length+)))
                     (offset (+ 8 +key-length+))
                     (public-spend-key (subseq seed offset (+ offset +key-length+)))
                     (offset (+ offset +key-length+))
                     (secret-view-key (subseq seed offset (+ offset +key-length+)))
                     (offset (+ offset +key-length+))
                     (public-view-key (subseq seed offset (+ offset +key-length+)))
                     (offset (+ offset +key-length+))
                     (offset1 (+ offset (* multisig-keys-length +key-length+)))
                     (multisig-keys (loop for i from offset below offset1 by +key-length+
                                          collect (subseq seed i (+ i +key-length+))))
                     (multisig-keys (coerce multisig-keys 'vector))
                     (signers (loop for i from offset1 below (length seed) by +key-length+
                                    collect (subseq seed i (+ i +key-length+))))
                     (signers (coerce signers 'vector)))
                (if (not (and (equalp public-view-key (secret-key->public-key secret-view-key))
                              (find (secret-key->public-key secret-spend-key) signers :test #'equalp)
                              (equalp secret-spend-key
                                      (compute-multisig-secret-spend-key multisig-keys))))
                    (error "Invalid multisig seed.")
                    (list (cons :threshold threshold)
                          (cons :total total)
                          (cons :secret-spend-key secret-spend-key)
                          (cons :public-spend-key public-spend-key)
                          (cons :secret-view-key secret-view-key)
                          (cons :public-view-key public-view-key)
                          (cons :multisig-keys multisig-keys)
                          (cons :signers signers)))))))))

#|
n/n multisig wallet
-------------------
(let* ((secret-spend-key (generate-secret-key))
       (secret-view-key (secret-spend-key->secret-view-key secret-spend-key))
       (multisig-info (make-multisig-info secret-view-key secret-spend-key))
       (decoded-multisig-info (decode-multisig-info multisig-info))
       (blinded-secret-view-key (geta decoded-multisig-info :secret-view-key))
       (public-signer-key (geta decoded-multisig-info :public-signer-key)))
  (send-to-other-multisig-signers multisig-info)
  (let* ((other-signers-multisig-info (receive-info-from-other-multisig-signers))
         (other-signers-multisig-info (map 'vector
                                           #'decode-multisig-info
                                           other-signers-multisig-info))
         (other-secret-view-keys (map 'vector
                                      (lambda (multisig-info)
                                        (geta multisig-info :secret-view-key))
                                      other-signers-multisig-info))
         (other-public-signer-keys (map 'vector
                                        (lambda (multisig-info)
                                          (geta multisig-info :public-signer-key))
                                        other-signers-multisig-info))
         (secret-view-keys (concatenate 'vector
                                        (vector blinded-secret-view-key)
                                        other-secret-view-keys))
         (public-signer-keys (concatenate 'vector
                                          (vector public-signer-key)
                                          other-public-signer-keys))
         (multisig-keys (compute-multisig-keys-n/n secret-spend-key))
         (multisig-secret-view-key (compute-multisig-secret-view-key secret-view-keys))
         (multisig-public-view-key (secret-key->public-key multisig-secret-view-key))
         (multisig-secret-spend-key (compute-multisig-secret-spend-key multisig-keys))
         (multisig-public-spend-key (compute-multisig-public-spend-key public-signer-keys)))
    (make-multisig-seed n
                        n
                        multisig-secret-spend-key
                        multisig-public-spend-key
                        multisig-secret-view-key
                        multisig-public-view-key
                        multisig-keys
                        public-signer-keys)))

m/n multisig wallet
-------------------
(let* ((secret-spend-key (generate-secret-key))
       (secret-view-key (secret-spend-key->secret-view-key secret-spend-key))
       (multisig-info (make-multisig-info secret-view-key secret-spend-key))
       (decoded-multisig-info (decode-multisig-info multisig-info))
       (blinded-secret-view-key (geta decoded-multisig-info :secret-view-key))
       (public-signer-key (geta decoded-multisig-info :public-signer-key)))
  (send-to-other-multisig-signers multisig-info)
  (let* ((other-signers-multisig-info (receive-info-from-other-multisig-signers))
         (other-signers-multisig-info (map 'vector
                                           #'decode-multisig-info
                                           other-signers-multisig-info))
         (other-secret-view-keys (map 'vector
                                      (lambda (multisig-info)
                                        (geta multisig-info :secret-view-key))
                                      other-signers-multisig-info))
         (other-public-signer-keys (map 'vector
                                        (lambda (multisig-info)
                                          (geta multisig-info :public-signer-key))
                                        other-signers-multisig-info))
         (multisig-keys (compute-multisig-keys-m/n other-public-signer-keys))
         (multisig-extra-info (make-multisig-extra-info multisig-keys))
         (public-signer-key (geta multisig-extra-info :public-signer-key))
         (multisig-public-keys (geta multisig-extra-info :multisig-public-keys)))
    (send-to-other-multisig-signers multisig-extra-info)
    (let* ((other-signers-multisig-extra-info (receive-extra-info-from-other-multisig-signers))
           (other-signers-multisig-extra-info (map 'vector
                                                   #'decode-multisig-extra-info
                                                   other-signers-multisig-extra-info))
           (other-public-signer-keys (map 'vector
                                          (lambda (multisig-extra-info)
                                            (geta multisig-extra-info :public-signer-key))
                                          other-signers-multisig-extra-info))
           (other-multisig-public-keys (map 'vector
                                            (lambda (multisig-extra-info)
                                              (geta multisig-extra-info :multisig-public-keys))
                                            other-signers-multisig-extra-info))
           (secret-view-keys (concatenate 'vector
                                          (vector blinded-secret-view-key)
                                          other-secret-view-keys))
           (public-signer-keys (concatenate 'vector
                                            (vector public-signer-key)
                                            other-public-signer-keys))
           (multisig-public-keys (reduce (lambda (x y)
                                           (remove-duplicates (concatenate 'vector x y)
                                                              :test (complement #'mismatch)))
                                         other-multisig-public-keys
                                         :initial-value multisig-public-keys))
           (multisig-secret-view-key (compute-multisig-secret-view-key secret-view-keys))
           (multisig-public-view-key (secret-key->public-key multisig-secret-view-key))
           (multisig-secret-spend-key (compute-multisig-secret-spend-key multisig-keys))
           (multisig-public-spend-key (compute-multisig-public-spend-key multisig-public-keys)))
      (make-multisig-seed m
                          n
                          multisig-secret-spend-key
                          multisig-public-spend-key
                          multisig-secret-view-key
                          multisig-public-view-key
                          multisig-keys
                          public-signer-keys))))
|#
