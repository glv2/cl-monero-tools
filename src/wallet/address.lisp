;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defconstant +address-base58-prefix+ 18)
(defconstant +integrated-address-base58-prefix+ 19)
(defconstant +subaddress-base58-prefix+ 42)
(defconstant +stagenet-address-base58-prefix+ 24)
(defconstant +stagenet-integrated-address-base58-prefix+ 25)
(defconstant +stagenet-subaddress-base58-prefix+ 36)
(defconstant +testnet-address-base58-prefix+ 53)
(defconstant +testnet-integrated-address-base58-prefix+ 54)
(defconstant +testnet-subaddress-base58-prefix+ 63)

(defun encode-address (public-spend-key public-view-key &key payment-id subaddress (chain :mainnet))
  "Return the base58 encoded Monero address matching the
PUBLIC-SPEND-KEY and PUBLIC-VIEW-KEY. If a PAYMENT-ID is supplied, an
integrated address is returned. If SUBADDRESS is T, a subaddress is
returned. CHAIN can be :MAINNET, :STAGENET or :TESTNET."
  (when (and payment-id subaddress)
    (error "Integrated subaddress not supported."))
  (macrolet ((concat (&rest sequences)
               `(concatenate 'octet-vector ,@sequences)))
    (let* ((tag (vector (case chain
                          ((:mainnet nil t)
                           (cond (subaddress +subaddress-base58-prefix+)
                                 (payment-id +integrated-address-base58-prefix+)
                                 (t +address-base58-prefix+)))
                          ((:stagenet)
                           (cond (subaddress +testnet-subaddress-base58-prefix+)
                                 (payment-id +testnet-integrated-address-base58-prefix+)
                                 (t +testnet-address-base58-prefix+)))
                          ((:testnet)
                           (cond (subaddress +testnet-subaddress-base58-prefix+)
                                 (payment-id +testnet-integrated-address-base58-prefix+)
                                 (t +testnet-address-base58-prefix+)))
                          (t
                           (error "Address for an unknown chain.")))))
           (data (concat tag public-spend-key public-view-key payment-id))
           (hash (subseq (fast-hash data) 0 +base58-checksum-size+)))
      (base58-encode (concat data hash)))))

(defun decode-address (address)
  "Return an alist containing the components of a Monero ADDRESS."
  (let* ((data (base58-decode address))
         (size (length data))
         (tag (aref data 0))
         (chain (cond
                  ((or (= tag +address-base58-prefix+)
                       (= tag +integrated-address-base58-prefix+)
                       (= tag +subaddress-base58-prefix+))
                   :mainnet)
                  ((or (= tag +stagenet-address-base58-prefix+)
                       (= tag +stagenet-integrated-address-base58-prefix+)
                       (= tag +stagenet-subaddress-base58-prefix+))
                   :stagenet)
                  ((or (= tag +testnet-address-base58-prefix+)
                       (= tag +testnet-integrated-address-base58-prefix+)
                       (= tag +testnet-subaddress-base58-prefix+))
                   :stagenet)
                  (t
                   (error "Address for an unknown chain."))))
         (integrated-address (or (= tag +integrated-address-base58-prefix+)
                                 (= tag +stagenet-integrated-address-base58-prefix+)
                                 (= tag +testnet-integrated-address-base58-prefix+)))
         (subaddress (or (= tag +subaddress-base58-prefix+)
                         (= tag +stagenet-subaddress-base58-prefix+)
                         (= tag +testnet-subaddress-base58-prefix+)))
         (public-spend-key (subseq data 1 33))
         (public-view-key (subseq data 33 65))
         (payment-id (when integrated-address
                       (subseq data 65 (- size +base58-checksum-size+))))
         (hash (subseq data (- size +base58-checksum-size+) size)))
    (let* ((data (subseq data 0 (- size +base58-checksum-size+)))
           (computed-hash (subseq (fast-hash data) 0 +base58-checksum-size+)))
      (unless (equalp hash computed-hash)
        (error "Bad checksum.")))
    (append (list (cons :public-spend-key public-spend-key)
                  (cons :public-view-key public-view-key)
                  (cons :chain chain))
            (when integrated-address
              (list (cons :payment-id payment-id)))
            (when subaddress
              (list (cons :subaddress subaddress))))))

(defun public-keys->address (public-spend-key public-view-key &key subaddress (chain :mainnet))
  "Get the Monero address matching the PUBLIC-SPEND-KEY and
PUBLIC-VIEW-KEY. If SUBADDRESS is T, a subaddress is returned. CHAIN
can be :MAINNET, :STAGENET or :TESTNET."
  (encode-address public-spend-key public-view-key :subaddress subaddress :chain chain))

(defun public-keys->subaddress (public-spend-key secret-view-key major-index minor-index &key (chain :mainnet))
  "Get the Monero subaddress matching the PUBLIC-SPEND-KEY,
SECRET-VIEW-KEY, MAJOR-INDEX and MINOR-INDEX. CHAIN can
be :MAINNET, :STAGENET or :TESTNET."
  (let* ((public-spend-subkey (derive-public-spend-subkey secret-view-key
                                                          public-spend-key
                                                          major-index
                                                          minor-index))
         (public-view-subkey (public-spend-subkey->public-view-subkey secret-view-key
                                                                      public-spend-subkey)))
    (public-keys->address public-spend-subkey public-view-subkey :subaddress t :chain chain)))

(defun secret-spend-key->address (secret-spend-key &key (chain :mainnet))
  "Get the Monero address matching the SECRET-SPEND-KEY. CHAIN can
be :MAINNET, :STAGENET or :TESTNET."
  (let* ((keys (recover-keys secret-spend-key))
         (public-spend-key (geta keys :public-spend-key))
         (public-view-key (geta keys :public-view-key)))
    (public-keys->address public-spend-key public-view-key :chain chain)))

(defun secret-spend-key->subaddress (secret-spend-key major-index minor-index &key (chain :mainnet))
  "Get the Monero subaddress matching the SECRET-SPEND-KEY,
MAJOR-INDEX and MINOR-INDEX. CHAIN can be :MAINNET, :STAGENET
or :TESTNET."
  (let* ((keys (recover-keys secret-spend-key))
         (public-spend-key (geta keys :public-spend-key))
         (secret-view-key (geta keys :secret-view-key)))
    (public-keys->subaddress public-spend-key
                             secret-view-key
                             major-index
                             minor-index
                             :chain chain)))

(defun make-integrated-address (address payment-id)
  "Return an integrated address made from a Monero ADDRESS and a PAYMENT-ID."
  (let* ((address-info (decode-address address))
         (public-spend-key (geta address-info :public-spend-key))
         (public-view-key (geta address-info :public-view-key))
         (chain (geta address-info :chain)))
    (when (geta address-info :subaddress)
      (error "Integrated subaddress not supported."))
    (when (geta address-info :payment-id)
      (warn "~a is already an integrated address." address))
    (unless (= (length payment-id) 8)
      (error "Bad payment-id length."))
    (encode-address public-spend-key public-view-key :payment-id payment-id :chain chain)))
