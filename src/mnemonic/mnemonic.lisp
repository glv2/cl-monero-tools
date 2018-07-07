;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defconstant +secret-key-length+ 32)
(defconstant +seed-length+ 24)
(defparameter *word-lists* (make-hash-table))

(defclass word-list ()
  ((language :initarg :language :reader language)
   (prefix-length :initarg :prefix-length :reader prefix-length)
   (words :initarg :words :reader get-word-list)))

(defun add-word-list (language prefix-length words)
  "Add a new LANGUAGE to *WORD-LISTS*, where the PREFIX-LENGTH first
characters of the elements of WORDS are never the same."
  (setf (gethash language *word-lists*)
        (make-instance 'word-list
                       :language language
                       :prefix-length prefix-length
                       :words words)))

(defun available-mnemonic-seed-languages ()
  "Return the list of available languages for mnemonix=c seeds."
  (sort (hash-table-keys *word-lists*) #'string<))

(defun bytes->seed (bytes word-list)
  "Convert the BYTES of a secret key to a mnemonic seed using words
from WORD-LIST."
  (if (/= (length bytes) +secret-key-length+)
      (error "Bad data length.")
      (do ((word-list-length (length word-list))
           (seed (make-array (* +secret-key-length+ 3/4)))
           (i 0 (+ i 4))
           (j 0 (+ j 3)))
          ((>= i +secret-key-length+) seed)
        ;; 8 base 16 digits -> 3 base 1626 digits
        (let* ((val (bytes->integer bytes :start i :end (+ i 4)))
               (q1 (floor val word-list-length))
               (q2 (floor q1 word-list-length))
               (w1 (mod val word-list-length))
               (w2 (mod (+ q1 w1) word-list-length))
               (w3 (mod (+ q2 w2) word-list-length)))
          (setf (aref seed j) (elt word-list w1)
                (aref seed (+ j 1)) (elt word-list w2)
                (aref seed (+ j 2)) (elt word-list w3))))))

(defun seed->bytes (seed word-list prefix-length)
  "Convert a mnemonic seed made of words from WORD-LIST to a secret
key made of bytes."
  (if (/= (length seed) +seed-length+)
      (error "Bad seed length.")
      (let ((word-list-length (length word-list))
            (prefix= (lambda (word1 word2)
                       (string= (subseq word1 0 (min prefix-length (length word1)))
                                (subseq word2 0 (min prefix-length (length word2)))))))
        (do ((bytes (make-array (* +seed-length+ 4/3) :element-type '(unsigned-byte 8)))
             (i 0 (+ i 3))
             (j 0 (+ j 4)))
            ((>= i +seed-length+) bytes)
          ;; 3 base 1626 digits -> 8 base 16 digits
          (let* ((w1 (position (aref seed i) word-list :test prefix=))
                 (w2 (position (aref seed (+ i 1)) word-list :test prefix=))
                 (w3 (position (aref seed (+ i 2)) word-list :test prefix=))
                 (val (+ w1
                         (* word-list-length
                            (mod (- w2 w1) word-list-length))
                         (* word-list-length
                            word-list-length
                            (mod (- w3 w2) word-list-length)))))
            (if (/= (mod val word-list-length) w1)
                (error "Bad seed.")
                (integer->bytes val :buffer bytes :start j)))))))

(defun seed-checksum (seed prefix-length)
  "Compute the checksum word of a mnemonic SEED."
  (if (/= (length seed) +seed-length+)
      (error "Bad seed length.")
      (let* ((crc32 (ironclad:make-digest :crc32))
             (checksum (dotimes (i +seed-length+ (ironclad:produce-digest crc32))
                         (let* ((word (aref seed i))
                                (end (min prefix-length (length word)))
                                (bytes (utf-8-string->bytes (subseq word 0 end))))
                           (ironclad:update-digest crc32 bytes))))
             (index (mod (bytes->integer checksum :big-endian t) +seed-length+)))
        (aref seed index))))

(defun find-seed-language (seed)
  "Find which language the words from the SEED are from."
  (loop for language in (available-mnemonic-seed-languages)
        when (loop with word-list = (get-word-list (gethash language *word-lists*))
                   for word across seed
                   always (position word word-list :test #'string-equal))
          do (return language)))

(defun secret-key->mnemonic-seed (secret-key language)
  "Convert a SECRET-KEY to a mnemonic seed."
  (if (/= (length secret-key) +secret-key-length+)
      (error "Bad secret key length.")
      (let* ((language-info (or (gethash language *word-lists*)
                                (error "Language ~a not supported." language)))
             (word-list (get-word-list language-info))
             (prefix-length (prefix-length language-info))
             (seed (bytes->seed secret-key word-list))
             (checksum (seed-checksum seed prefix-length)))
        (reduce (lambda (x y)
                  (concatenate 'string x " " y))
                (concatenate 'vector seed (vector checksum))))))

(defun mnemonic-seed->secret-key (mnemonic-seed &optional language)
  "Convert a MNEMONIC-SEED to a secret-key."
  (let ((words (split-sequence #\space mnemonic-seed :remove-empty-subseqs t)))
    (if (/= (length words) (1+ +seed-length+))
        (error "Bad number of words in mnemonic seed.")
        (let* ((seed (apply #'vector (butlast words)))
               (language (or language (find-seed-language seed)))
               (language-info (or (gethash language *word-lists*)
                                  (error "Language ~a not supported." language)))
               (word-list (get-word-list language-info))
               (prefix-length (prefix-length language-info))
               (checksum (car (last words))))
          (if (string/= (seed-checksum seed prefix-length) checksum)
              (error "Checksum verification failed.")
              (seed->bytes seed word-list prefix-length))))))

(defun encrypt-mnemonic-seed (mnemonic-seed password &optional language)
  "Encrypt a MNEMONIC-SEED with a PASSWORD and return the result as an encrypted
mnemonic seed which looks just like a not encrypted mnemonic seed. This
encryption is similar to a one-time pad, therefore the same password should not
be used for different mnemonic seeds."
  (let* ((language (or language
                       (let ((words (split-sequence #\space mnemonic-seed :remove-empty-subseqs t)))
                         (find-seed-language (apply #'vector words)))))
         (plaintext (mnemonic-seed->secret-key mnemonic-seed language))
         (m (bytes->integer plaintext))
         (encryption-key (slow-hash (utf-8-string->bytes password)))
         (k (bytes->integer encryption-key))
         (c (mod (+ m k) +l+))
         (ciphertext (integer->bytes c :size +key-length+)))
    (secret-key->mnemonic-seed ciphertext language)))

(defun decrypt-mnemonic-seed (mnemonic-seed password &optional language)
  "Decrypt an encrypted MNEMONIC-SEED with a PASSWORD."
  (let* ((language (or language
                       (let ((words (split-sequence #\space mnemonic-seed :remove-empty-subseqs t)))
                         (find-seed-language (apply #'vector words)))))
         (ciphertext (mnemonic-seed->secret-key mnemonic-seed language))
         (c (bytes->integer ciphertext))
         (decryption-key (slow-hash (utf-8-string->bytes password)))
         (k (bytes->integer decryption-key))
         (m (mod (- c k) +l+))
         (plaintext (integer->bytes m :size +key-length+)))
    (secret-key->mnemonic-seed plaintext language)))
