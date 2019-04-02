;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun decrypt-wallet-keys (keys-file password &key chacha8)
  "Get the info from an encrypted KEYS-FILE. Set CHACHA8 to T if the wallet was
encrypted with chacha8 instead of chacha20."
  (let* ((keys-file-data (read-file-into-byte-vector keys-file))
         (iv (subseq keys-file-data 0 +chacha-iv-length+)))
    (multiple-value-bind (encrypted-data-length varint-size)
        (deserialize-integer keys-file-data +chacha-iv-length+)
      (let* ((start (+ +chacha-iv-length+ varint-size))
             (end (+ start encrypted-data-length))
             (encrypted-data (subseq keys-file-data start end))
             (key (generate-chacha-key password))
             (json-string (bytes->string (if chacha8
                                             (chacha8 encrypted-data key iv)
                                             (chacha20 encrypted-data key iv))))
             (info (handler-case
                       (decode-json-from-string json-string)
                     (t ()
                       (error "Bad password."))))
             (key-data (string->bytes (geta info :key-data)))
             (keys (deserialize-from-binary-storage key-data 0))
             (m-keys (geta keys :m-keys))
             (m-account-address (geta m-keys :m-account-address)))
        (flet ((convert-value (assoc key)
                 (let ((v (geta assoc key)))
                   (when v
                     (setf (geta assoc key) (string->bytes v))))))
          (convert-value m-account-address :m-spend-public-key)
          (convert-value m-account-address :m-view-public-key)
          (convert-value m-keys :m-encryption-iv)
          (convert-value m-keys :m-spend-secret-key)
          (convert-value m-keys :m-view-secret-key))
        (setf (geta info :key-data) keys)
        info))))

(defun get-wallet-keys (keys-file password &key chacha8)
  "Get the wallet view keys and spend keys from an encrypted KEYS-FILE. Set
CHACHA8 to T if the wallet was encrypted with chacha8 instead of chacha20."
  (let* ((info (decrypt-wallet-keys keys-file password :chacha8 chacha8))
         (keys (geta (geta info :key-data) :m-keys))
         (account-address (geta keys :m-account-address)))
    (flet ((get-value (keyword assoc key)
             (let ((v (geta assoc key)))
               (when v
                 (list (cons keyword v))))))
      (append (get-value :public-spend-key account-address :m-spend-public-key)
              (get-value :public-view-key account-address :m-view-public-key)
              (get-value :secret-spend-key keys :m-spend-secret-key)
              (get-value :secret-view-key keys :m-view-secret-key)))))

(defparameter *bruteforce-dictionary* nil)
(defparameter *bruteforce-state* nil)
(defparameter *bruteforce-stop* nil)
(defparameter *bruteforce-result* nil)
(defparameter *bruteforce-lock* nil)

(defun bruteforce-wallet-keys (keys-file &key (threads 1) dictionary-file (characters " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~") (minimum-length 1) (maximum-length 8) prefix suffix chacha8)
  "Try to find the password and keys of an encrypted KEYS-FILE either
using a DICTIONARY-FILE, or by trying all the passwords composed of
some CHARACTERS, having a length between MINIMUM-LENGTH and
MAXIMUM-LENGTH, starting with PREFIX and ending with SUFFIX. Several
THREADS can be used to go faster. Set CHACHA8 to T if the wallet was
encrypted with chacha8 instead of chacha20."
  (when *bruteforce-lock*
    (error "bruteforce-wallet-keys is already running."))
  (unwind-protect
       (progn
         (setf *bruteforce-lock* (make-lock))
         (if dictionary-file
             (setf *bruteforce-dictionary* #-ccl (open dictionary-file)
                                           #+ccl (open dictionary-file :sharing :external))
             (setf *bruteforce-state* (make-array (1+ (- minimum-length (length prefix) (length suffix)))
                                                  :initial-element 0)))
         (labels ((read-dictionary-line ()
                    (when *bruteforce-dictionary*
                      (let (password)
                        (with-lock-held (*bruteforce-lock*)
                          (setf password (read-line *bruteforce-dictionary* nil nil))
                          (unless password
                            (setf *bruteforce-stop* t)))
                        (when (plusp (length password))
                          password))))

                  (generate-next-password ()
                    (let (password)
                      (with-lock-held (*bruteforce-lock*)
                        (let ((len (1- (length *bruteforce-state*))))
                          (if (> len (- maximum-length (length prefix) (length suffix)))
                              (setf *bruteforce-stop* t)
                              (let ((charset-len (length characters)))
                                (setf password (loop for i from 0 below len
                                                     collect (aref characters (aref *bruteforce-state* (- len 1 i)))))
                                ;; Prepare next password
                                (incf (aref *bruteforce-state* 0))
                                (when (= (aref *bruteforce-state* 0) charset-len)
                                  (setf (aref *bruteforce-state* 0) 0))
                                (loop with i = 0
                                      while (and (< i len) (zerop (aref *bruteforce-state* i)))
                                      do (progn
                                           (incf i)
                                           (incf (aref *bruteforce-state* i))
                                           (when (= (aref *bruteforce-state* i) charset-len)
                                             (setf (aref *bruteforce-state* i) 0))))
                                (when (plusp (aref *bruteforce-state* len))
                                  (setf *bruteforce-state* (make-array (+ len 2) :initial-element 0)))))))
                      (when (plusp (length password))
                        (concatenate 'string prefix password suffix))))

                  (bruteforce ()
                    (loop until *bruteforce-stop* do
                      (let* ((password (if *bruteforce-dictionary*
                                           (read-dictionary-line)
                                           (generate-next-password)))
                             (keys (handler-case (get-wallet-keys keys-file password :chacha8 chacha8)
                                     (t () nil))))
                        (when keys
                          (with-lock-held (*bruteforce-lock*)
                            (setf *bruteforce-stop* t)
                            (setf *bruteforce-result* (acons :password password keys))))))))
           (let ((thread-handles (make-array threads)))
             (dotimes (i threads)
               (setf (aref thread-handles i) (make-thread #'bruteforce)))
             (dotimes (i threads)
               (join-thread (aref thread-handles i))))
           *bruteforce-result*))
    (when *bruteforce-dictionary*
      (close *bruteforce-dictionary*)
      (setf *bruteforce-dictionary* nil))
    (setf *bruteforce-state* nil)
    (setf *bruteforce-stop* nil)
    (setf *bruteforce-lock* nil)
    (setf *bruteforce-result* nil)))

(defun decrypt-wallet-cache (cache-file password &optional keys-file chacha8)
  (let* ((keys-file (or keys-file (concatenate 'string cache-file ".keys")))
         (wallet-keys (get-wallet-keys keys-file password :chacha8 chacha8))
         (secret-view-key (geta wallet-keys :secret-view-key))
         (secret-spend-key (geta wallet-keys :secret-spend-key))
         (key (generate-chacha-key-from-secret-keys secret-view-key secret-spend-key))
         (cache-file-data (read-file-into-byte-vector cache-file))
         (iv (subseq cache-file-data 0 +chacha-iv-length+)))
    (multiple-value-bind (encrypted-data-length varint-size)
        (deserialize-integer cache-file-data +chacha-iv-length+)
      (let* ((encrypted-data (subseq cache-file-data
                                     (+ +chacha-iv-length+ varint-size)
                                     (+ +chacha-iv-length+ varint-size encrypted-data-length)))
             (data (if chacha8
                       (chacha8 encrypted-data key iv)
                       (chacha20 encrypted-data key iv))))
        ;; TODO: deserialize data
        data))))
