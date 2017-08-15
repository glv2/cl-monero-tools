;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun get-wallet-keys (keys-file password)
  "Get the wallet view keys and spend keys from an encrypted KEYS-FILE."
  (let* ((keys-file-data (read-file-into-byte-vector keys-file))
         (iv (subseq keys-file-data 0 +chacha8-iv-length+)))
    (multiple-value-bind (encrypted-data-length varint-size)
        (deserialize-integer keys-file-data +chacha8-iv-length+)
      (let* ((encrypted-data (subseq keys-file-data
                                     (+ +chacha8-iv-length+ varint-size)
                                     (+ +chacha8-iv-length+ varint-size encrypted-data-length)))
             (key (generate-chacha8-key password))
             (account-json-data (map 'string #'code-char (chacha8 encrypted-data key iv)))
             (account-json (handler-case (decode-json-from-string account-json-data)
                             (t () nil)))
             (key-data (geta account-json :key--data)))
        (unless key-data
          (error "Bad password."))
        (flet ((find-key-field (data key)
                 (let ((l (length key))
                       (i (search key data)))
                   (when i
                     (map 'octet-vector #'char-code (subseq data (+ i 2 l) (+ i 2 l 32)))))))
          (append (let ((v (find-key-field key-data "m_spend_public_key")))
                    (when v (list (cons :public-spend-key v))))
                  (let ((v (find-key-field key-data "m_view_public_key")))
                    (when v (list (cons :public-view-key v))))
                  (let ((v (find-key-field key-data "m_spend_secret_key")))
                    (when v (list (cons :secret-spend-key v))))
                  (let ((v (find-key-field key-data "m_view_secret_key")))
                    (when v (list (cons :secret-view-key v))))))))))

(defparameter *bruteforce-dictionary* nil)
(defparameter *bruteforce-state* nil)
(defparameter *bruteforce-stop* nil)
(defparameter *bruteforce-result* nil)
(defparameter *bruteforce-lock* nil)

(defun bruteforce-wallet-keys (keys-file &key (threads 1) dictionary-file (characters " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~") (minimum-length 1) (maximum-length 8) prefix suffix)
  "Try to find the password and keys of an encrypted KEYS-FILE either
using a DICTIONARY-FILE, or by trying all the passwords composed of
some CHARACTERS, having a length between MINIMUM-LENGTH and
MAXIMUM-LENGTH, starting with PREFIX and ending with SUFFIX. Several
THREADS can be used to go faster."
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
                        (acquire-lock *bruteforce-lock*)
                        (setf password (read-line *bruteforce-dictionary* nil nil))
                        (unless password
                          (setf *bruteforce-stop* t))
                        (release-lock *bruteforce-lock*)
                        (when (plusp (length password))
                          password))))

                  (generate-next-password ()
                    (let (password)
                      (acquire-lock *bruteforce-lock*)
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
                                (setf *bruteforce-state* (make-array (+ len 2) :initial-element 0))))))
                      (release-lock *bruteforce-lock*)
                      (when (plusp (length password))
                        (concatenate 'string prefix password suffix))))

                  (bruteforce ()
                    (loop until *bruteforce-stop* do
                      (let* ((password (if *bruteforce-dictionary*
                                           (read-dictionary-line)
                                           (generate-next-password)))
                             (keys (handler-case (get-wallet-keys keys-file password)
                                     (t () nil))))
                        (when keys
                          (acquire-lock *bruteforce-lock*)
                          (setf *bruteforce-stop* t)
                          (setf *bruteforce-result* (acons :password password keys))
                          (release-lock *bruteforce-lock*))))))
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
