;;;; This file is part of monero-tools
;;;; Copyright 2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :monero-tools)


(defparameter *randomx-service-host* "127.0.0.1"
  "Location of the randomx-service server.")
(defparameter *randomx-service-port* 39093
  "Port on which the randomx-service server expects connections.")

(defun randomx-service-info (&key (host *randomx-service-host*) (port *randomx-service-port*))
  "Get information about a running randomx-service daemon."
  (let* ((url (format nil "http://~a:~d/info" host port))
         (body (dex:get url)))
    (decode-json-from-string body)))

(defun randomx-service-reseed (seed &key (host *randomx-service-host*) (port *randomx-service-port*))
  "Reinitialize a randomx-service daemon with the provided SEED."
  (check-type seed octet-vector)
  (let ((url (format nil "http://~a:~d/seed" host port))
        (headers '(("Content-Type" . "application/x.randomx+bin"))))
    (dex:post url :headers headers :content seed)
    t))

(defun randomx-service-hash (data &key seed (host *randomx-service-host*) (port *randomx-service-port*))
  "Calculate a RandomX hash of the DATA octet vector. If SEED is provided and
if it is not equal to the seed the randomx-service daemon has been initialized
with, an error is signalled."
  (check-type data octet-vector)
  (check-type seed (or null octet-vector))
  (let* ((url (format nil "http://~a:~d/hash" host port))
         (headers (append '(("Content-Type" . "application/x.randomx+bin")
                            ("Accept" . "application/x.randomx+bin"))
                          (when seed
                            (list (cons "RandomX-Seed"
                                        (bytes->hex-string seed))))))
         (body (dex:post url :headers headers :content data)))
    body))

(defun randomx-service-hash-batch (inputs &key seed (host *randomx-service-host*) (port *randomx-service-port*))
  "Calculate the RandomX hashes of the INPUTS. INPUTS must be a list of at most
256 octet-vectors. If SEED is provided and if it is not equal to the seed the
randomx-service daemon has been initialized with, an error is signalled."
  (check-type inputs list)
  (check-type seed (or null octet-vector))
  (when (> (length inputs) 256)
    (error "Too many inputs. The maximum size is 256."))
  (let* ((url (format nil "http://~a:~d/batch" host port))
         (headers (append '(("Content-Type" . "application/x.randomx.batch+bin")
                            ("Accept" . "application/x.randomx.batch+bin"))
                          (when seed
                            (list (cons "RandomX-Seed"
                                        (bytes->hex-string seed))))))
         (content (with-octet-output-stream (out)
                    (mapc (lambda (data)
                            (let ((length (length data)))
                              (when (> length 127)
                                (error "Too much data."))
                              (write-byte length out)
                              (write-sequence data out)))
                          inputs)))
         (body (dex:post url :headers headers :content content)))
    (iter (repeat (length inputs))
          (for i from 0 by 33)
          (unless (= (aref body i) 32)
            (error "Malformed binary response"))
          (collect (subseq body (1+ i) (+ i 33))))))
