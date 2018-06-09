;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-rpc)


(defparameter *mine-refresh-delay* 10)

(defun mine-block (address reserve-size &key (threads 1) (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  "Mine a block with several THREADS for an ADDRESS using RESERVE-SIZE
bytes of extra nonce."
  (when monero-tools::*mine-lock*
    (error "The mining process is already running."))
  (unwind-protect
       (let ((thread-handles (make-array threads :initial-element nil)))
         (setf monero-tools::*mine-lock* (make-lock))
         (labels ((stop-threads ()
                    (when (aref thread-handles 0)
                      (with-lock-held (monero-tools::*mine-lock*)
                        (setf monero-tools::*mine-stop* t))
                      (dotimes (i threads)
                        (join-thread (aref thread-handles i))
                        (setf (aref thread-handles i) nil))
                      (with-lock-held (monero-tools::*mine-lock*)
                        (setf monero-tools::*mine-stop* nil))))

                  (update-miners (template reserve-size reserve-offset difficulty)
                    (stop-threads)
                    (let ((miner (lambda ()
                                   (miner template reserve-size reserve-offset difficulty))))
                      (dotimes (i threads)
                        (setf (aref thread-handles i) (make-thread miner))))))
           (let ((data (do ()
                           (monero-tools::*mine-result* monero-tools::*mine-result*)
                         (let* ((info (get-block-template address
                                                          reserve-size
                                                          :host host
                                                          :port port
                                                          :user user
                                                          :password password))
                                (template (hex-string->bytes (geta info :blocktemplate-blob)))
                                (difficulty (geta info :difficulty))
                                (reserve-offset (geta info :reserved-offset))
                                (height (geta info :height)))
                           (format t "height=~a difficulty=~d~%" height difficulty)
                           (update-miners template reserve-size reserve-offset difficulty)
                           (sleep *mine-refresh-delay*)))))
             (stop-threads)
             (submit-block (bytes->hex-string data) :host host :port port :user user :password password))))
    (setf monero-tools::*mine-stop* nil)
    (setf monero-tools::*mine-lock* nil)
    (setf monero-tools::*mine-result* nil)))
