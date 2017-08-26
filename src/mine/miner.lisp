;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defparameter *mine-stop* nil)
(defparameter *mine-result* nil)
(defparameter *mine-lock* nil)
(defparameter *mine-refresh-delay* 10)

(defun miner (block-template-data reserve-size reserve-offset difficulty)
  "Given a block template represented by a BLOCK-TEMPLATE-DATA byte
vector containing a reserved space of RESERVE-SIZE bytes starting at
RESERVE-OFFSET in BLOCK-TEMPLATE-DATA in which an extra nonce can be
put, find a nonce and an extra nonce allowing the hash of the
resulting block (computed with slow-hash) to be acceptable for a given
DIFFICULTY level.
The returned value is the new block data containing the found nonces."
  (loop with nonce-offset = (get-nonce-offset block-template-data)
        with template = (copy-seq block-template-data)
        for hash = (compute-block-hash-from-data template t)
        until (or *mine-stop* (acceptable-hash-p hash difficulty))
        do (let ((random-data (ironclad:random-data (+ 4 reserve-size))))
             (replace template random-data :start1 nonce-offset :end2 4)
             (replace template random-data :start1 reserve-offset :start2 4))
        finally (return (when (acceptable-hash-p hash difficulty)
                          (if *mine-lock*
                              (progn
                                (with-lock-held (*mine-lock*)
                                  (setf *mine-stop* t)
                                  (setf *mine-result* template))
                                *mine-result*)
                              template)))))

(defun mine-block (address reserve-size &key (threads 1) (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  "Mine a block with several THREADS for an ADDRESS using RESERVE-SIZE
bytes of extra nonce."
  (when *mine-lock*
    (error "The mining process is already running."))
  (unwind-protect
       (let ((thread-handles (make-array threads :initial-element nil)))
         (setf *mine-lock* (make-lock))
         (labels ((stop-threads ()
                    (when (aref thread-handles 0)
                      (with-lock-held (*mine-lock*)
                        (setf *mine-stop* t))
                      (dotimes (i threads)
                        (join-thread (aref thread-handles i))
                        (setf (aref thread-handles i) nil))
                      (with-lock-held (*mine-lock*)
                        (setf *mine-stop* nil))))

                  (update-miners (template reserve-size reserve-offset difficulty)
                    (stop-threads)
                    (let ((miner (lambda ()
                                   (miner template reserve-size reserve-offset difficulty))))
                      (dotimes (i threads)
                        (setf (aref thread-handles i) (make-thread miner))))))
           (let ((data (do ()
                           (*mine-result* *mine-result*)
                         (let* ((info (get-block-template-from-daemon address
                                                                      reserve-size
                                                                      :host host
                                                                      :port port
                                                                      :user user
                                                                      :password password))
                                (template (hex-string->bytes (geta info :blocktemplate--blob)))
                                (difficulty (geta info :difficulty))
                                ;;(reserve-offset (geta info :reserved--offset))
                                (reserve-offset (1- (geta info :reserved--offset))) ; off by 1 bug?
                                (height (geta info :height)))
                           (format t "height=~a difficulty=~d~%" height difficulty)
                           (update-miners template reserve-size reserve-offset difficulty)
                           (sleep *mine-refresh-delay*)))))
             (stop-threads)
             (submit-block-to-daemon data :host host :port port :user user :password password))))
    (setf *mine-stop* nil)
    (setf *mine-lock* nil)
    (setf *mine-result* nil)))
