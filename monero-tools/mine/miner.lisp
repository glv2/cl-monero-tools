;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defparameter *mine-stop* nil)
(defparameter *mine-result* nil)
(defparameter *mine-lock* nil)

(defun modify-block-template (template nonce &key reserve-nonce reserve-offset in-place)
  "Change the NONCE and the optional RESERVE-NONCE of a block TEMPLATE. If
IN-PLACE is NIL a new template is created, if not the TEMPLATE passed in
argument is modified."
  (let ((nonce-offset (get-nonce-offset template))
        (template (if in-place template (copy-seq template))))
    (replace template nonce :start1 nonce-offset :end2 4)
    (when (and reserve-nonce reserve-offset)
      (replace template reserve-nonce
               :start1 reserve-offset :end2 (length reserve-nonce)))
    template))

(defun miner (template reserve-size reserve-offset difficulty)
  "Given a block TEMPLATE represented by a byte vector containing a reserved
space of RESERVE-SIZE bytes starting at RESERVE-OFFSET in BLOCK-TEMPLATE-DATA
in which an extra nonce can be put, find a nonce and an extra nonce allowing
the hash of the resulting block (computed with slow-hash) to be acceptable for
a given DIFFICULTY level.
The returned value is the new block data containing the found nonces."
  (let ((template (copy-seq template)))
    (iter (for hash next (compute-block-hash-from-data template t))
          (until (or *mine-stop* (acceptable-hash-p hash difficulty)))
          (modify-block-template template (random-data 4)
                                 :reserve-nonce (random-data reserve-size)
                                 :reserve-offset reserve-offset
                                 :in-place t)
          (finally (return (when (acceptable-hash-p hash difficulty)
                             (if *mine-lock*
                                 (progn
                                   (with-lock-held (*mine-lock*)
                                     (setf *mine-stop* t)
                                     (setf *mine-result* template))
                                   *mine-result*)
                                 template)))))))
