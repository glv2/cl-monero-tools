;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun transaction-history (address secret-view-key &key secret-spend-key key-images (start 0) end)
  (let ((public-spend-key (geta (decode-address address) :public-spend-key))
        (key-images (or key-images (make-hash-table :test #'equalp)))
        (end (or end (get-block-count-from-daemon)))
        (history '()))
    (loop while (<= start end) do
      (let* ((block-heights (loop for i from start to (min (+ start 99) end)
                                  collect i))
             (transaction-ids (apply #'concatenate
                                     'vector
                                      (map 'list
                                           (lambda (data)
                                             (transaction-hashes (deserialize-block data 0)))
                                           (get-blocks-by-height-from-daemon block-heights))))
             (transactions (map 'vector
                                (lambda (data)
                                  (deserialize-transaction data 0))
                                (get-transaction-data-from-daemon transaction-ids))))
        (incf start (length block-heights))
        (dotimes (j (length transactions))
          (let* ((transaction (aref transactions j))
                 (prefix (geta transaction :prefix))
                 (outputs (geta prefix :outputs))
                 (extra (geta prefix :extra))
                 (transaction-public-key (dolist (field extra)
                                           (let ((key (geta field :transaction-public-key)))
                                             (when key
                                               (return key)))))
                 (rct-signature (geta transaction :rct-signature))
                 (received 0))
            (dotimes (k (length outputs))
              (let* ((output (aref outputs k))
                     (key (geta (geta output :target) :key))
                     (derivation (derive-key transaction-public-key secret-view-key))
                     (output-public-key (derive-output-public-key derivation k public-spend-key))
                     (output-secret-key (when secret-spend-key
                                          (derive-output-secret-key derivation k secret-spend-key))))
                (when (equalp key output-public-key)
                  (let ((amount (if (or (null rct-signature)
                                        (eql (geta rct-signature :type) +rct-type-null+))
                                    (geta output :amount)
                                    (let* ((ecdh-info (aref (geta rct-signature :ecdh-info) k))
                                           (encrypted-amount (geta ecdh-info :amount)))
                                      (decrypt-amount encrypted-amount
                                                      k
                                                      transaction-public-key
                                                      secret-view-key)))))
                    (when output-secret-key
                      (let ((key-image (compute-key-image output-secret-key output-public-key)))
                        (setf (gethash key-image key-images) amount)))
                    (incf received amount)))))
            (when (plusp received)
              (push (list (elt transaction-ids j) received) history))
            (dolist (key-image (spent-key-images transaction))
              (let ((amount (gethash key-image key-images)))
                (when amount
                  (push (list (elt transaction-ids j) (- amount)) history)
                  (remhash key-image key-images))))))))
    (let ((total (reduce #'+ history :key #'cadr)))
      (values (reverse history) total key-images))))
