;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun cryptonight (data)
  (let* ((+scratchpad-size+ (ash 1 21))
         (+bounce-iterations+ (ash 1 20))
         (+aes-block-size+ 16)
         (+init-size-blk+ 8)
         (+init-size-byte+ (* +init-size-blk+ +aes-block-size+)))
    ;; Step 1: Use Keccak1600 to initialize the STATE and TEXT buffers
    ;; from the DATA.
    (let* ((state (keccak1600 data))
           (text (subseq state 64 (+ 64 +init-size-byte+))))
      (declare (type (simple-array (unsigned-byte 8) (200)) state))
      ;; Step 2: Iteratively encrypt the results from Keccak to fill the
      ;; 2MB large random access buffer.
      (let ((round-keys (pseudo-aes-expand-key state))
            (scratchpad (make-array +scratchpad-size+ :element-type '(unsigned-byte 8))))
        (dotimes (i (/ +scratchpad-size+ +init-size-byte+))
          (dotimes (j +init-size-blk+)
            (pseudo-aes-rounds text (* j +aes-block-size+) text (* j +aes-block-size+) round-keys))
          (replace scratchpad text :start1 (* i +init-size-byte+)))
        ;; Step 3: Bounce randomly 1,048,576 times through the mixing
        ;; buffer, using 524,288 iterations of the following mixing
        ;; function. Each execution performs two reads and writes from
        ;; the mixing buffer.
        (let ((a (make-array 16 :element-type '(unsigned-byte 8)))
              (b (make-array 16 :element-type '(unsigned-byte 8)))
              (c (make-array 16 :element-type '(unsigned-byte 8)))
              (d (make-array 16 :element-type '(unsigned-byte 8))))
          (setf (ironclad:ub64ref/le a 0) (logxor (ironclad:ub64ref/le state 0)
                                                  (ironclad:ub64ref/le state 32)))
          (setf (ironclad:ub64ref/le a 8) (logxor (ironclad:ub64ref/le state 8)
                                                  (ironclad:ub64ref/le state 40)))
          (setf (ironclad:ub64ref/le b 0) (logxor (ironclad:ub64ref/le state 16)
                                                  (ironclad:ub64ref/le state 48)))
          (setf (ironclad:ub64ref/le b 8) (logxor (ironclad:ub64ref/le state 24)
                                                  (ironclad:ub64ref/le state 56)))
          (dotimes (i (/ +bounce-iterations+ 2))
            (let ((scratchpad-address (logand (ironclad:ub32ref/le a 0) #x1ffff0)))
              (replace c scratchpad :start2 scratchpad-address)
              (pseudo-aes-round c 0 c 0 a)
              (ironclad::xor-block 16 b c 0 scratchpad scratchpad-address)
              (replace b c)
              (setf scratchpad-address (logand (ironclad:ub32ref/le b 0) #x1ffff0))
              (replace c scratchpad :start2 scratchpad-address)
              (integer->bytes (* (ironclad:ub64ref/le b 0) (ironclad:ub64ref/le c 0))
                              :buffer d
                              :size 16)
              (rotatef (ironclad:ub64ref/le d 0) (ironclad:ub64ref/le d 8))
              (setf (ironclad:ub64ref/le a 0) (ironclad::mod64+ (ironclad:ub64ref/le a 0)
                                                                (ironclad:ub64ref/le d 0)))
              (setf (ironclad:ub64ref/le a 8) (ironclad::mod64+ (ironclad:ub64ref/le a 8)
                                                                (ironclad:ub64ref/le d 8)))
              (replace scratchpad a :start1 scratchpad-address)
              (ironclad::xor-block 16 a c 0 a 0))))
        ;; Step 4: Sequentially pass through the mixing buffer and use
        ;; 10 rounds of AES encryption to mix the random data back into
        ;; the TEXT buffer. TEXT was originally created with the output
        ;; of Keccak1600.
        (replace text state :start2 64)
        (setf round-keys (pseudo-aes-expand-key (subseq state 32)))
        (dotimes (i (/ +scratchpad-size+ +init-size-byte+))
          (dotimes (j +init-size-blk+)
            (setf (ironclad:ub64ref/le text (* j +aes-block-size+))
                  (logxor (ironclad:ub64ref/le text (* j +aes-block-size+))
                          (ironclad:ub64ref/le scratchpad (+ (* i +init-size-byte+)
                                                             (* j +aes-block-size+)))))
            (setf (ironclad:ub64ref/le text (+ (* j +aes-block-size+) 8))
                  (logxor (ironclad:ub64ref/le text (+ (* j +aes-block-size+) 8))
                          (ironclad:ub64ref/le scratchpad (+ (* i +init-size-byte+)
                                                             (* j +aes-block-size+)
                                                             8))))
            (pseudo-aes-rounds text (* j +aes-block-size+) text (* j +aes-block-size+) round-keys)))
        ;; Step 5: Apply Keccak to the state again, and then use the
        ;; resulting data to select which of four finalizer hash
        ;; functions to apply to the data (Blake, Groestl, JH, or
        ;; Skein). Use this hash to squeeze the state array down to the
        ;; final 256 bit hash output.
        (replace state text :start1 64)
        (keccakf state)
        (ironclad:digest-sequence (case (logand (aref state 0) 3)
                                    ((0) :blake256)
                                    ((1) :groestl/256)
                                    ((2) :jh/256)
                                    ((3) :skein512/256))
                                  state)))))
