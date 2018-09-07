;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun cryptonight (data variant)
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum variant)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (when (and (plusp variant) (< (length data) 43))
    (error "Cryptonight variants require at least 43 bytes of data."))
  (let* ((+scratchpad-size+ #.(ash 1 21))
         (+bounce-iterations+ #.(ash 1 20))
         (+aes-block-size+ 16)
         (+init-size-blk+ 8)
         (+init-size-byte+ (* +init-size-blk+ +aes-block-size+)))
    ;; Step 1: Use Keccak1600 to initialize the STATE and TEXT buffers
    ;; from the DATA.
    (let* ((state (keccak1600 data))
           (text (make-array +init-size-byte+ :element-type '(unsigned-byte 8)))
           (tweak (case variant
                    ((1) (logxor (ub64ref/le state 192) (ub64ref/le data 35)))
                    (t 0))))
      (declare (type (simple-array (unsigned-byte 8) (200)) state)
               (type (simple-array (unsigned-byte 8) (128)) text)
               (type (unsigned-byte 64) tweak)
               (dynamic-extent text))
      (replace text state :start2 64)
      ;; Step 2: Iteratively encrypt the results from Keccak to fill the
      ;; 2MB large random access buffer.
      (let ((round-keys (pseudo-aes-expand-key state 0))
            (scratchpad (make-array +scratchpad-size+ :element-type '(unsigned-byte 8))))
        (declare (type ironclad::aes-round-keys round-keys)
                 (type (simple-array (unsigned-byte 8) (#.(ash 1 21))) scratchpad))
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
          (declare (type (simple-array (unsigned-byte 8) (16)) a b c d)
                   (dynamic-extent a b c d))
          (setf (ub64ref/le a 0) (logxor (ub64ref/le state 0) (ub64ref/le state 32)))
          (setf (ub64ref/le a 8) (logxor (ub64ref/le state 8) (ub64ref/le state 40)))
          (setf (ub64ref/le b 0) (logxor (ub64ref/le state 16) (ub64ref/le state 48)))
          (setf (ub64ref/le b 8) (logxor (ub64ref/le state 24) (ub64ref/le state 56)))
          (dotimes (i (/ +bounce-iterations+ 2))
            ;; Iteration 1
            (let ((scratchpad-address (logand (ub32ref/le a 0) #x1ffff0)))
              (declare (type (unsigned-byte 21) scratchpad-address))
              (ironclad::copy-block 16 scratchpad scratchpad-address c 0)
              (pseudo-aes-round c 0 c 0 a)
              (ironclad::xor-block 16 b c 0 scratchpad scratchpad-address)
              (ironclad::copy-block 16 c 0 b 0)
              (case variant
                ((1)
                 (let* ((tmp (aref scratchpad (+ scratchpad-address 11)))
                        (index (logand (ash (logior (logand (ash tmp -3) 6) (logand tmp 1)) 1) #xff)))
                   (declare (type (unsigned-byte 8) tmp index))
                   (setf (aref scratchpad (+ scratchpad-address 11))
                         (logxor tmp (logand (ash #x75310 (- index)) #x30))))))
              ;; Iteration 2
              (setf scratchpad-address (logand (ub32ref/le b 0) #x1ffff0))
              (ironclad::copy-block 16 scratchpad scratchpad-address c 0)
              (let* ((b0c0 (* (ub32ref/le b 0) (ub32ref/le c 0)))
                     (b0c1 (* (ub32ref/le b 0) (ub32ref/le c 4)))
                     (b1c0 (* (ub32ref/le b 4) (ub32ref/le c 0)))
                     (b1c1 (* (ub32ref/le b 4) (ub32ref/le c 4)))
                     (carry (+ (ash b0c0 -32) (logand b0c1 #xffffffff) (logand b1c0 #xffffffff)))
                     (s0 (logior (logand b0c0 #xffffffff) (ironclad::mod64ash carry 32)))
                     (carry (+ (ash carry -32) (ash b0c1 -32) (ash b1c0 -32)))
                     (s1 (ironclad::mod64+ b1c1 carry)))
                (declare (type (unsigned-byte 64) b0c0 b0c1 b1c0 b1c1 s0 s1 carry))
                (setf (ub64ref/le d 0) s1)
                (setf (ub64ref/le d 8) s0))
              (setf (ub64ref/le a 0) (ironclad::mod64+ (ub64ref/le a 0) (ub64ref/le d 0)))
              (setf (ub64ref/le a 8) (ironclad::mod64+ (ub64ref/le a 8) (ub64ref/le d 8)))
              (ironclad::copy-block 16 a 0 scratchpad scratchpad-address)
              (ironclad::xor-block 16 a c 0 a 0)
              (case variant
                ((1)
                 (setf (ub64ref/le scratchpad (+ scratchpad-address 8))
                       (logxor (ub64ref/le scratchpad (+ scratchpad-address 8)) tweak)))))))
        ;; Step 4: Sequentially pass through the mixing buffer and use
        ;; 10 rounds of AES encryption to mix the random data back into
        ;; the TEXT buffer. TEXT was originally created with the output
        ;; of Keccak1600.
        (replace text state :start2 64)
        (setf round-keys (pseudo-aes-expand-key state 32))
        (dotimes (i (/ +scratchpad-size+ +init-size-byte+))
          (dotimes (j +init-size-blk+)
            (setf (ub64ref/le text (* j +aes-block-size+))
                  (logxor (ub64ref/le text (* j +aes-block-size+))
                          (ub64ref/le scratchpad (+ (* i +init-size-byte+)
                                                    (* j +aes-block-size+)))))
            (setf (ub64ref/le text (+ (* j +aes-block-size+) 8))
                  (logxor (ub64ref/le text (+ (* j +aes-block-size+) 8))
                          (ub64ref/le scratchpad (+ (* i +init-size-byte+)
                                                    (* j +aes-block-size+)
                                                    8))))
            (pseudo-aes-rounds text (* j +aes-block-size+) text (* j +aes-block-size+) round-keys)))
        ;; Step 5: Apply Keccak to the state again, and then use the
        ;; resulting data to select which of four finalizer hash
        ;; functions to apply to the data (Blake, Groestl, JH, or
        ;; Skein). Use this hash to squeeze the state array down to the
        ;; final 256 bit hash output.
        (replace state text :start1 64)
        (let ((state-64 (make-array 25 :element-type '(unsigned-byte 64))))
          (declare (type (simple-array (unsigned-byte 64) (25)) state-64)
                   (dynamic-extent state-64))
          (dotimes (i 25)
            (setf (aref state-64 i) (ub64ref/le state (* 8 i))))
          (keccakf state-64)
          (dotimes (i 25)
            (setf (ub64ref/le state (* 8 i)) (aref state-64 i))))
        (ironclad:digest-sequence (case (logand (aref state 0) 3)
                                    ((0) :blake256)
                                    ((1) :groestl/256)
                                    ((2) :jh/256)
                                    ((3) :skein512/256))
                                  state)))))
