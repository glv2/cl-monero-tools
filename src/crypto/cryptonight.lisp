;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


#+(and sbcl x86-64)
(in-package :sb-c)
#+(and sbcl x86-64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown monero-tools::replace-16/fast
    ((simple-array (unsigned-byte 8) (*))
     (unsigned-byte 32)
     (simple-array (unsigned-byte 8) (*))
     (unsigned-byte 32))
    (values)
    (any)
    :overwrite-fndb-silently t))

#+(and sbcl x86-64)
(in-package :sb-vm)
#+(and sbcl x86-64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-vop (replace-16/fast)
    (:translate monero-tools::replace-16/fast)
    (:policy :fast-safe)
    (:args (buffer1 :scs (descriptor-reg))
           (start1 :scs (unsigned-reg))
           (buffer2 :scs (descriptor-reg))
           (start2 :scs (unsigned-reg)))
    (:arg-types simple-array-unsigned-byte-8
                unsigned-num
                simple-array-unsigned-byte-8
                unsigned-num)
    (:temporary (:sc double-reg) x0)
    (:generator 1000
      (flet ((buffer-mem (base offset)
               (make-ea :qword
                        :base base
                        :index offset
                        :disp (- (* n-word-bytes vector-data-offset)
                                 other-pointer-lowtag))))
        (inst movdqa x0 (buffer-mem buffer2 start2))
        (inst movdqa (buffer-mem buffer1 start1) x0)))))


(in-package :monero-tools)


(declaim (inline replace-16))
(defun replace-16 (buffer1 start1 buffer2 start2)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer1 buffer2)
           (type (unsigned-byte 32) start1 start2)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  #+(and sbcl x86-64)
  (replace-16/fast buffer1 start1 buffer2 start2)
  #-(and sbcl x86-64)
  (replace buffer1 buffer2 :start1 start1 :end1 (+ start1 16) :start2 start2))

(defun cryptonight (data)
  (let* ((+scratchpad-size+ #.(ash 1 21))
         (+bounce-iterations+ #.(ash 1 20))
         (+aes-block-size+ 16)
         (+init-size-blk+ 8)
         (+init-size-byte+ (* +init-size-blk+ +aes-block-size+)))
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    ;; Step 1: Use Keccak1600 to initialize the STATE and TEXT buffers
    ;; from the DATA.
    (let* ((state (keccak1600 data))
           (text (make-array +init-size-byte+ :element-type '(unsigned-byte 8))))
      (declare (type (simple-array (unsigned-byte 8) (200)) state)
               (type (simple-array (unsigned-byte 8) (128)) text)
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
              (declare (type (unsigned-byte 21) scratchpad-address))
              (replace-16 c 0 scratchpad scratchpad-address)
              (pseudo-aes-round c 0 c 0 a)
              (ironclad::xor-block 16 b c 0 scratchpad scratchpad-address)
              (replace-16 b 0 c 0)
              (setf scratchpad-address (logand (ironclad:ub32ref/le b 0) #x1ffff0))
              (replace-16 c 0 scratchpad scratchpad-address)
              (let* ((b0c0 (* (ironclad:ub32ref/le b 0) (ironclad:ub32ref/le c 0)))
                     (b0c1 (* (ironclad:ub32ref/le b 0) (ironclad:ub32ref/le c 4)))
                     (b1c0 (* (ironclad:ub32ref/le b 4) (ironclad:ub32ref/le c 0)))
                     (b1c1 (* (ironclad:ub32ref/le b 4) (ironclad:ub32ref/le c 4)))
                     (carry (+ (ash b0c0 -32) (logand b0c1 #xffffffff) (logand b1c0 #xffffffff)))
                     (s0 (logior (logand b0c0 #xffffffff) (ironclad::mod64ash carry 32)))
                     (carry (+ (ash carry -32) (ash b0c1 -32) (ash b1c0 -32)))
                     (s1 (ironclad::mod64+ b1c1 carry)))
                (declare (type (unsigned-byte 64) b0c0 b0c1 b1c0 b1c1 s0 s1 carry))
                (setf (ironclad:ub64ref/le d 0) s1)
                (setf (ironclad:ub64ref/le d 8) s0))
              (setf (ironclad:ub64ref/le a 0) (ironclad::mod64+ (ironclad:ub64ref/le a 0)
                                                                (ironclad:ub64ref/le d 0)))
              (setf (ironclad:ub64ref/le a 8) (ironclad::mod64+ (ironclad:ub64ref/le a 8)
                                                                (ironclad:ub64ref/le d 8)))
              (replace-16 scratchpad scratchpad-address a 0)
              (ironclad::xor-block 16 a c 0 a 0))))
        ;; Step 4: Sequentially pass through the mixing buffer and use
        ;; 10 rounds of AES encryption to mix the random data back into
        ;; the TEXT buffer. TEXT was originally created with the output
        ;; of Keccak1600.
        (replace text state :start2 64)
        (setf round-keys (pseudo-aes-expand-key state 32))
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
        (let ((state-64 (make-array 25 :element-type '(unsigned-byte 64))))
          (declare (type (simple-array (unsigned-byte 64) (25)) state-64)
                   (dynamic-extent state-64))
          (dotimes (i 25)
            (setf (aref state-64 i) (ironclad:ub64ref/le state (* 8 i))))
          (keccakf state-64)
          (dotimes (i 25)
            (setf (ironclad:ub64ref/le state (* 8 i)) (aref state-64 i))))
        (ironclad:digest-sequence (case (logand (aref state 0) 3)
                                    ((0) :blake256)
                                    ((1) :groestl/256)
                                    ((2) :jh/256)
                                    ((3) :skein512/256))
                                  state)))))
