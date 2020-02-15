;;;; This file is part of monero-tools
;;;; Copyright 2018-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +keccakf-rndc+
      (make-array 24
                  :element-type '(unsigned-byte 64)
                  :initial-contents '(#x0000000000000001 #x0000000000008082
                                      #x800000000000808a #x8000000080008000
                                      #x000000000000808b #x0000000080000001
                                      #x8000000080008081 #x8000000000008009
                                      #x000000000000008a #x0000000000000088
                                      #x0000000080008009 #x000000008000000a
                                      #x000000008000808b #x800000000000008b
                                      #x8000000000008089 #x8000000000008003
                                      #x8000000000008002 #x8000000000000080
                                      #x000000000000800a #x800000008000000a
                                      #x8000000080008081 #x8000000000008080
                                      #x0000000080000001 #x8000000080008008))
    :test #'equalp)
  (define-constant +keccakf-rotc+
      (make-array 24
                  :element-type '(unsigned-byte 6)
                  :initial-contents '(1 3 6 10 15 21 28 36
                                      45 55 2 14 27 41 56 8
                                      25 43 62 18 39 61 20 44))
    :test #'equalp)
  (define-constant +keccakf-piln+
      (make-array 24
                  :element-type '(unsigned-byte 5)
                  :initial-contents '(10 7 11 17 18 3 5 16
                                      8 21 24 4 15 23 19 13
                                      12 2 20 14 22 9 6 1))
    :test #'equalp))

(defun keccakf (state)
  (declare (type (simple-array (unsigned-byte 64) (25)) state)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((rndc (load-time-value +keccakf-rndc+ t))
        (rotc (load-time-value +keccakf-rotc+ t))
        (piln (load-time-value +keccakf-piln+ t))
        (t1 0)
        (t2 0)
        (bc (make-array 5 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (24)) rndc)
             (type (simple-array (unsigned-byte 6) (24)) rotc)
             (type (simple-array (unsigned-byte 5) (24)) piln)
             (type (unsigned-byte 64) t1 t2)
             (type (simple-array (unsigned-byte 64) (5)) bc)
             (dynamic-extent bc))
    (dotimes (round 24)
      ;; Theta
      (dotimes-unrolled (i 5)
        (setf (aref bc i) (logxor (aref state i)
                                  (aref state (+ i 5))
                                  (aref state (+ i 10))
                                  (aref state (+ i 15))
                                  (aref state (+ i 20)))))
      (dotimes-unrolled (i 5)
        (setf t1 (logxor (aref bc (mod (+ i 4) 5))
                         (rol64 (aref bc (mod (+ i 1) 5)) 1)))
        (dotimes-unrolled (j 5)
          (setf (aref state (+ i (* j 5))) (logxor (aref state (+ i (* j 5))) t1))))

      ;; Rho Pi
      (setf t1 (aref state 1))
      (dotimes-unrolled (i 24)
        (setf t2 (aref piln i))
        (setf (aref bc 0) (aref state t2))
        (setf (aref state t2) (rol64 t1 (aref rotc i)))
        (setf t1 (aref bc 0)))

      ;; Chi
      (dotimes-unrolled (j 5)
        (dotimes-unrolled (i 5)
          (setf (aref bc i) (aref state (+ i (* j 5)))))
        (dotimes-unrolled (i 5)
          (setf (aref state (+ i (* j 5))) (logxor (aref state (+ i (* j 5)))
                                                   (logand (mod64lognot (aref bc (mod (+ i 1) 5)))
                                                           (aref bc (mod (+ i 2) 5)))))))

      ;; Iota
      (setf (aref state 0) (logxor (aref state 0) (aref rndc round))))))

(defun keccak1600 (data)
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let* ((data-length (length data))
         (start 0)
         (state (make-array 25 :element-type '(unsigned-byte 64) :initial-element 0))
         (tmp (make-array 144 :element-type '(unsigned-byte 8))))
    (declare (type fixnum data-length start)
             (type (simple-array (unsigned-byte 64) (25)) state)
             (type (simple-array (unsigned-byte 8) (144)) tmp)
             (dynamic-extent state tmp))
    (iter (until (< data-length 136))
          (dotimes-unrolled (i 17)
            (setf (aref state i) (logxor (aref state i) (ub64ref/le data start)))
            (incf start 8))
          (decf data-length 136)
          (keccakf state))

    ;; Last block and padding
    (replace tmp data :end1 data-length :start2 start)
    (setf (aref tmp data-length) 1)
    (fill tmp 0 :start (+ data-length 1))
    (setf (aref tmp 135) (logior (aref tmp 135) #x80))

    (dotimes-unrolled (i 17)
      (setf (aref state i) (logxor (aref state i) (ub64ref/le tmp (* i 8)))))
    (keccakf state)

    (let ((result (make-array 200 :element-type '(unsigned-byte 8))))
      (dotimes-unrolled (i 25)
        (setf (ub64ref/le result (* i 8)) (aref state i)))
      result)))
