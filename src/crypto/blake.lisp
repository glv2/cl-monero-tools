;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :crypto)


;;;
;;; Parameters
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +blake256-rounds+ 14)
  (defconstant +blake256-block-size+ 64)
  (defconst +blake256-sigma+
    (make-array '(14 16)
                :element-type '(integer 0 15)
                :initial-contents '((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                                    (14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3)
                                    (11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4)
                                    (7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8)
                                    (9 0 5 7 2 4 10 15 14 1 11 12 6 8 3 13)
                                    (2 12 6 10 0 11 8 3 4 13 7 5 15 14 1 9)
                                    (12 5 1 15 14 13 4 10 0 7 6 3 9 2 8 11)
                                    (13 11 7 14 12 1 3 9 5 0 15 4 8 6 2 10)
                                    (6 15 14 9 11 3 0 8 12 2 13 7 1 4 10 5)
                                    (10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0)
                                    (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                                    (14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3)
                                    (11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4)
                                    (7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8))))
  (defconst +blake256-cst+
    (make-array 16
                :element-type '(unsigned-byte 32)
                :initial-contents '(#x243f6a88 #x85a308d3 #x13198a2e #x03707344
                                    #xa4093822 #x299f31d0 #x082efa98 #xec4e6c89
                                    #x452821e6 #x38d01377 #xbe5466cf #x34e90c6c
                                    #xc0ac29b7 #xc97c50dd #x3f84d5b5 #xb5470917)))
  (defconst +blake256-padding-a+
    (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#x81)))
  (defconst +blake256-padding-b+
    (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#x01)))
  (defconst +blake256-padding+
    (make-array 64
                :element-type '(unsigned-byte 8)
                :initial-contents '(#x80 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))


;;;
;;; Blake256 rounds
;;;

(declaim (ftype (function ((simple-array (unsigned-byte 32) (8))
                           (simple-array (unsigned-byte 32) (4))
                           (simple-array (unsigned-byte 32) (2))
                           (simple-array (unsigned-byte 8) (*))
                           fixnum
                           boolean))
                blake256-rounds))
(defun blake256-rounds (state-h state-s state-t input start nullt)
  (declare (type (simple-array (unsigned-byte 32) (8)) state-h)
           (type (simple-array (unsigned-byte 32) (4)) state-s)
           (type (simple-array (unsigned-byte 32) (2)) state-t)
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start)
           (type boolean nullt)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((v0 (aref state-h 0))
        (v1 (aref state-h 1))
        (v2 (aref state-h 2))
        (v3 (aref state-h 3))
        (v4 (aref state-h 4))
        (v5 (aref state-h 5))
        (v6 (aref state-h 6))
        (v7 (aref state-h 7))
        (v8 (logxor (aref state-s 0) #x243f6a88))
        (v9 (logxor (aref state-s 1) #x85a308d3))
        (v10 (logxor (aref state-s 2) #x13198a2e))
        (v11 (logxor (aref state-s 3) #x03707344))
        (v12 #xa4093822)
        (v13 #x299f31d0)
        (v14 #x082efa98)
        (v15 #xec4e6c89)
        (m (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
    (declare (type (unsigned-byte 32) v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
             (type (simple-array (unsigned-byte 32) (16)) m)
             (dynamic-extent m))
    (unless nullt
      (setf v12 (logxor v12 (aref state-t 0))
            v13 (logxor v13 (aref state-t 0))
            v14 (logxor v14 (aref state-t 1))
            v15 (logxor v15 (aref state-t 1))))

    ;; Get input data as 32-bit big-endian integers
    (dotimes-unrolled (i 16)
      (setf (aref m i) (ub32ref/be input (+ start (* i 4)))))

    ;; Mixing rounds
    (macrolet ((blake256-mixing (va vb vc vd e)
                 `(setf ,va (mod32+ (mod32+ ,va ,vb)
                                    (logxor (aref m (aref +blake256-sigma+ i ,e))
                                            (aref +blake256-cst+ (aref +blake256-sigma+ i (1+ ,e)))))
                        ,vd (ror32 (logxor ,vd ,va) 16)
                        ,vc (mod32+ ,vc ,vd)
                        ,vb (ror32 (logxor ,vb ,vc) 12)
                        ,va (mod32+ (mod32+ ,va ,vb)
                                    (logxor (aref m (aref +blake256-sigma+ i (1+ ,e)))
                                            (aref +blake256-cst+ (aref +blake256-sigma+ i ,e))))
                        ,vd (ror32 (logxor ,vd ,va) 8)
                        ,vc (mod32+ ,vc ,vd)
                        ,vb (ror32 (logxor ,vb ,vc) 7))))
      (dotimes-unrolled (i +blake256-rounds+)
        (blake256-mixing v0 v4 v8 v12 0)
        (blake256-mixing v1 v5 v9 v13 2)
        (blake256-mixing v2 v6 v10 v14 4)
        (blake256-mixing v3 v7 v11 v15 6)
        (blake256-mixing v3 v4 v9 v14 14)
        (blake256-mixing v2 v7 v8 v13 12)
        (blake256-mixing v0 v5 v10 v15 8)
        (blake256-mixing v1 v6 v11 v12 10)))

      ;; Compute new state
      (setf (aref state-h 0) (logxor (aref state-h 0) v0 v8 (aref state-s 0))
            (aref state-h 1) (logxor (aref state-h 1) v1 v9 (aref state-s 1))
            (aref state-h 2) (logxor (aref state-h 2) v2 v10 (aref state-s 2))
            (aref state-h 3) (logxor (aref state-h 3) v3 v11 (aref state-s 3))
            (aref state-h 4) (logxor (aref state-h 4) v4 v12 (aref state-s 0))
            (aref state-h 5) (logxor (aref state-h 5) v5 v13 (aref state-s 1))
            (aref state-h 6) (logxor (aref state-h 6) v6 v14 (aref state-s 2))
            (aref state-h 7) (logxor (aref state-h 7) v7 v15 (aref state-s 3))))

  (values))


;;;
;;; Digest structures and functions
;;;

(defstruct (blake256
             (:constructor %make-blake256-digest nil)
             (:copier nil))
  (state-h (make-array 8
                       :element-type '(unsigned-byte 32)
                       :initial-contents '(#x6a09e667 #xbb67ae85
                                           #x3c6ef372 #xa54ff53a
                                           #x510e527f #x9b05688c
                                           #x1f83d9ab #x5be0cd19))
           :type (simple-array (unsigned-byte 32) (8)))
  (state-s (make-array 4 :element-type '(unsigned-byte 32) :initial-element 0)
           :type (simple-array (unsigned-byte 32) (4)))
  (state-t (make-array 2 :element-type '(unsigned-byte 32) :initial-element 0)
           :type (simple-array (unsigned-byte 32) (2)))
  (nullt nil :type boolean)
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
          :type (simple-array (unsigned-byte 8) (64)))
  (buffer-index 0 :type (integer 0 64)))

(defmethod reinitialize-instance ((state blake256) &rest initargs)
  (declare (ignore initargs))
  (setf (aref (blake256-state-h state) 0) #x6a09e667
        (aref (blake256-state-h state) 1) #xbb67ae85
        (aref (blake256-state-h state) 2) #x3c6ef372
        (aref (blake256-state-h state) 3) #xa54ff53a
        (aref (blake256-state-h state) 4) #x510e527f
        (aref (blake256-state-h state) 5) #x9b05688c
        (aref (blake256-state-h state) 6) #x1f83d9ab
        (aref (blake256-state-h state) 7) #x5be0cd19
        (blake256-nullt state) nil
        (blake256-buffer-index state) 0)
  (fill (blake256-state-s state) 0)
  (fill (blake256-state-t state) 0)
  state)

(defmethod copy-digest ((state blake256) &optional copy)
  (declare (type (or null blake256) copy))
  (let ((copy (if copy
                  copy
                  (%make-blake256-digest))))
    (declare (type blake256 copy))
    (replace (blake256-state-h copy) (blake256-state-h state))
    (replace (blake256-state-s copy) (blake256-state-s state))
    (replace (blake256-state-t copy) (blake256-state-t state))
    (replace (blake256-buffer copy) (blake256-buffer state))
    (setf (blake256-nullt copy) (blake256-nullt state)
          (blake256-buffer-index copy) (blake256-buffer-index state))
    copy))

(defun blake256-update (state input start end)
  (declare (type blake256 state)
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let* ((blake256-state-h (blake256-state-h state))
         (blake256-state-s (blake256-state-s state))
         (blake256-state-t (blake256-state-t state))
         (nullt (blake256-nullt state))
         (buffer (blake256-buffer state))
         (buffer-index (blake256-buffer-index state))
         (length (- end start))
         (free (- +blake256-block-size+ buffer-index)))
    (declare (type (simple-array (unsigned-byte 32) (8)) blake256-state-h)
             (type (simple-array (unsigned-byte 32) (4)) blake256-state-s)
             (type (simple-array (unsigned-byte 32) (2)) blake256-state-t)
             (type (simple-array (unsigned-byte 8) (64)) buffer)
             (type (integer 0 64) buffer-index free)
             (type fixnum length))
    ;; Try to process the data in buffer
    (when (and (plusp buffer-index) (>= length free))
      (replace buffer input :start1 buffer-index :end1 +blake256-block-size+ :start2 start)
      (setf (aref blake256-state-t 0) (mod32+ (aref blake256-state-t 0)
                                              (* 8 +blake256-block-size+)))
      (when (zerop (aref blake256-state-t 0))
        (setf (aref blake256-state-t 1) (mod32+ (aref blake256-state-t 1) 1)))
      (blake256-rounds blake256-state-h blake256-state-s blake256-state-t buffer 0 nullt)
      (incf start free)
      (decf length free)
      (setf buffer-index 0))

    ;; Process data in message
    (loop until (< length +blake256-block-size+) do
      (setf (aref blake256-state-t 0) (mod32+ (aref blake256-state-t 0)
                                              (* 8 +blake256-block-size+)))
      (when (zerop (aref blake256-state-t 0))
        (setf (aref blake256-state-t 1) (mod32+ (aref blake256-state-t 1) 1)))
      (blake256-rounds blake256-state-h blake256-state-s blake256-state-t input start nullt)
      (incf start +blake256-block-size+)
      (decf length +blake256-block-size+))

    ;; Put remaining message data in buffer
    (when (plusp length)
      (replace buffer input :start1 buffer-index :end1 (+ buffer-index length) :start2 start)
      (incf buffer-index length))

    ;; Save the new state
    (setf (blake256-buffer-index state) buffer-index)
    (values)))

(defun blake256-finalize (state digest digest-start)
  (let* ((digest-length (digest-length state))
         (blake256-state-h (blake256-state-h state))
         (blake256-state-t (blake256-state-t state))
         (buffer-index (blake256-buffer-index state))
         (buffer-bit-length (* 8 buffer-index))
         (lo (mod32+ (aref blake256-state-t 0) buffer-bit-length))
         (hi (aref blake256-state-t 1))
         (message-length (make-array 8 :element-type '(unsigned-byte 8))))
    ;; Process remaining data after padding it
    (when (< lo buffer-bit-length)
      (setf hi (mod32+ hi 1)))
    (setf (ub32ref/be message-length 0) hi
          (ub32ref/be message-length 4) lo)
    (if (= buffer-index 55)
        (progn
          (setf (aref blake256-state-t 0) (mod32- (aref blake256-state-t 0) 8))
          (blake256-update state +blake256-padding-a+ 0 1))
        (progn
          (if (< buffer-index 55)
              (progn
                (when (zerop buffer-index)
                  (setf (blake256-nullt state) t))
                (setf (aref blake256-state-t 0) (mod32- (aref blake256-state-t 0)
                                                        (- 440 buffer-bit-length)))
                (blake256-update state +blake256-padding+ 0 (- 55 buffer-index)))
              (progn
                (setf (aref blake256-state-t 0) (mod32- (aref blake256-state-t 0)
                                                        (- 512 buffer-bit-length)))
                (blake256-update state +blake256-padding+ 0 (- 64 buffer-index))
                (setf (aref blake256-state-t 0) (mod32- (aref blake256-state-t 0) 440))
                (blake256-update state +blake256-padding+ 1 55)
                (setf (blake256-nullt state) t)))
          (blake256-update state +blake256-padding-b+ 0 1)
          (setf (aref blake256-state-t 0) (mod32- (aref blake256-state-t 0) 8))))
    (setf (aref blake256-state-t 0) (mod32- (aref blake256-state-t 0) 64))
    (blake256-update state message-length 0 8)

    ;; Get output
    (let ((output (make-array digest-length :element-type '(unsigned-byte 8))))
      (dotimes (i 8)
        (dotimes (j 4)
          (setf (aref output (+ (* i 4) j)) (ldb (byte 8 (- 24 (* j 8))) (aref blake256-state-h i)))))
      (etypecase digest
        ((simple-array (unsigned-byte 8) (*))
         (replace digest output :start1 digest-start :end2 digest-length)
         digest)
        (null
         output)))))

(define-digest-updater blake256
  (blake256-update state sequence start end))

(define-digest-finalizer (blake256 32)
  (blake256-finalize state digest digest-start))

(defdigest blake256 :digest-length 32 :block-length 64)

(export 'blake256)
