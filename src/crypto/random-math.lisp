;;;; This file is part of monero-tools
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defconstant +total-latency+ (* 15 3))
(defconstant +num-instructions-min+ 60)
(defconstant +num-instructions-max+ 70)
(defconstant +alu-count-mul+ 1)
(defconstant +alu-count+ 3)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +mul+ 0)
  (defconstant +add+ 1)
  (defconstant +sub+ 2)
  (defconstant +ror+ 3)
  (defconstant +rol+ 4)
  (defconstant +xor+ 5)
  (defconstant +ret+ 6))
(defconstant +instruction-count+ 6)
(define-constant +op-latency+
    (make-array +instruction-count+
                :element-type 'fixnum
                :initial-contents '(3 2 1 2 2 1))
  :test #'equalp)
(define-constant +asic-op-latency+
    (make-array +instruction-count+
                :element-type 'fixnum
                :initial-contents '(3 1 1 1 1 1))
  :test #'equalp)
(define-constant +op-alus+
    (make-array +instruction-count+
                :element-type 'fixnum
                :initial-contents (list +alu-count-mul+
                                        +alu-count+
                                        +alu-count+
                                        +alu-count+
                                        +alu-count+
                                        +alu-count+))
  :test #'equalp)

(defstruct instruction
  (opcode 0 :type (unsigned-byte 8))
  (dst-index 0 :type (unsigned-byte 8))
  (src-index 0 :type (unsigned-byte 8))
  (c 0 :type (unsigned-byte 32)))

(declaim (inline random-math))
(defun random-math (code r)
  (declare (type (simple-array (unsigned-byte 32) (9)) r)
           (type (simple-array instruction (*)) code)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (dotimes (i 70)
    (let* ((op (aref code i))
           (opcode (instruction-opcode op))
           (src (aref r (instruction-src-index op)))
           (dst-index (instruction-dst-index op))
           (dst (aref r dst-index)))
      (declare (type instruction op)
               (type (unsigned-byte 8) opcode dst-index)
               (type (unsigned-byte 32) src dst))
      (case opcode
        (#.+mul+ (setf (aref r dst-index) (mod32* dst src)))
        (#.+add+ (setf (aref r dst-index) (mod32+ dst (mod32+ (instruction-c op) src))))
        (#.+sub+ (setf (aref r dst-index) (mod32- dst src)))
        (#.+ror+ (setf (aref r dst-index) (ror32 dst (mod src 32))))
        (#.+rol+ (setf (aref r dst-index) (rol32 dst (mod src 32))))
        (#.+xor+ (setf (aref r dst-index) (logxor dst src)))
        (#.+ret+ (return)))))
  (values))

(defun random-math-init (code height)
  (let* ((data-index 32)
         (data (make-array data-index :element-type '(unsigned-byte 8) :initial-element 0))
         (code-size 0)
         (r8-used nil))
    (flet ((check-data (bytes-needed data data-size)
             (when (> (+ data-index bytes-needed) data-size)
               (digest-sequence :blake256 data :digest data)
               (setf data-index 0)))
           (decode-opcode (c)
             (ldb (byte 3 0) c))
           (decode-dst-index (c)
             (ldb (byte 2 3) c))
           (decode-src-index (c)
             (ldb (byte 3 5) c)))
      (setf (ub64ref/le data 0) height
            (aref data 20) #xda)
      (loop do (let ((latency (make-array 9
                                          :element-type 'fixnum
                                          :initial-element 0))
                     (asic-latency (make-array 9
                                               :element-type 'fixnum
                                               :initial-element 0))
                     (inst-data (make-array 9
                                            :element-type 'fixnum
                                            :initial-contents '(0 1 2 3 #xffffff #xffffff
                                                                #xffffff #xffffff #xffffff)))
                     (alu-busy (make-array (list (1+ +total-latency+) +alu-count+)
                                           :element-type 'boolean
                                           :initial-element nil))
                     (is-rotation (make-array +instruction-count+
                                              :element-type 'boolean
                                              :initial-contents '(nil nil nil t t nil)))
                     (rotated (make-array 4
                                          :element-type 'boolean
                                          :initial-element nil))
                     (rotate-count 0)
                     (num-retries 0)
                     (total-iterations 0))
                 (setf code-size 0)
                 (loop while (and (or (< (aref latency 0) +total-latency+)
                                      (< (aref latency 1) +total-latency+)
                                      (< (aref latency 2) +total-latency+)
                                      (< (aref latency 3) +total-latency+))
                                  (< num-retries 64))
                       do (progn
                            (incf total-iterations)
                            (when (> total-iterations 256)
                              (return))
                            (check-data 1 data (length data))
                            (let* ((c (aref data data-index))
                                   (opcode (decode-opcode c))
                                   (dst-index (decode-dst-index c))
                                   (src-index (decode-src-index c)))
                              (incf data-index)
                              (cond
                                ((= opcode 5)
                                 (check-data 1 data (length data))
                                 (setf opcode (if (< (aref data data-index) 128) +ror+ +rol+))
                                 (incf data-index))
                                ((>= opcode 6)
                                 (setf opcode +xor+))
                                (t
                                 (setf opcode (if (<= opcode 2) +mul+ (- opcode 2)))))
                              (let ((a dst-index)
                                    (b src-index))
                                (when (and (or (= opcode +add+) (= opcode +sub+) (= opcode +xor+))
                                           (= a b))
                                  (setf b 8)
                                  (setf src-index 8))
                                (unless (or (and (aref is-rotation opcode) (aref rotated a))
                                            (and (/= opcode +mul+)
                                                 (= (logand (aref inst-data a) #xffff00)
                                                    (+ (ash opcode 8)
                                                       (ash (logand (aref inst-data b) 255) 16)))))
                                  (let ((next-latency (max (aref latency a) (aref latency b)))
                                        (alu-index -1))
                                    (loop while (< next-latency +total-latency+) do
                                      (loop for i from (1- (aref +op-alus+ opcode)) downto 0 do
                                        (unless (or (aref alu-busy next-latency i)
                                                    (and (= opcode +add+)
                                                         (aref alu-busy (1+ next-latency) i))
                                                    (and (aref is-rotation opcode)
                                                         (< next-latency (* rotate-count (aref +op-latency+ opcode)))))
                                          (setf alu-index i)
                                          (return)))
                                      (when (>= alu-index 0)
                                        (return))
                                      (incf next-latency))
                                    (unless (> next-latency (+ (aref latency a) 7))
                                      (incf next-latency (aref +op-latency+ opcode))
                                      (if (<= next-latency +total-latency+)
                                          (progn
                                            (when (aref is-rotation opcode)
                                              (incf rotate-count))
                                            (setf (aref alu-busy (- next-latency (aref +op-latency+ opcode)) alu-index) t)
                                            (setf (aref latency a) next-latency)
                                            (setf (aref asic-latency a) (+ (max (aref asic-latency a) (aref asic-latency b))
                                                                           (aref +asic-op-latency+ opcode)))
                                            (setf (aref rotated a) (aref is-rotation opcode))
                                            (setf (aref inst-data a) (+ code-size
                                                                        (ash opcode 8)
                                                                        (ash (logand (aref inst-data b) 255) 16)))
                                            (setf (aref code code-size) (make-instruction :opcode opcode
                                                                                          :dst-index dst-index
                                                                                          :src-index src-index
                                                                                          :c 0))
                                            (when (= src-index 8)
                                              (setf r8-used t))
                                            (when (= opcode +add+)
                                              (setf (aref alu-busy (1+ (- next-latency (aref +op-latency+ opcode))) alu-index) t)
                                              (check-data 4 data (length data))
                                              (setf (instruction-c (aref code code-size)) (ub32ref/le data data-index))
                                              (incf data-index 4))
                                            (incf code-size)
                                            (when (>= code-size +num-instructions-min+)
                                              (return)))
                                          (incf num-retries)))))))))
                 (let ((prev-code-size code-size))
                   (loop while (and (< code-size +num-instructions-max+)
                                    (< (aref asic-latency 0) +total-latency+)
                                    (< (aref asic-latency 1) +total-latency+)
                                    (< (aref asic-latency 2) +total-latency+)
                                    (< (aref asic-latency 3) +total-latency+))
                         do (let* ((min-idx 0)
                                   (max-idx 0)
                                   (pattern (make-array 3
                                                        :element-type '(unsigned-byte 8)
                                                        :initial-contents (list +ror+ +mul+ +mul+)))
                                   (opcode (aref pattern (mod (- code-size prev-code-size) 3))))
                              (loop for i from 1 below 4 do
                                (when (< (aref asic-latency i) (aref asic-latency min-idx))
                                  (setf min-idx i))
                                (when (> (aref asic-latency i) (aref asic-latency max-idx))
                                  (setf max-idx i)))
                              (setf (aref latency min-idx) (+ (aref latency max-idx) (aref +op-latency+ opcode)))
                              (setf (aref asic-latency min-idx) (+ (aref asic-latency max-idx) (aref +asic-op-latency+ opcode)))
                              (setf (aref code code-size) (make-instruction :opcode opcode
                                                                            :dst-index min-idx
                                                                            :src-index max-idx
                                                                            :c 0))
                              (incf code-size)))))
            while (or (not r8-used)
                      (< code-size +num-instructions-min+)
                      (> code-size +num-instructions-max+)))
      (setf (aref code code-size) (make-instruction :opcode +ret+
                                                    :dst-index 0
                                                    :src-index 0
                                                    :c 0))
      code-size)))
