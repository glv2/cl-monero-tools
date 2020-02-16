;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun pseudo-aes-expand-key (state state-start)
  (declare (type (simple-array (unsigned-byte 8) (*)) state)
           (type fixnum state-start)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let* ((key (make-array 32 :element-type '(unsigned-byte 8)))
         (encryption-keys (allocate-round-keys key)))
    (declare (type (simple-array (unsigned-byte 8) (32)) key)
             (type aes-round-keys encryption-keys)
             (dynamic-extent key))
    (replace key state :start2 state-start)
    #+(and sbcl x86-64 aes-ni)
    (pseudo-aes-expand-key/aes-ni key encryption-keys)
    #-(and sbcl x86-64 aes-ni)
    (generate-round-keys-for-encryption key encryption-keys)
    encryption-keys))

(macrolet ((rk-ref (x)
             `(aref round-keys ,x))
           (mix (rk a0 a1 a2 a3 sym0 sym1 sym2 sym3)
             `(logxor (aref ,a0 (fourth-byte ,sym0))
                      (aref ,a1 (third-byte ,sym1))
                      (aref ,a2 (second-byte ,sym2))
                      (aref ,a3 (first-byte ,sym3))
                      (rk-ref ,rk)))
           (mix-s-into-t-encrypting (offset)
             `(setf t0 (mix ,offset Te0 Te1 Te2 Te3 s0 s1 s2 s3)
                    t1 (mix ,(1+ offset) Te0 Te1 Te2 Te3 s1 s2 s3 s0)
                    t2 (mix ,(+ offset 2) Te0 Te1 Te2 Te3 s2 s3 s0 s1)
                    t3 (mix ,(+ offset 3) Te0 Te1 Te2 Te3 s3 s0 s1 s2)))
           (mix-t-into-s-encrypting (offset)
             `(setf s0 (mix ,offset Te0 Te1 Te2 Te3 t0 t1 t2 t3)
                    s1 (mix ,(1+ offset) Te0 Te1 Te2 Te3 t1 t2 t3 t0)
                    s2 (mix ,(+ offset 2) Te0 Te1 Te2 Te3 t2 t3 t0 t1)
                    s3 (mix ,(+ offset 3) Te0 Te1 Te2 Te3 t3 t0 t1 t2))))
  (defun pseudo-aes-round (in in-start out out-start round-key)
    (declare (type (simple-array (unsigned-byte 8) (*)) in out)
             (type (simple-array (unsigned-byte 8) (16)) round-key)
             (type (unsigned-byte 32) in-start out-start)
             (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    #+(and sbcl x86-64 aes-ni)
    (pseudo-aes-round/aes-ni in in-start out out-start round-key)
    #-(and sbcl x86-64 aes-ni)
    (let ((round-keys (make-array 4 :element-type '(unsigned-byte 32))))
      (declare (type (simple-array (unsigned-byte 32) (4)) round-keys)
               (dynamic-extent round-keys))
      (setf (aref round-keys 0) (ub32ref/be round-key 0))
      (setf (aref round-keys 1) (ub32ref/be round-key 4))
      (setf (aref round-keys 2) (ub32ref/be round-key 8))
      (setf (aref round-keys 3) (ub32ref/be round-key 12))
      (with-words ((s0 s1 s2 s3) in in-start)
        (let ((t0 0) (t1 0) (t2 0) (t3 0))
          (declare (type (unsigned-byte 32) t0 t1 t2 t3))
          (mix-s-into-t-encrypting 0)
          (store-words out out-start t0 t1 t2 t3)))))

  (defun pseudo-aes-rounds (in in-start out out-start round-keys)
    (declare (type (simple-array (unsigned-byte 8) (*)) in out)
             (type aes-round-keys round-keys)
             (type (unsigned-byte 32) in-start out-start)
             (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    #+(and sbcl x86-64 aes-ni)
    (pseudo-aes-rounds/aes-ni in in-start out out-start round-keys)
    #-(and sbcl x86-64 aes-ni)
    (with-words ((s0 s1 s2 s3) in in-start)
      (let ((t0 0) (t1 0) (t2 0) (t3 0))
        (declare (type (unsigned-byte 32) t0 t1 t2 t3))
        (mix-s-into-t-encrypting 0)
        (mix-t-into-s-encrypting 4)
        (mix-s-into-t-encrypting 8)
        (mix-t-into-s-encrypting 12)
        (mix-s-into-t-encrypting 16)
        (mix-t-into-s-encrypting 20)
        (mix-s-into-t-encrypting 24)
        (mix-t-into-s-encrypting 28)
        (mix-s-into-t-encrypting 32)
        (mix-t-into-s-encrypting 36)
        (store-words out out-start s0 s1 s2 s3)))))