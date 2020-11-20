;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


#+(and sbcl x86-64)
(defpackage :monero-tools-vm
  (:use :cl :sb-c :sb-assem :sb-vm)
  (:shadow #:ea)
  (:import-from :sb-vm
                #:descriptor-reg
                #:double-reg
                #:unsigned-reg
                #:unsigned-num
                #:simple-array-unsigned-byte-8
                #:simple-array-unsigned-byte-32))

#+(and sbcl x86-64)
(in-package :monero-tools-vm)


#+(and sbcl x86-64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (fboundp 'sb-vm::ea)
      (setf (fdefinition 'ea) (fdefinition 'sb-vm::ea)) ; Newer SBCL (>= 1.4.11)
      (defun ea (displacement &optional base index (scale 1)) ; Older SBCL (< 1.4.11)
        (sb-vm::make-ea :qword
                        :base base
                        :index index
                        :scale scale
                        :disp (or displacement 0))))
  (when (crypto::aes-ni-supported-p)
    (pushnew :aes-ni *features*)))

#+(and sbcl x86-64 aes-ni)
(defknown monero-tools::pseudo-aes-expand-key/aes-ni
  ((simple-array (unsigned-byte 8) (*))
   (simple-array (unsigned-byte 32) (*)))
  (values)
  (any)
  :overwrite-fndb-silently t)

#+(and sbcl x86-64 aes-ni)
(define-vop (pseudo-aes-expand-key/aes-ni)
  (:translate monero-tools::pseudo-aes-expand-key/aes-ni)
  (:policy :fast-safe)
  (:args (key :scs (descriptor-reg))
         (encryption-keys :scs (descriptor-reg)))
  (:arg-types simple-array-unsigned-byte-8
              simple-array-unsigned-byte-32)
  (:temporary (:sc double-reg) x0 x1 x2 x3)
  (:generator 1000
    (flet ((buffer-mem (base i)
             (let ((disp (+ (- (* n-word-bytes vector-data-offset)
                               other-pointer-lowtag)
                            (* 16 i))))
               (ea disp base)))
           (expand-key-256a ()
             (inst pshufd x1 x1 #b11111111)
             (inst shufps x2 x0 #b00010000)
             (inst pxor x0 x2)
             (inst shufps x2 x0 #b10001100)
             (inst pxor x0 x2)
             (inst pxor x0 x1))
           (expand-key-256b ()
             (inst pshufd x1 x1 #b10101010)
             (inst shufps x2 x3 #b00010000)
             (inst pxor x3 x2)
             (inst shufps x2 x3 #b10001100)
             (inst pxor x3 x2)
             (inst pxor x3 x1)))
      (inst pxor x2 x2)
      (inst movdqu x0 (buffer-mem key 0))
      (inst movdqu x3 (buffer-mem key 1))
      (inst movdqu (buffer-mem encryption-keys 0) x0)
      (inst movdqu (buffer-mem encryption-keys 1) x3)
      (inst aeskeygenassist x1 x3 1)
      (expand-key-256a)
      (inst movdqu (buffer-mem encryption-keys 2) x0)
      (inst aeskeygenassist x1 x0 1)
      (expand-key-256b)
      (inst movdqu (buffer-mem encryption-keys 3) x3)
      (inst aeskeygenassist x1 x3 2)
      (expand-key-256a)
      (inst movdqu (buffer-mem encryption-keys 4) x0)
      (inst aeskeygenassist x1 x0 2)
      (expand-key-256b)
      (inst movdqu (buffer-mem encryption-keys 5) x3)
      (inst aeskeygenassist x1 x3 4)
      (expand-key-256a)
      (inst movdqu (buffer-mem encryption-keys 6) x0)
      (inst aeskeygenassist x1 x0 4)
      (expand-key-256b)
      (inst movdqu (buffer-mem encryption-keys 7) x3)
      (inst aeskeygenassist x1 x3 8)
      (expand-key-256a)
      (inst movdqu (buffer-mem encryption-keys 8) x0)
      (inst aeskeygenassist x1 x0 8)
      (expand-key-256b)
      (inst movdqu (buffer-mem encryption-keys 9) x3))))

#+(and sbcl x86-64 aes-ni)
(defknown monero-tools::pseudo-aes-round/aes-ni
  ((simple-array (unsigned-byte 8) (*))
   (unsigned-byte 32)
   (simple-array (unsigned-byte 8) (*))
   (unsigned-byte 32)
   (simple-array (unsigned-byte 8) (*)))
  (values)
  (any)
  :overwrite-fndb-silently t)

#+(and sbcl x86-64 aes-ni)
(define-vop (pseudo-aes-round/aes-ni)
  (:translate monero-tools::pseudo-aes-round/aes-ni)
  (:policy :fast-safe)
  (:args (in :scs (descriptor-reg))
         (in-start :scs (unsigned-reg))
         (out :scs (descriptor-reg))
         (out-start :scs (unsigned-reg))
         (round-key :scs (descriptor-reg)))
  (:arg-types simple-array-unsigned-byte-8
              unsigned-num
              simple-array-unsigned-byte-8
              unsigned-num
              simple-array-unsigned-byte-8)
  (:temporary (:sc double-reg) x0 x1)
  (:generator 1000
    (let ((disp (- (* n-word-bytes vector-data-offset)
                   other-pointer-lowtag)))
      (flet ((buffer-mem (base offset)
               (ea disp base offset))
             (round-key ()
               (ea disp round-key)))
        (inst movdqu x0 (buffer-mem in in-start))
        (inst movdqu x1 (round-key))
        (inst aesenc x0 x1)
        (inst movdqu (buffer-mem out out-start) x0)))))

#+(and sbcl x86-64 aes-ni)
(defknown monero-tools::pseudo-aes-rounds/aes-ni
  ((simple-array (unsigned-byte 8) (*))
   (unsigned-byte 32)
   (simple-array (unsigned-byte 8) (*))
   (unsigned-byte 32)
   (simple-array (unsigned-byte 32) (*)))
  (values)
  (any)
  :overwrite-fndb-silently t)

#+(and sbcl x86-64 aes-ni)
(define-vop (pseudo-aes-rounds/aes-ni)
  (:translate monero-tools::pseudo-aes-rounds/aes-ni)
  (:policy :fast-safe)
  (:args (in :scs (descriptor-reg))
         (in-start :scs (unsigned-reg))
         (out :scs (descriptor-reg))
         (out-start :scs (unsigned-reg))
         (round-keys :scs (descriptor-reg)))
  (:arg-types simple-array-unsigned-byte-8
              unsigned-num
              simple-array-unsigned-byte-8
              unsigned-num
              simple-array-unsigned-byte-32)
  (:temporary (:sc double-reg) x0 x1)
  (:generator 1000
    (flet ((buffer-mem (base offset)
             (let ((disp (- (* n-word-bytes vector-data-offset)
                            other-pointer-lowtag)))
               (ea disp base offset)))
           (round-key (i)
             (let ((disp (+ (- (* n-word-bytes vector-data-offset)
                               other-pointer-lowtag)
                            (* 16 i))))
               (ea disp round-keys))))
      (inst movdqu x0 (buffer-mem in in-start))
      (inst movdqu x1 (round-key 0))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 1))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 2))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 3))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 4))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 5))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 6))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 7))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 8))
      (inst aesenc x0 x1)
      (inst movdqu x1 (round-key 9))
      (inst aesenc x0 x1)
      (inst movdqu (buffer-mem out out-start) x0))))
