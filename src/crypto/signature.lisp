;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun generate-signature (data secret-key)
  "Return a Schnorr signature of DATA by SECRET-KEY."
  (check-type data octet-vector)
  (check-type secret-key (octet-vector #.+key-length+))
  (let* ((public-key (secret-key->public-key secret-key))
         (s (bytes->integer secret-key))
         (k-data (random-scalar))
         (k (bytes->integer k-data))
         (k-point (secret-key->public-key k-data))
         (c-data (hash-to-scalar (concatenate 'octet-vector data public-key k-point)))
         (c (bytes->integer c-data))
         (r-data (integer->bytes (mod (- k (* c s)) +l+) :size +key-length+)))
    (concatenate 'octet-vector c-data r-data)))

(defun valid-signature-p (data public-key signature)
  "Return T if a Schnorr SIGNATURE of DATA by the secret key matching
a PUBLIC-KEY is valid, and NIL otherwise."
  (check-type data octet-vector)
  (check-type public-key (octet-vector #.+key-length+))
  (check-type signature (octet-vector #.(* 2 +key-length+)))
  (let* ((p (bytes->point public-key))
         (c (bytes->integer (subseq signature 0 +key-length+)))
         (r (bytes->integer (subseq signature +key-length+)))
         (k-point (point+ (point* +g+ r) (point* p c)))
         (k-data (point->bytes k-point))
         (h-data (hash-to-scalar (concatenate 'octet-vector data public-key k-data)))
         (h (bytes->integer h-data)))
    (= c h)))

(defun generate-key-image (secret-key &optional (public-key (secret-key->public-key secret-key)))
  "Compute the key image of a SECRET-KEY."
  (check-type public-key (octet-vector #.+key-length+))
  (check-type secret-key (octet-vector #.+key-length+))
  (let* ((h (hash-to-point public-key))
         (p (bytes->point h))
         (s (bytes->integer secret-key)))
    (point->bytes (point* p s))))

(defun generate-ring-signature (data public-keys secret-key secret-index)
  (let* ((public-key (secret-key->public-key secret-key))
         (key-image (generate-key-image secret-key public-key))
         (ki (bytes->point key-image))
         (n (length public-keys))
         (s (bytes->integer secret-key))
         (a (make-array n))
         (b (make-array n))
         (c (make-array n))
         (r (make-array n))
         (sum 0))
    (dotimes (i n)
      (let ((p (bytes->point (aref public-keys i)))
            (hp (bytes->point (hash-to-point (aref public-keys i))))
            (q (1+ (ironclad:strong-random (1- +l+))))
            (w (1+ (ironclad:strong-random (1- +l+)))))
        (if (= i secret-index)
            (setf (aref a i) (point* +g+ q)
                  (aref b i) (point* hp q)
                  (aref r i) q)
            (setf (aref a i) (point+ (point* +g+ q) (point* p w))
                  (aref b i) (point+ (point* hp q) (point* ki w))
                  (aref c i) w
                  (aref r i) q
                  sum (mod (+ sum w) +l+)))))
    ;; TODO: hash a1 a2 a3 b1 b2 b3 or a1 b1 a2 b2 a3 b3 ?
    (let ((h (bytes->integer (hash-to-scalar (concatenate 'octet-vector
                                                          data
                                                          (map 'vector #'point->bytes a)
                                                          (map 'vector #'point->bytes b))))))
      (setf (aref c secret-index) (mod (- h sum) +l+)
            (aref r secret-index) (mod (- (aref r secret-index) (* (aref c secret-index) s)) +l+))
      (list key-image
            (map 'vector (lambda (n) (integer->bytes n :size +key-length+)) c)
            (map 'vector (lambda (n) (integer->bytes n :size +key-length+)) r)))))

(defun valid-ring-signature-p (data public-keys key-image signature)
  (let* ((n (length public-keys))
         (ki (bytes->point key-image))
         (a (make-array n))
         (b (make-array n))
         (c (map 'vector #'bytes->integer (second signature)))
         (r (map 'vector #'bytes->integer (third signature)))
         (sum 0))
    (dotimes (i n)
      (let ((p (bytes->point (aref public-keys i)))
            (hp (bytes->point (hash-to-point (aref public-keys i)))))
        (setf (aref a i) (point+ (point* +g+ (aref r i)) (point* p (aref c i)))
              (aref b i) (point+ (point* hp (aref r i)) (point* ki (aref c i)))
              sum (mod (+ sum (aref c i)) +l+))))
    (let ((h (bytes->integer (hash-to-scalar (concatenate 'octet-vector
                                                          data
                                                          (map 'vector #'point->bytes a)
                                                          (map 'vector #'point->bytes b))))))
      (= h sum))))
