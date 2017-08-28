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
         (c (bytes->integer signature :start 0 :end +key-length+))
         (r (bytes->integer signature :start +key-length+))
         (k-point (point+ (point* +g+ r) (point* p c)))
         (k-data (point->bytes k-point))
         (h-data (hash-to-scalar (concatenate 'octet-vector data public-key k-data)))
         (h (bytes->integer h-data)))
    (= c h)))

(defun compute-key-image (secret-key &optional (public-key (secret-key->public-key secret-key)))
  "Compute the key image of a SECRET-KEY."
  (check-type public-key (octet-vector #.+key-length+))
  (check-type secret-key (octet-vector #.+key-length+))
  (let* ((h (hash-to-point public-key))
         (p (bytes->point h))
         (s (bytes->integer secret-key)))
    (point->bytes (point* p s))))

(defun interleave-vectors (a b)
  "Take two vectors of same length #(a1 a2 a3...) and #(b1 b2 b3...),
and return the vector #(a1 b1 a2 b2 a3 b3...)."
  (check-type a vector)
  (check-type b vector)
  (let* ((n (length a))
         (c (make-array (* 2 n))))
    (unless (= (length b) n)
      (error "The vectors don't have the same length."))
    (dotimes (i n c)
      (setf (aref c (* 2 i)) (aref a i)
            (aref c (+ (* 2 i) 1)) (aref b i)))))

(defun compute-non-interactive-challenge (data a b)
  "Compute the non-interactive challenge used in ring signatures."
  (check-type data octet-vector)
  (check-type a (vector point))
  (check-type b (vector point))
  (hash-to-scalar (apply #'concatenate
                         'octet-vector
                         data
                         (map 'list
                              #'point->bytes
                              (interleave-vectors a b)))))

(defun generate-ring-signature (data public-keys secret-key secret-index)
  "Return a ring signature of DATA by SECRET-KEY using a set of
PUBLIC-KEYS. The public key matching the SECRET-KEY must be at
SECRET-INDEX in the PUBLIC-KEYS vector."
  (check-type data octet-vector)
  (check-type public-keys (vector (octet-vector #.+key-length+)))
  (check-type secret-key (octet-vector #.+key-length+))
  (check-type secret-index (integer 0))
  (let* ((public-key (secret-key->public-key secret-key))
         (key-image (compute-key-image secret-key public-key))
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
    (let ((h (bytes->integer (compute-non-interactive-challenge data a b))))
      (setf (aref c secret-index) (mod (- h sum) +l+)
            (aref r secret-index) (mod (- (aref r secret-index) (* (aref c secret-index) s)) +l+))
      (apply #'concatenate
             'octet-vector
             (map 'list
                  (lambda (x)
                    (integer->bytes x :size +key-length+))
                  (interleave-vectors c r))))))

(defun valid-ring-signature-p (data public-keys key-image signature)
  "Return T if a ring SIGNATURE of DATA by the secret key matching
a KEY-IMAGE using a set of PUBLIC-KEYS is valid, and NIL otherwise."
  (check-type data octet-vector)
  (check-type public-keys (vector (octet-vector #.+key-length+)))
  (check-type key-image (octet-vector #.+key-length+))
  (check-type signature octet-vector)
  (let* ((n (length public-keys))
         (ki (bytes->point key-image))
         (a (make-array n))
         (b (make-array n))
         (sum 0))
    (unless (= (length signature) (* 2 n +key-length+))
      (error "Bad ring signature length."))
    (dotimes (i n)
      (let* ((p (bytes->point (aref public-keys i)))
             (hp (bytes->point (hash-to-point (aref public-keys i))))
             (index-c (* 2 i +key-length+))
             (index-r (* (+ (* 2 i) 1) +key-length+))
             (c (bytes->integer signature :start index-c :end index-r))
             (r (bytes->integer signature :start index-r :end (+ index-r +key-length+))))
        (setf (aref a i) (point+ (point* +g+ r) (point* p c))
              (aref b i) (point+ (point* hp r) (point* ki c))
              sum (mod (+ sum c) +l+))))
    (let ((h (bytes->integer (compute-non-interactive-challenge data a b))))
      (= h sum))))
