;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun make-qr-code (png-file address &key payment-id recipient-name amount description)
  "Write the QR code containing the information for a payment to
a PNG-FILE."
  (let ((uri (make-uri address
                       :payment-id payment-id
                       :recipient-name recipient-name
                       :amount amount
                       :description description)))
    (cl-qrencode:encode-png uri :fpath png-file :mode :byte :pixsize 4)))

(define-foreign-library zbar
  (t (:default "libzbar")))

(use-foreign-library zbar)

(defcfun ("zbar_image_create" zbar-image-create) :pointer)

(defcfun ("zbar_image_destroy" zbar-image-destroy) :void
  (image :pointer))

(defcfun ("zbar_image_set_format" zbar-image-set-format) :void
  (image :pointer)
  (format :unsigned-long))

(defcfun ("zbar_image_set_size" zbar-image-set-size) :void
  (image :pointer)
  (width :unsigned-int)
  (height :unsigned-int))

(defcfun ("zbar_image_set_data" zbar-image-set-data) :void
  (image :pointer)
  (data :pointer)
  (data-length :unsigned-long)
  (cleanup-handler :pointer))

(defcfun ("zbar_image_free_data" zbar-image-free-data) :void
  (image :pointer))

(defcfun ("zbar_image_first_symbol" zbar-image-first-symbol) :pointer
  (image :pointer))

(defcfun ("zbar_image_convert" zbar-image-convert) :pointer
  (image :pointer)
  (format :unsigned-long))

(defcfun ("zbar_processor_create" zbar-processor-create) :pointer)

(defcfun ("zbar_processor_destroy" zbar-processor-destroy) :void
  (processor :pointer))

(defcfun ("zbar_processor_init" zbar-processor-init) :int
  (processor :pointer)
  (video-device :pointer :char)
  (enable-display :int))

(defcfun ("zbar_process_image" zbar-process-image) :int
  (processor :pointer)
  (image :pointer))

(defcfun ("zbar_symbol_get_data_length" zbar-symbol-get-data-length) :unsigned-int
  (symbol :pointer))

(defcfun ("zbar_symbol_get_data" zbar-symbol-get-data) :pointer
  (symbol :pointer))

(defcfun ("zbar_symbol_next" zbar-symbol-next) :pointer
  (symbol :pointer))

(defun decode-png (file)
  (flet ((fourcc (fourcc)
           (logior (char-code (char fourcc 0))
                   (ash (char-code (char fourcc 1)) 8)
                   (ash (char-code (char fourcc 2)) 16)
                   (ash (char-code (char fourcc 3)) 24))))
    (pngload:with-png-in-static-vector (png file)
      (let* ((width (pngload:width png))
             (height (pngload:height png))
             (data (pngload:data png))
             (data-length (length data))
             (processor (zbar-processor-create))
             (image (zbar-image-create))
             result)
        (zbar-processor-init processor (null-pointer) 0)
        (zbar-image-set-format image (fourcc "RGB3"))
        (zbar-image-set-size image width height)
        (let ((tmp image))
          (with-foreign-object (raw-data :unsigned-char data-length)
            (dotimes (i data-length)
              (setf (mem-aref raw-data :unsigned-char i) (aref data i)))
            (zbar-image-set-data image raw-data data-length (null-pointer))
            (setf image (zbar-image-convert image (fourcc "Y800"))))
          (zbar-image-destroy tmp))
        (zbar-process-image processor image)
        (do ((symbol (zbar-image-first-symbol image)
                     (zbar-symbol-next symbol)))
            ((null-pointer-p symbol))
          (let* ((data-length (zbar-symbol-get-data-length symbol))
                 (raw-data (zbar-symbol-get-data symbol))
                 (data (make-array data-length)))
            (dotimes (i data-length)
              (setf (aref data i) (mem-aref raw-data :unsigned-char i)))
            (setf result (concatenate 'vector result data))))
        (zbar-image-destroy image)
        (zbar-processor-destroy processor)
        (map 'string #'code-char result)))))

(defun decode-qr-code (png-file)
  "Return an alist containing the payment information of the QR code
in a PNG-FILE."
  (let ((uri (decode-png png-file)))
    (decode-uri uri)))
