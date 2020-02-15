;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
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

(handler-case
    (use-foreign-library zbar)
  (load-foreign-library-error ()
    (warn "The zbar library was not found.")))

(defun decode-png (file)
  (if (foreign-library-loaded-p 'zbar)
      (flet ((flatten-array (array)
               (make-array (array-total-size array)
                           :displaced-to array
                           :element-type (array-element-type array)))
             (convert-to-rgb3 (data bit-depth)
               (let* ((data-length (length data))
                      (shift (- 8 bit-depth))
                      (rgb-data (make-array (* 3 data-length)
                                            :element-type '(unsigned-byte 8))))
                 (iter (for i from 0 below data-length)
                       (let ((v (ash (aref data i) shift)))
                         (setf (aref rgb-data (* 3 i)) v
                               (aref rgb-data (+ (* 3 i) 1)) v
                               (aref rgb-data (+ (* 3 i) 2)) v))
                       (finally (return rgb-data)))))
             (fourcc (fourcc)
               (logior (char-code (char fourcc 0))
                       (ash (char-code (char fourcc 1)) 8)
                       (ash (char-code (char fourcc 2)) 16)
                       (ash (char-code (char fourcc 3)) 24))))
        (let* ((png (png-read:read-png-file file))
               (width (png-read:width png))
               (height (png-read:height png))
               (image-data (png-read:image-data png))
               (data (if (= 3 (length (array-dimensions image-data)))
                         (flatten-array image-data)
                         (convert-to-rgb3 (flatten-array image-data)
                                          (png-read:bit-depth png))))
               (data-length (length data))
               (processor (foreign-funcall "zbar_processor_create" :pointer))
               (image (foreign-funcall "zbar_image_create" :pointer))
               result)
          (foreign-funcall "zbar_processor_init"
                           :pointer processor
                           :pointer (null-pointer)
                           :int 0
                           :int)
          (foreign-funcall "zbar_image_set_format"
                           :pointer image
                           :unsigned-long (fourcc "RGB3"))
          (foreign-funcall "zbar_image_set_size"
                           :pointer image
                           :unsigned-int width
                           :unsigned-int height)
          (let ((tmp image))
            (with-foreign-object (raw-data :unsigned-char data-length)
              (lisp-array->c-array data raw-data)
              (foreign-funcall "zbar_image_set_data"
                               :pointer image
                               :pointer raw-data
                               :unsigned-int data-length
                               :pointer (null-pointer))
              (setf image (foreign-funcall "zbar_image_convert"
                                           :pointer image
                                           :unsigned-long (fourcc "Y800")
                                           :pointer)))
            (foreign-funcall "zbar_image_destroy"
                             :pointer tmp))
          (foreign-funcall "zbar_process_image"
                           :pointer processor
                           :pointer image
                           :int)
          (do ((symbol (foreign-funcall "zbar_image_first_symbol"
                                        :pointer image
                                        :pointer)
                       (foreign-funcall "zbar_symbol_next"
                                        :pointer symbol
                                        :pointer)))
              ((null-pointer-p symbol))
            (let* ((data-length (foreign-funcall "zbar_symbol_get_data_length"
                                                 :pointer symbol
                                                 :unsigned-int))
                   (raw-data (foreign-funcall "zbar_symbol_get_data"
                                              :pointer symbol
                                              :pointer))
                   (data (c-array->lisp-array raw-data data-length)))
              (setf result (concatenate 'vector result data))))
          (foreign-funcall "zbar_image_destroy"
                           :pointer image)
          (foreign-funcall "zbar_processor_destroy"
                           :pointer processor)
          (bytes->string result)))
      (error "QR code decoding not supported because the zbar library was not found.")))

(defun decode-qr-code (png-file)
  "Return an alist containing the payment information of the QR code
in a PNG-FILE."
  (let ((uri (decode-png png-file)))
    (decode-uri uri)))
