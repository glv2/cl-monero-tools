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

(handler-case
    (use-foreign-library zbar)
  (load-foreign-library-error ()
    (warn "The zbar library was not found.")))

(defun decode-png (file)
  (if (foreign-library-loaded-p 'zbar)
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
                (dotimes (i data-length)
                  (setf (mem-aref raw-data :unsigned-char i) (aref data i)))
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
                     (data (make-array data-length)))
                (dotimes (i data-length)
                  (setf (aref data i) (mem-aref raw-data :unsigned-char i)))
                (setf result (concatenate 'vector result data))))
            (foreign-funcall "zbar_image_destroy"
                             :pointer image)
            (foreign-funcall "zbar_processor_destroy"
                             :pointer processor)
            (map 'string #'code-char result))))
      (error "QR code decoding not supported because the zbar library was not found.")))

(defun decode-qr-code (png-file)
  "Return an alist containing the payment information of the QR code
in a PNG-FILE."
  (let ((uri (decode-png png-file)))
    (decode-uri uri)))
