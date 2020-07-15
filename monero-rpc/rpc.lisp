;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-rpc)


(defparameter *rpc-host* "127.0.0.1"
  "Location of the RPC server.")
(defparameter *rpc-port* 18081
  "Port on which the RPC server expects connections.")
(defparameter *rpc-user* nil
  "Username to use to connect to the RPC server.")
(defparameter *rpc-password* nil
  "Password to use to connect to the RPC server.")
(defparameter *rpc-client-secret-key* (generate-secret-key)
  "Secret key used to identify a client on the RPC server.")

(defun parse-digest-authentication-challenge (challenge)
  "Parse a 'Digest' authentication CHALLENGE received from a HTTP server."
  (let* ((tmp (split-sequence #\space challenge))
         (authentication-type (first tmp))
         (authentication-info (split-sequence #\, (second tmp))))
    (unless (string-equal authentication-type "digest")
      (error "Bad digest authentication challenge."))
    (do ((info authentication-info (cdr info))
         (result '()))
        ((null info) result)
      (let* ((index (position #\= (car info)))
             (parameter (when index
                          (list (subseq (car info) 0 index)
                                (subseq (car info) (1+ index))))))
        (when parameter
          (let ((key (intern (string-upcase (first parameter)) :keyword))
                (value (remove #\" (second parameter))))
            (setf result (acons key value result))))))))

(defun compute-digest-authentication-response (user password qop algorithm realm method uri nonce)
  "Build the reponse to a 'Digest' authentication challenge."
  (unless (string-equal qop "auth")
    (error "Unsupported quality of protection."))
  (labels ((bytes->hex (bytes)
             (string-downcase (bytes->hex-string bytes)))
           (digest (string)
             (let ((algo (intern (string-upcase algorithm) :keyword)))
               (bytes->hex (digest-sequence algo (utf-8-string->bytes string))))))
    (let* ((ha1 (digest (format nil "~a:~a:~a" user realm password)))
           (ha2 (digest (format nil "~a:~a" method uri)))
           (nc "00000001")
           (cnonce (usb8-array-to-base64-string (random-data 16)))
           (response (digest (format nil "~a:~a:~a:~a:~a:~a" ha1 nonce nc cnonce qop ha2))))
      (concatenate 'string
                   "Digest username=\"" user "\""
                   ", realm=\"" realm "\""
                   ", nonce=\"" nonce "\""
                   ", uri=\"" uri "\""
                   ", cnonce=\"" cnonce "\""
                   ", nc=" nc
                   ", qop=" qop
                   ", response=\"" response "\""
                   ", algorithm=\"" algorithm "\""))))

(defun generate-rpc-payment-signature (secret-key)
  "Make a signature of the current time with using a SECRET-KEY."
  (let* ((public-key (secret-key->public-key secret-key))
         (timestamp (format nil "~16,'0x" (* (get-unix-time) 1000000)))
         (hash (fast-hash (string->bytes timestamp)))
         (signature (generate-signature hash secret-key)))
    (concatenate 'string
                 (bytes->hex-string public-key)
                 timestamp
                 (bytes->hex-string signature))))

(defun rpc (method &key raw parameters (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  "Send a METHOD RPC request to the server at RPC-HOST:RPC-PORT with optional
PARAMETERS."
  (let* ((page (format nil "/~(~a~)" method))
         (parameters (when parameters
                       (encode-json-to-string parameters)))
         (server-uri (format nil "http://~a:~d~a" rpc-host rpc-port page))
         (auth (handler-case (progn (dex:head server-uri) nil)
                 (dex:http-request-unauthorized (e)
                   (unless (and rpc-user rpc-password)
                     (error "USER and PASSWORD required."))
                   (let* ((response-headers (dex:response-headers e))
                          (www-authenticate (gethash "www-authenticate" response-headers))
                          (challenge (parse-digest-authentication-challenge www-authenticate))
                          (qop (geta challenge :qop))
                          (algorithm (geta challenge :algorithm))
                          (realm (geta challenge :realm))
                          (nonce (geta challenge :nonce)))
                     (compute-digest-authentication-response rpc-user
                                                             rpc-password
                                                             qop
                                                             algorithm
                                                             realm
                                                             "POST"
                                                             page
                                                             nonce)))
                 (t () nil))))
    (multiple-value-bind (body status response-headers uri stream)
        (dex:post server-uri
                  :headers (if auth
                               (list (cons "authorization" auth)
                                     (cons "content-type" "application/json"))
                               (list (cons "content-type" "application/json")))
                  :content parameters
                  :force-binary raw)
      (declare (ignore status response-headers uri stream))
      (if raw body (decode-json-from-string body)))))

(defun json-rpc (method &key parameters (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  "Send a METHOD JSON RPC request to RPC-HOST:RPC-PORT with optional
PARAMETERS."
  (let* ((request (list (cons :jsonrpc "2.0")
                        (cons :id (random 1000000))
                        (cons :method (string-downcase (string method)))
                        (cons :params parameters)))
         (answer (rpc "json_rpc"
                      :parameters request
                      :rpc-host rpc-host
                      :rpc-port rpc-port
                      :rpc-user rpc-user
                      :rpc-password rpc-password))
         (err (geta answer :error)))
    (if err
        (error (geta err :message))
        (geta answer :result))))

(defmacro defhttprpc (type name (method &rest args) &body body)
  (assert (<= (length body) 3))
  (let* ((key-args-p (member '&key args))
         (docstring (when (stringp (car body))
                      (pop body)))
         (parameters (when body
                       (pop body)))
         (postprocess (car body)))
    `(defun ,name (,@args ,@(unless key-args-p (list '&key))
                   (rpc-host *rpc-host*) (rpc-port *rpc-port*)
                   (rpc-user *rpc-user*) (rpc-password *rpc-password*)
                   (rpc-client-secret-key *rpc-client-secret-key*))
       ,@(list docstring)
       (let* ((client (generate-rpc-payment-signature rpc-client-secret-key))
              (parameters (acons "client" client ,parameters))
              (result (,(case type
                           ((:json) 'json-rpc)
                           (t 'rpc))
                       ,method
                       ,@(cond
                           ((eql type :raw) (list :raw t)))
                       :parameters parameters
                       :rpc-host rpc-host
                       :rpc-port rpc-port
                       :rpc-user rpc-user
                       :rpc-password rpc-password)))
         ,(if postprocess
              `(funcall ,postprocess result)
              `result)))))

;; def*rpc macros usage:
;;
;; (def*rpc my-rpc ("my_method" arg1 arg2 arg3 &key optarg1 optarg2)
;;   "my docstring" ; optional
;;   (make-parameters-form arg1 arg2 arg3 optarg1 optarg2) ; optional unless postprocess
;;   (lambda (result) (process result))) ; optional postprocess
;;
;; (my-rpc arg1 arg2 arg3 :optarg2 123)

(defmacro defrpc (name (method &rest args) &body body)
  `(defhttprpc :rpc ,name (,method ,@args) ,@body))

(defmacro defrawrpc (name (method &rest args) &body body)
  `(defhttprpc :raw ,name (,method ,@args) ,@body))

(defmacro defjsonrpc (name (method &rest args) &body body)
  `(defhttprpc :json ,name (,method ,@args) ,@body))
