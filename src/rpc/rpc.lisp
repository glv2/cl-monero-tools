;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defparameter *rpc-host* "127.0.0.1")
(defparameter *rpc-port* 18081)
(defparameter *rpc-user* nil)
(defparameter *rpc-password* nil)

(defun parse-digest-authentication-challenge (challenge)
  "Parse a 'Digest' authentication CHALLENGE received from a HTTP server."
  (let* ((tmp (split-sequence #\space challenge))
         (authentication-type (first tmp))
         (authenticatio-info (split-sequence #\, (second tmp))))
    (unless (string-equal authentication-type "digest")
      (error "Bad digest authentication challenge."))
    (do ((info authenticatio-info (cdr info))
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
               (bytes->hex (ironclad:digest-sequence algo (string->bytes string))))))
    (let* ((ha1 (digest (format nil "~a:~a:~a" user realm password)))
           (ha2 (digest (format nil "~a:~a" method uri)))
           (nc "00000001")
           (cnonce (usb8-array-to-base64-string (ironclad:random-data 16)))
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

;;(defun rpc (host port user password method &optional parameters)
(defun rpc (method &key parameters (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  "Send a METHOD RPC request to the server at HOST:PORT with optional
PARAMETERS."
  (let* ((page (format nil "/~(~a~)" method))
         (parameters (when parameters
                       (encode-json-to-string parameters)))
         (server-uri (format nil "http://~a:~d~a" host port page))
         (auth (handler-case (progn (dex:head server-uri) nil)
                 (dex:http-request-unauthorized (e)
                   (unless (and user password)
                     (error "USER and PASSWORD required."))
                   (let* ((response-headers (dex:response-headers e))
                          (www-authenticate (gethash "www-authenticate" response-headers))
                          (challenge (parse-digest-authentication-challenge www-authenticate))
                          (qop (geta challenge :qop))
                          (algorithm (geta challenge :algorithm))
                          (realm (geta challenge :realm))
                          (nonce (geta challenge :nonce)))
                     (compute-digest-authentication-response user
                                                             password
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
                  :content parameters)
      (declare (ignore status response-headers uri stream))
      (decode-json-from-string body))))

(defun json-rpc (method &key parameters (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
;;(defun json-rpc (host port user password method &optional parameters)
  "Send a METHOD JSON-RPC request to HOST:PORT with optional PARAMETERS."
  (let* ((request (list (cons :jsonrpc "2.0")
                        (cons :id (random 1000000))
                        (cons :method (string-downcase (string method)))
                        (cons :params parameters)))
         (answer (rpc "json_rpc"
                      :parameters request
                      :host host
                      :port port
                      :user user
                      :password password))
         (err (geta answer :error)))
    (if err
        (error (geta err :message))
        (geta answer :result))))
