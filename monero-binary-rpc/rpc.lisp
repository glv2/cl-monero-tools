;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-binary-rpc)


(defun binary-rpc (method &key raw parameters (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  "Send a METHOD binary RPC request to the server at RPC-HOST:RPC-PORT with
optional PARAMETERS."
  (let* ((page (format nil "/~(~a~)" method))
         (parameters (when parameters
                       (serialize-to-binary-storage parameters)))
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
                                     (cons "content-type" "application/octet-stream"))
                               (list (cons "content-type" "application/octet-stream")))
                  :content parameters
                  :force-binary raw)
      (declare (ignore status response-headers uri stream))
      (deserialize-from-binary-storage body 0))))

(defmacro defbinrpc (name (method &rest args) &body body)
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
                   (rpc-client-secret-key (or *rpc-client-secret-key*
                                              (generate-secret-key))))
       ,@(list docstring)
       (let* ((client (generate-rpc-payment-signature rpc-client-secret-key))
              (parameters (acons "client" client ,parameters))
              (result (binary-rpc
                       ,method
                       :parameters parameters
                       :rpc-host rpc-host
                       :rpc-port rpc-port
                       :rpc-user rpc-user
                       :rpc-password rpc-password)))
         ,(if postprocess
              `(funcall ,postprocess result)
              `result)))))
