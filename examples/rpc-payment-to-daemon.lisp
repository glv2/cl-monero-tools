;;; Make a RPC request to a Monero node requiring payment.
;;; If we don't have credits on this node, generate some.
;;; Mining credits requires a running randomx-service daemon.

(asdf:load-system "ironclad")
(asdf:load-system "iterate")
(asdf:load-system "monero-rpc")
(asdf:load-system "monero-tools")
(asdf:load-system "monero-utils")

(use-package :iterate)
(use-package :monero-rpc)
(use-package :monero-daemon-rpc)
(use-package :monero-tools)
(use-package :monero-utils)

(defun need-credits-p (access-info)
  (let ((credits (geta access-info :credits)))
    (zerop credits)))

(defun maybe-reseed-randomx-service (seed)
  (let* ((randomx-info (randomx-service-info))
         (randomx-seed (hex-string->bytes (geta randomx-info :seed))))
    (unless (equalp seed randomx-seed)
      (randomx-service-reseed seed))))

(defun hash-templates (templates seed)
  (maybe-reseed-randomx-service seed)
  (randomx-service-hash-batch templates :seed seed))

(defun mine-credits (access-info)
  (let* ((cookie (geta access-info :cookie))
         (difficulty (geta access-info :diff))
         (template (hex-string->bytes (geta access-info :hashing-blob)))
         (seed (hex-string->bytes (geta access-info :seed-hash)))
         (nonces (iter (repeat 256)
                       (collect (ironclad:random-data 4))))
         (templates (mapcar (lambda (template nonce)
                              (modify-block-template template nonce))
                            (make-list 256 :initial-element template)
                            nonces))
         (hashes (hash-templates templates seed))
         (good-nonces (iter (for hash in hashes)
                            (for nonce in nonces)
                            (when (acceptable-hash-p hash difficulty)
                              (collect (bytes->integer nonce))))))
    (iter (for nonce in good-nonces)
          (rpc-access-submit-nonce nonce cookie))))

(defun maybe-mine-credits ()
  (iter (for access-info next (rpc-access-info))
        (while (need-credits-p access-info))
        (mine-credits access-info)))

(let ((*randomx-service-host* "localhost")
      (*randomx-service-port* 39093)
      (*rpc-host* "some-monero-node")
      (*rpc-port* 18081)
      (*rpc-user* "rpc-user-if-necessary")
      (*rpc-password* "rpc-password-if-necessary")
      (*rpc-client-secret-key* (generate-secret-key)))
  ;; In this example, a new *rpc-client-secret-key* is generated every time,
  ;; as if each request was done by a different client.
  ;; You should instead generate a secret key once, save it, and reuse it for
  ;; every RPC request you make. This will allow you to use the credits you
  ;; still have on the Monero node instead of having to mine some for every
  ;; request.
  (maybe-mine-credits)
  (iter (for (key . value) in (get-info))
        (format t "~(~a~): ~a~%" key value)))
