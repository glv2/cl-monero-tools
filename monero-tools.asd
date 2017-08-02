;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+quicklisp (ql:quickload "trivial-features")
  #-quicklisp (asdf:load-system "trivial-features"))

(defsystem "monero-tools"
  :name "monero-tools"
  :description "Tools to work with the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("alexandria"
               "babel"
               "bordeaux-threads"
               "cffi"
               "cl-base64"
               "cl-json"
               "dexador"
               "ironclad"
               "split-sequence"
               "trivial-features")
  :in-order-to ((test-op (test-op "monero-tools/tests")))
  :components ((:module "src"
                :components ((:file "package")
                             (:module "blockchain"
                              :depends-on ("crypto" "package" "serialization" "utils")
                              :components ((:file "block" :depends-on ("transaction"))
                                           (:file "transaction")))
                             (:module "crypto"
                              :depends-on ("package" "utils")
                              :components ((:file "crypto")
                                           (:file "key" :depends-on ("crypto"))))
                             (:module "mnemonic"
                              :depends-on ("package" "utils")
                              :components ((:file "chinese-simplified" :depends-on ("mnemonic"))
                                           (:file "dutch" :depends-on ("mnemonic"))
                                           (:file "english" :depends-on ("mnemonic"))
                                           (:file "french" :depends-on  ("mnemonic"))
                                           (:file "german" :depends-on ("mnemonic"))
                                           (:file "italian" :depends-on ("mnemonic"))
                                           (:file "japanese" :depends-on ("mnemonic"))
                                           (:file "mnemonic")
                                           (:file "portuguese" :depends-on ("mnemonic"))
                                           (:file "russian" :depends-on ("mnemonic"))
                                           (:file "spanish" :depends-on ("mnemonic"))))
                             (:module "rpc"
                              :depends-on ("blockchain" "package" "serialization" "utils")
                              :components ((:file "rpc")
                                           (:file "server" :depends-on ("rpc"))
                                           (:file "wallet" :depends-on ("rpc"))))
                             (:module "serialization"
                              :depends-on ("crypto" "package" "utils")
                              :components ((:file "constants")
                                           (:file "deserialization" :depends-on ("constants"))
                                           (:file "serialization" :depends-on ("constants"))))
                             (:module "utils"
                              :depends-on ("package")
                              :components ((:file "base58" :depends-on ("utils"))
                                           (:file "utils")))
                             (:module "wallet"
                              :depends-on ("crypto" "package" "serialization" "utils")
                              :components ((:file "address")
                                           (:file "wallet")))))))

(defun compile-cncrypto-library (&optional static)
  (flet ((get-pathname (file)
           (namestring (asdf:system-relative-pathname "monero-tools" file)))
         (program-exists-p (program)
           (handler-case (uiop:run-program (list program) :ignore-error-status t)
             (t () (return-from program-exists-p nil)))
           t))
    (let* ((architecture (or #+(or arm ppc x86) "32"
                             #+(or arm64 ppc64 x86-64) "64"
                             (error "Unknown architecture.")))
           (library (get-pathname (concatenate 'string
                                               "lib/libcncrypto"
                                               architecture
                                               (or #+unix (if static ".a" ".so")
                                                   #+windows (if static ".lib" ".dll")
                                                   (error "Unknown platform."))))))
      (unless (uiop:file-exists-p library)
        (let* ((sources (loop for file in '("aesb.c"
                                            "blake256.c"
                                            "chacha8.c"
                                            "crypto-ops.c"
                                            "crypto-ops-data.c"
                                            "groestl.c"
                                            "hash-extra-blake.c"
                                            "hash-extra-groestl.c"
                                            "hash-extra-jh.c"
                                            "hash-extra-skein.c"
                                            "hash.c"
                                            "jh.c"
                                            "keccak.c"
                                            "oaes_lib.c"
                                            "random.c"
                                            "skein.c"
                                            "slow-hash.c"
                                            "tree-hash.c")
                              collect (get-pathname (concatenate 'string
                                                                 "lib/cncrypto/"
                                                                 file))))
               (compiler (let ((cc (uiop:getenv "CC")))
                           (cond
                             ((and cc (program-exists-p cc)) cc)
                             ((program-exists-p "gcc") "gcc")
                             ((program-exists-p "clang") "clang")
                             (t (error "Compiler not found."))))))
          (if static
              (let* ((archiver (cond
                                 ((program-exists-p "ar") "ar")
                                 (t (error "Archiver not found."))))
                     (objects (mapcar (lambda (source)
                                        (concatenate 'string
                                                     (subseq source 0 (- (length source) 1))
                                                     "o"))
                                      sources))
                     (compile (mapcar (lambda (c o)
                                        (list compiler
                                              "-c"
                                              "-O2"
                                              "-pipe"
                                              "-fPIC"
                                              "-maes"
                                              (concatenate 'string "-m" architecture)
                                              "-o" o
                                              c))
                                      sources
                                      objects))
                     (archive (append (list archiver "rcs" library) objects)))
                (dolist (cmd compile)
                  (uiop:run-program cmd :output t :error-output t))
                (uiop:run-program archive :output t :error-output t)
                (dolist (object objects)
                  (delete-file object)))
              (let ((compile (append (list compiler
                                           "-shared"
                                           "-O2"
                                           "-pipe"
                                           "-fPIC"
                                           "-maes"
                                           (concatenate 'string "-m" architecture)
                                           "-o" library)
                                     sources)))
                (uiop:run-program compile :output t :error-output t))))))))

(defmethod asdf:perform :before ((op asdf:load-op) (c asdf:cl-source-file))
  (compile-cncrypto-library))

(defsystem "monero-tools/tests"
  :name "monero-tools/tests"
  :description "Tests for monreo-tools"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("fiveam"
               "monero-tools")
  :in-order-to ((test-op (load-op "monero-tools/tests")))
  :perform (test-op (op s)
                    (let ((tests (uiop:find-symbol* 'monero-tools-tests :monero-tools/tests)))
                      (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:module "tests"
                :components ((:file "crypto" :depends-on ("tests"))
                             (:file "tests")
                             (:file "utils" :depends-on ("tests"))
                             (:file "wallet" :depends-on ("tests"))))))
