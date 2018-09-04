;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools/tests
  (:use :cl :fiveam :monero-tools))

(in-package :monero-tools/tests)


(def-suite monero-tools-tests
  :description "Unit tests for monero-tools.")

(in-suite monero-tools-tests)

(defun data-file-path (filename)
  (asdf:system-relative-pathname "monero-tools/tests"
                                 (concatenate 'string "tests/data/" filename)))

(defun load-hex-data (filename)
  (with-open-file (in (data-file-path filename))
    (read-line in)))
