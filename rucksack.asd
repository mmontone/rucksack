;;; $Id: rucksack.asd,v 1.13 2008-01-22 15:59:24 alemmens Exp $

(in-package :cl-user)

(asdf:defsystem :rucksack
  :version "0.1.11"
  :serial t
  :components ((:file "queue")
               (:file "package")
               (:file "errors")
               (:file "mop")
               (:file "serialize" )
               (:file "heap")
               (:file "object-table")
               (:file "schema-table")
               (:file "garbage-collector")
               (:file "cache")
               (:file "objects")
               (:file "p-btrees")
               (:file "index")
               (:file "rucksack")
               (:file "transactions")
               (:file "test")))
