;;; $Id: rucksack.asd,v 1.19 2008-02-19 22:44:06 alemmens Exp $

(in-package :cl-user)

(asdf:defsystem :rucksack
  :version "0.1.17"
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
               (:file "import-export")))

