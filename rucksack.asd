;;; $Id: rucksack.asd,v 1.17 2008-02-03 12:32:16 alemmens Exp $

(in-package :cl-user)

(asdf:defsystem :rucksack
  :version "0.1.15"
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

