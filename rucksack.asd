;;; $Id: rucksack.asd,v 1.10 2007-03-13 13:13:00 alemmens Exp $

(in-package :cl-user)

(asdf:defsystem :rucksack
  :version "0.1.8"
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
    
