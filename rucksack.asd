;;; $Id: rucksack.asd,v 1.8 2007-01-22 10:23:14 alemmens Exp $

(in-package :cl-user)

(asdf:defsystem :rucksack
  :version "0.1.6"
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
    
