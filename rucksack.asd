;;; $Id: rucksack.asd,v 1.3 2007-01-16 08:31:49 charmon Exp $

(in-package :cl-user)

(asdf:defsystem :rucksack
  :version "0.1.1"
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
    
