;;; $Id: rucksack.asd,v 1.5 2007-01-16 08:47:36 charmon Exp $

(in-package :cl-user)

(asdf:defsystem :rucksack
  :version "0.1.3"
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
    
