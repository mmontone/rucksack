(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import/export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The contents of a rucksack can be exported to a single file.  The file will
;; contain enough information to reconstruct the original rucksack objects.
;; Rucksack export files use a relatively simple s-expression format.
;;
;; There are two reasons for exporting a rucksack:
;; - backup
;;   The export file has a simple format, so it's a lot less sensitive
;;   to data corruption bugs.
;; - migration
;;   Export files can be imported by newer versions of Rucksack.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import/export API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric export-rucksack (rucksack pathname)
  (:documentation "Export all objects in a rucksack to a file.  The
resulting file can be imported by newer versions of Rucksack."))

(defgeneric import-rucksack (pathname directory-designator
                             &rest args
                             &key (if-exists :error)
                             &allow-other-keys)
  (:documentation "Creates a new rucksack in the directory specified by
DIRECTORY-DESIGNATOR, opens the new rucksack and imports all objects
that were exported to the file specified by PATHNAME."))


