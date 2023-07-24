;;;; cl-orgel-gui.asd
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(asdf:defsystem #:cl-orgel-gui
  :description "Gui für Papierrohrorgel."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :depends-on (#:clog #:orm-utils #:uuid)
  :serial t
  :components ((:file "package")
               (:file "widget-defs")
               (:file "cl-orgel-gui")
               (:file "init")))
