(in-package #:cl-user)

(defpackage #:system-naming
  (:use #:cl #:asdf))

(in-package #:system-naming)

(defsystem #:naming
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.1.0"
  :depends-on (#:cl-dbi
               #:cl-ppcre
               #:clack
               #:jonathan
               #:ningle)
  :components
  ((:module "src"
            :components
            ((:module "app"
                      :components
                      ((:module "entity"
                                :components
                                ((:file "letter")))
                       (:module "use_case"
                                :components
                                ((:file "add_letter")
                                 (:file "find_letter_by_pinyin"))
                                :depends-on ("entity"))))
             (:module "infra"
                      :components
                      ((:file "db_connection"))
                      :depends-on ("repository"))
             (:module "lib"
                      :components
                      ((:file "sql_builder")))
             (:module "repository"
                      :components
                      ((:file "connection_interface")
                       (:file "letter"
                              :depends-on ("connection_interface")))
                      :depends-on ("lib"))
             (:module "web"
                      :components
                      ((:file "app")
                       (:module "controller"
                                :components
                                ((:file "add_letter")
                                 (:file "find_letter_by_pinyin"))
                                :depends-on ("app"))))))))
