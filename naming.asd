(in-package #:cl-user)

(defpackage #:system-naming
  (:use #:cl #:asdf))

(in-package #:system-naming)

(defsystem #:naming
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.1.0"
  :depends-on ()
  :components
  ((:module "src"
            :components
            ((:module "app"
                      :components
                      ((:module "entity"
                                :components
                                ((:file "letter")
                                 (:file "word")))
                       (:module "use_case"
                                :components
                                ((:file "create_letter"))
                                :depends-on ("entity"))))))))
