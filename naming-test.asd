(in-package #:cl-user)

(defpackage #:system-naming-test
  (:use #:cl #:asdf))

(in-package #:system-naming-test)

(defsystem #:naming-test
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.1.0"
  :depends-on (#:naming)
  :components
  ((:module "src"
            :components
            ((:module "tests"
                      :components
                      ((:module "repository"
                                :components
                                ((:file "test_rdbms_letter_repository")))
                       (:module "use_case"
                                :components
                                ((:file "test_create_letter")
                                 (:file "test_create_word")))))))))
