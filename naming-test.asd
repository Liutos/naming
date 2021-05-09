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
                                ((:file "test_mysql_letter_repository")))))))))
