(in-package #:cl-user)

(defpackage #:naming.tests.repository.test-rdbms-letter-repository
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:letter
                #:letter-id)
  (:import-from #:naming.repository.connection-interface
                #:execute-sql
                #:get-last-insert-id)
  (:import-from #:naming.repository.letter
                #:add
                #:rdbms-repository))

(in-package #:naming.tests.repository.test-rdbms-letter-repository)

(defun test-add ()
  "测试往数据库中添加一个字的功能。"
  (let* ((connection (dbi:connect :mysql
                                  :database-name "naming_unittest"
                                  :password "2617267"
                                  :username "root")))
    (unwind-protect
         (let ((letter (make-instance 'letter :content #\中))
               (repository (make-instance 'rdbms-repository
                                          :connection connection)))
           (add repository letter)
           (assert (integerp (letter-id letter))))
      (dbi:disconnect connection))))
