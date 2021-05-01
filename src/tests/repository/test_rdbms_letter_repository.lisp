(in-package #:cl-user)

(defpackage #:naming.tests.repository.test-rdbms-letter-repository
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:letter
                #:letter-id)
  (:import-from #:naming.repository.letter
                #:add
                #:execute-sql
                #:get-last-insert-id
                #:rdbms-repository))

(in-package #:naming.tests.repository.test-rdbms-letter-repository)

(defclass mock-connection ()
  ())

(defmethod execute-sql ((connection mock-connection) (sql string))
  )

(defmethod get-last-insert-id ((connection mock-connection))
  666)

(defun test-add ()
  "测试往数据库中添加一个字的功能。"
  (let* ((connection (make-instance 'mock-connection))
         (letter (make-instance 'letter :content #\中))
         (repository (make-instance 'rdbms-repository
                                    :connection connection)))
    (add repository letter)
    (assert (= (letter-id letter) 666))))
