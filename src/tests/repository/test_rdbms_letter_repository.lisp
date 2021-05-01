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

(defmethod execute-sql ((connection dbi.driver:<dbi-connection>) (sql string))
  "通过cl-dbi这个库请求MySQL执行SQL语句。"
  (let ((query (dbi:prepare connection sql)))
    (dbi:execute query)))

(defmethod get-last-insert-id ((connection mock-connection))
  666)

(defmethod get-last-insert-id ((connection dbi.driver:<dbi-connection>))
  (let* ((alias "last_insert_id")
         (sql (format nil "SELECT LAST_INSERT_ID() AS `~A`" alias))
         (query (dbi:prepare connection sql)))
    (dbi:execute query)
    (let ((row (dbi:fetch query)))
      (getf row (intern alias :keyword)))))

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
