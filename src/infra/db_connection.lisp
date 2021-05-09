(in-package #:cl-user)

(defpackage #:naming.infra.db-connection
  (:use #:cl)
  (:import-from #:dbi.driver
                #:<dbi-connection>)
  (:import-from #:naming.repository.connection-interface
                #:execute-sql
                #:fetch-all
                #:fetch-one
                #:get-last-insert-id))

(in-package #:naming.infra.db-connection)

(defclass <mysql-connection> ()
  ((dbi-connection
    :documentation "来自dbi的数据库连接"
    :initarg :dbi-connection
    :reader mysql-connection-dbi-connection)
   (dbi-query
    :accessor mysql-connection-dbi-query)))

(defmethod execute-sql ((connection <mysql-connection>) (sql string))
  "通过cl-dbi这个库请求MySQL执行SQL语句。"
  (let* ((dbi-connection (mysql-connection-dbi-connection connection))
         (query (dbi:prepare dbi-connection sql)))
    (dbi:execute query)
    (setf (mysql-connection-dbi-query connection) query)))

(defmethod fetch-all ((connection <mysql-connection>))
  (let ((dbi-query (mysql-connection-dbi-query connection)))
    (dbi:fetch-all dbi-query)))

(defmethod fetch-one ((connection <mysql-connection>))
  (let ((dbi-query (mysql-connection-dbi-query connection)))
    (dbi:fetch dbi-query)))

(defmethod get-last-insert-id ((connection <mysql-connection>))
  (let* ((alias "last_insert_id")
         (sql (format nil "SELECT LAST_INSERT_ID() AS `~A`" alias)))
    (execute-sql connection sql)
    (let ((row (fetch-one connection)))
      (getf row (intern alias :keyword)))))

(defun close-mysql-connection (connection)
  (dbi:disconnect (mysql-connection-dbi-connection connection)))

(defun open-mysql-connection ()
  (let ((dbi-connection
         (dbi:connect :mysql
                      :database-name "naming_unittest"
                      :password "2617267"
                      :username "root")))
    (make-instance '<mysql-connection>
                   :dbi-connection dbi-connection)))
