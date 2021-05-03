(in-package #:cl-user)

(defpackage #:naming.infra.db-connection
  (:use #:cl)
  (:import-from #:naming.repository.connection-interface
                #:execute-sql
                #:get-last-insert-id))

(in-package #:naming.infra.db-connection)

(defmethod execute-sql ((connection dbi.driver:<dbi-connection>) (sql string))
  "通过cl-dbi这个库请求MySQL执行SQL语句。"
  (let ((query (dbi:prepare connection sql)))
    (dbi:execute query)))

(defmethod get-last-insert-id ((connection dbi.driver:<dbi-connection>))
  (let* ((alias "last_insert_id")
         (sql (format nil "SELECT LAST_INSERT_ID() AS `~A`" alias))
         (query (dbi:prepare connection sql)))
    (dbi:execute query)
    (let ((row (dbi:fetch query)))
      (getf row (intern alias :keyword)))))
