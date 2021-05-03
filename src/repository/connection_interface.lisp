(in-package #:cl-user)

(defpackage #:naming.repository.connection-interface
  (:use #:cl))

(in-package #:naming.repository.connection-interface)

(defgeneric execute-sql (connection sql)
  (:documentation "执行SQL。"))

(defgeneric fetch-all (connection)
  (:documentation "获取所有的结果集。"))

(defgeneric fetch-row (connection)
  (:documentation "获取一行查询结果。"))

(defgeneric get-last-insert-id (connection)
  (:documentation "获取最新的自增ID。"))
