(in-package #:cl-user)

(defpackage #:naming.repository.letter
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:add
                #:letter
                #:letter-content
                #:letter-id))

(in-package #:naming.repository.letter)

(defgeneric get-connection (pool)
  (:documentation "从池中获取一个数据库连接。"))

(defgeneric release-connection (pool connection)
  (:documentation "将连接归还给池。"))

(defgeneric execute-sql (connection sql)
  (:documentation "执行SQL。"))

(defgeneric fetch-all (connection)
  (:documentation "获取所有的结果集。"))

(defgeneric fetch-row (connection)
  (:documentation "获取一行查询结果。"))

(defgeneric get-last-insert-id (connection)
  (:documentation "获取最新的自增ID。"))

(defclass rdbms-repository ()
  ((connection
    :initarg :connection
    :reader rdbms-repository-connection))
  (:documentation "基于关系型数据库的字的存储仓库。"))

(defmethod add ((repository rdbms-repository) (letter letter))
  "将一个字保存到关系型数据库中。"
  (let ((connection (rdbms-repository-connection repository))
        (content (letter-content letter)))
    (let ((sql (format nil "INSERT INTO `t_letter` SET `content` = '~A'" content)))
      (execute-sql connection sql)
      (let ((id (get-last-insert-id connection)))
        (setf (letter-id letter) id)
        letter))))
