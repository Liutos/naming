(in-package #:cl-user)

(defpackage #:naming.lib.sql-builder
  (:use #:cl))

(in-package #:naming.lib.sql-builder)

(defclass <sql-builder> ()
  ((conditions
    :initform ())
   (table
    :initarg :table)
   (type
    :documentation "SQL语句的类型，支持:select"
    :initarg :type))
  (:documentation "SQL构造器。"))

(defclass <where-clause> ()
  ((column
    :initarg :column)
   (operator
    :initarg :operator)
   (value
    :initarg :value)))

(defgeneric to-sql (builder)
  (:documentation "生成SQL语句。"))

(defgeneric to-typed-sql (builder type)
  (:documentation "生成特定类型的SQL。"))

(defgeneric where (builder clause)
  (:documentation "往生成器中添加一个查询条件。"))

(defmethod to-sql ((builder <sql-builder>))
  (with-slots (type) builder
    (to-typed-sql builder type)))

(defmethod to-typed-sql ((builder <sql-builder>) (type (eql :select)))
  "生成SELECT语句。"
  (with-output-to-string (s)
    (with-slots (conditions table) builder
      (format s "SELECT * FROM `~A`" table)
      (when conditions
        (format s " WHERE")
        (dotimes (i (length conditions))
          (with-slots (column operator value)
              (nth i conditions)
            (format s " `~A` ~A ~S" column operator value))
          (when (< i (1- (length conditions)))
            (format s " AND")))))))

(defmethod where ((builder <sql-builder>) (clause list))
  "如果是列表类型的子句，那么其结构为(操作符 列 值)"
  (with-slots (conditions) builder
    (destructuring-bind (operator column value)
        clause
      (push (make-instance '<where-clause>
                           :column column
                           :operator operator
                           :value value)
            conditions)))
  builder)
