(in-package #:cl-user)

(defpackage #:naming.lib.sql-builder
  (:use #:cl))

(in-package #:naming.lib.sql-builder)

(defclass <sql-builder> ()
  ((conditions
    :initform ())
   (pairs
    :documentation "要插入表中的值及其所属的列。"
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

(defgeneric set-pair (builder column value)
  (:documentation "往生成器中添加一对列和值。"))

(defgeneric to-sql (builder)
  (:documentation "生成SQL语句。"))

(defgeneric to-typed-sql (builder type)
  (:documentation "生成特定类型的SQL。"))

(defgeneric where (builder clause)
  (:documentation "往生成器中添加一个查询条件。"))

(defmethod set-pair ((builder <sql-builder>) (column string) (value t))
  (with-slots (pairs) builder
    (push (cons column value) pairs)
    builder))

(defmethod to-sql ((builder <sql-builder>))
  (with-slots (type) builder
    (to-typed-sql builder type)))

(defun condition-to-sql (conditions s)
  "将CONDITIONS转换为SQL的WHERE子句。"
  (check-type conditions list)
  (check-type s stream)
  (when conditions
    (format s " WHERE")
    (dotimes (i (length conditions))
      (with-slots (column operator value)
          (nth i conditions)
        ;;--- TODO: 这里要怎么写才比较优雅呢？
        (cond ((eq operator :in)
               (format s " `~A` IN (~{~S~^, ~})" column value))
              (t
               (let ((fmt (typecase value
                            (character " `~A` ~A '~C'")
                            (t " `~A` ~A ~S"))))
                 (format s fmt column operator value)))))
      (when (< i (1- (length conditions)))
        (format s " AND")))))

(defun pairs-to-sql (pairs s)
  "将PAIRS转换为SQL的SET子句。"
  (when pairs
    (format s " SET")
    (dotimes (i (length pairs))
      (let ((pair (nth i pairs)))
        (destructuring-bind (column . value) pair
          (let ((fmt (typecase value
                       (character " `~A` = '~C'")
                       (null " `~A` = NULL")
                       (t " `~A` = ~S"))))
            (format s fmt column value))))
      (when (< i (1- (length pairs)))
        (format s ", ")))))

(defmethod to-typed-sql ((builder <sql-builder>) (type (eql :delete)))
  "生成DELETE语句。"
  (with-output-to-string (s)
    (format s "DELETE FROM `~A`" (slot-value builder 'table))
    (condition-to-sql (slot-value builder 'conditions) s)))

(defmethod to-typed-sql ((builder <sql-builder>) (type (eql :insert)))
  "生成INSERT语句。"
  (with-output-to-string (s)
    (with-slots (pairs table) builder
      (format s "INSERT INTO `~A`" table)
      (pairs-to-sql pairs s))))

(defmethod to-typed-sql ((builder <sql-builder>) (type (eql :select)))
  "生成SELECT语句。"
  (with-output-to-string (s)
    (with-slots (conditions table) builder
      (format s "SELECT * FROM `~A`" table)
      (condition-to-sql conditions s))))

(defmethod to-typed-sql ((builder <sql-builder>) (type (eql :update)))
  "生成UPDATE语句。"
  (with-output-to-string (s)
    (with-slots (conditions pairs table) builder
      (format s "UPDATE `~A`" table)
      (pairs-to-sql pairs s)
      (condition-to-sql conditions s))))

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
