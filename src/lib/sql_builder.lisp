(in-package #:cl-user)

(defpackage #:naming.lib.sql-builder
  (:use #:cl))

(in-package #:naming.lib.sql-builder)

(defclass <sql-builder> ()
  ((columns
    :documentation "批量插入时的列名。"
    :initarg :columns
    :initform nil)
   (conditions
    :initform ())
   (order-by
    :documentation "SELECT语句的排序方向。"
    :initform nil)
   (pairs
    :documentation "要插入表中的值及其所属的列。"
    :initform ())
   (table
    :initarg :table)
   (type
    :documentation "SQL语句的类型，支持:select"
    :initarg :type)
   (values
    :documentation "批量插入时的值列表。"
    :initarg :values
    :initform nil))
  (:documentation "SQL构造器。"))

(defclass <where-clause> ()
  ((column
    :initarg :column)
   (operator
    :initarg :operator)
   (value
    :initarg :value)))

(defgeneric order-by (builder column direction)
  (:documentation "指定SELECT语句的排序方向。")
  (:method ((builder <sql-builder>)
            (column string)
            (direction string))
    (setf (slot-value builder 'order-by)
          (cons column direction))))

(defgeneric set-columns (builder columns)
  (:documentation "设定插入的列。"))

(defgeneric set-pair (builder column value)
  (:documentation "往生成器中添加一对列和值。"))

(defgeneric set-values (builder values)
  (:documentation "添加一组批量插入的值。"))

(defgeneric to-sql (builder)
  (:documentation "生成SQL语句。"))

(defgeneric to-typed-sql (builder type)
  (:documentation "生成特定类型的SQL。"))

(defgeneric where (builder clause)
  (:documentation "往生成器中添加一个查询条件。"))

(defmethod set-columns ((builder <sql-builder>) (columns list))
  (setf (slot-value builder 'columns) columns)
  builder)

(defmethod set-pair ((builder <sql-builder>) (column string) (value t))
  (with-slots (columns pairs values) builder
    (unless (position column columns :test #'string=)
      (push column columns)
      (unless values
        (setf values (list nil)))
      (push value (first values))
      (push (cons column value) pairs))
    builder))

(defmethod set-values ((builder <sql-builder>) (values list))
  (push values (slot-value builder 'values))
  builder)

(defmethod to-sql ((builder <sql-builder>))
  (with-slots (type) builder
    (to-typed-sql builder type)))

(defun columns-values-to-sql (columns values s)
  "将COLUMNS和VALUES转换为INSERT中批量插入的子句，打印到字符流S中。"
  (check-type columns list)
  (check-type s stream)
  (check-type values list)
  (format s " (~{`~A`~^, ~})" columns)
  (format s " VALUES ")
  (dotimes (i (length values))
    (let ((vals (nth i values)))
      (format s "(~{~S~^, ~})" vals))
    (when (< i (1- (length values)))
      (format s ", "))))

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
        (cond ((and (eq operator :in) (numberp (first value)))
               (format s " `~A` IN (~{~D~^, ~})" column value))
              ((and (eq operator :in) (characterp (first value)))
               (format s " `~A` IN (~{'~C'~^, ~})" column value))
              ((and (eq operator :not-in) (characterp (first value)))
               (format s " `~A` NOT IN (~{'~C'~^, ~})" column value))
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
    (with-slots (columns table values) builder
      (format s "INSERT INTO `~A`" table)
      ;; (pairs-to-sql pairs s)
      (columns-values-to-sql columns values s))))

(defmethod to-typed-sql ((builder <sql-builder>) (type (eql :select)))
  "生成SELECT语句。"
  (with-output-to-string (s)
    (with-slots (columns conditions order-by table) builder
      (unless columns
        (setf columns '("*")))
      (format s "SELECT ~{`~A`~^, ~} FROM `~A`" columns table)
      (condition-to-sql conditions s)
      (when order-by
        (format s " ORDER BY `~A` ~A"
                (car order-by)
                (cdr order-by))))))

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

(defun make-insert-statement (table &rest pairs)
  "生成INSERT语句。"
  (check-type table string)
  (let ((builder (make-instance '<sql-builder>
                                :table table
                                :type :insert)))
    (alexandria:doplist (column value pairs)
      (set-pair builder column value))
    (to-sql builder)))

(defun make-select-statement (table &rest args)
  (check-type table string)
  (let ((builder (make-instance '<sql-builder>
                                :table table
                                :type :select)))
    (alexandria:doplist (cmd val args)
      (ecase cmd
        (:columns (set-columns builder val))
        (:order-by (apply #'order-by builder val))
        (:where (where builder val))))
    (to-sql builder)))
