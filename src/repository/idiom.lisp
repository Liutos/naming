(in-package #:cl-user)

(defpackage #:naming.repository.idiom
  (:use #:cl)
  (:import-from #:naming.app.entity.idiom
                #:<idiom>
                #:add
                #:idiom-id
                #:idiom-letters)
  (:import-from #:naming.app.entity.letter
                #:letter-content
                #:letter-id)
  (:import-from #:naming.lib.sql-builder
                #:<sql-builder>
                #:set-pair
                #:to-sql
                #:where)
  (:import-from #:naming.repository.connection-interface
                #:execute-sql
                #:get-last-insert-id))

(in-package #:naming.repository.idiom)

(defclass <mysql-idiom-repository> ()
  ((connection
    :initarg :connection))
  (:documentation "基于MySQL的成语的存储。"))

(defun get-idiom-content (idiom)
  (check-type idiom <idiom>)
  (coerce (mapcar #'(lambda (letter)
                      (letter-content letter))
                  (idiom-letters idiom))
          'string))

(defmethod add ((repository <mysql-idiom-repository>) (idiom <idiom>))
  "将一个成语保存到MySQL中。"
  (with-slots (connection) repository
    (let ((letters (idiom-letters idiom)))
      ;; 先写入t_idiom表，再写入t_idiom_letter，最好有一个数据库事务。
      (let (builder
            (content (get-idiom-content idiom))
            sql)
        (setf builder (make-instance '<sql-builder>
                                     :table "t_idiom"
                                     :type :insert))
        (set-pair builder "content" content)
        (setf sql (to-sql builder))
        (execute-sql connection sql)
        (let ((idiom-id (get-last-insert-id connection)))
          (dolist (letter letters)
            (setf builder (make-instance '<sql-builder>
                                         :table "t_idiom_letter"
                                         :type :insert))
            (set-pair builder "idiom_id" idiom-id)
            (set-pair builder "letter_id" (letter-id letter))
            (setf sql (to-sql builder))
            (execute-sql connection sql))
          (setf (idiom-id idiom) idiom-id)
          idiom)))))
