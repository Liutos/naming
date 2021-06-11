(in-package #:cl-user)

(defpackage #:naming.repository.poetry
  (:use #:cl)
  (:import-from #:naming.app.entity.poetry
                #:<poetry>
                #:add
                #:poetry-author
                #:poetry-id
                #:poetry-pairs
                #:poetry-sentences-contents
                #:poetry-sentences-letter-ids
                #:poetry-title)
  (:import-from #:naming.repository.connection-interface
                #:execute-sql
                #:get-last-insert-id)
  (:import-from #:naming.lib.sql-builder
                #:<sql-builder>
                #:make-insert-statement
                #:set-columns
                #:set-values
                #:to-sql))

(in-package #:naming.repository.poetry)

(defclass <mysql-poetry-repository> ()
  ((connection
    :initarg :connection)))

(defmethod add ((repository <mysql-poetry-repository>) (poetry <poetry>))
  ;; 先插入到t_poetry表，再插入t_poetry_sentence表，最后插入t_letter_poetry_sentence表。
  ;;--- TODO: 开启一个数据库事务
  (with-slots (connection) repository
    (let ((sql (make-insert-statement "t_poetry"
                                      "author" (poetry-author poetry)
                                      "title" (poetry-title poetry))))
      (execute-sql connection sql)
      (let ((poetry-id (get-last-insert-id connection)))
        (setf (poetry-id poetry) poetry-id)
        (dolist (sentence (poetry-pairs poetry))
          (setf sql (make-insert-statement "t_poetry_sentence"
                                           "content" (poetry-sentences-contents sentence)
                                           "poetry_id" poetry-id))
          (execute-sql connection sql)
          ;; 用批量插入来提速
          (let ((builder (make-instance '<sql-builder>
                                        :table "t_letter_poetry_sentence"
                                        :type :insert))
                (poetry-sentence-id (get-last-insert-id connection)))
            (set-columns builder '("letter_id" "poetry_sentence_id"))
            (let ((letter-ids (poetry-sentences-letter-ids sentence)))
              (when letter-ids
                (dolist (letter-id letter-ids)
                  (set-values builder (list letter-id poetry-sentence-id)))
                (let ((sql (to-sql builder)))
                  (execute-sql connection sql))))))
        poetry))))
