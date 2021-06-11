(in-package #:cl-user)

(defpackage #:naming.repository.poetry
  (:use #:cl)
  (:import-from #:naming.app.entity.poetry
                #:<poetry>
                #:<poetry-sentences>
                #:add
                #:find-sentences-if-contain
                #:poetry-author
                #:poetry-id
                #:poetry-pairs
                #:poetry-sentences-contents
                #:poetry-sentences-letter-ids
                #:poetry-title)
  (:import-from #:naming.repository.connection-interface
                #:execute-sql
                #:fetch-all
                #:get-last-insert-id)
  (:import-from #:naming.lib.sql-builder
                #:<sql-builder>
                #:make-insert-statement
                #:make-select-statement
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

(defmethod find-sentences-if-contain ((repository <mysql-poetry-repository>) (letter-ids list))
  (with-slots (connection) repository
    (let ((sql (make-select-statement "t_letter_poetry_sentence"
                                      :where (list :in "letter_id" letter-ids))))
      (execute-sql connection sql)
      (let* ((rows (fetch-all connection))
             (poetry-sentence-ids (mapcar #'(lambda (row) (getf row :|poetry_sentence_id|)) rows))
             (poetry-sentences-table (make-hash-table)))
        (let ((sql (make-select-statement "t_poetry_sentence"
                                          :where (list :in "id" poetry-sentence-ids))))
          (execute-sql connection sql)
          ;; 将t_poetry_sentence表的结果转换为<POETRY-SENTENCES>类的对象，并填充letter-ids字段。
          (let ((rows (fetch-all connection)))
            (dolist (row rows)
              (let* ((id (getf row :|id|))
                     (poetry-sentences (make-instance '<poetry-sentences>
                                                      :contents (getf row :|content|)
                                                      :id id
                                                      :letter-ids nil)))
                (setf (gethash id poetry-sentences-table) poetry-sentences))))
          ;; 再遍历rows填充letter-ids
          (dolist (row rows)
            (let* ((letter-id (getf row :|letter_id|))
                   (poetry-sentence-id (getf row :|poetry_sentence_id|))
                   (poetry-sentences (gethash poetry-sentence-id poetry-sentences-table)))
              (when poetry-sentences
                (push letter-id (poetry-sentences-letter-ids poetry-sentences))))))
        ;; 现在可以返回了
        (alexandria:hash-table-values poetry-sentences-table)))))
