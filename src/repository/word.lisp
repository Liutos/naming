(in-package #:cl-user)

(defpackage #:naming.repository.word
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:letter-id)
  (:import-from #:naming.app.entity.word
                #:add
                #:word
                #:word-id
                #:word-letters)
  (:import-from #:naming.repository.connection-interface
                #:execute-sql
                #:get-last-insert-id))

(in-package #:naming.repository.word)

(defclass rdbms-repository ()
  ((connection
    :initarg :connection
    :reader rdbms-repository-connection))
  (:documentation "基于关系型数据库的词语的存储。"))

(defmethod add ((repository rdbms-repository) (word word))
  "将一个词语保存到数据库中。"
  (let ((connection (rdbms-repository-connection repository))
        (letters (word-letters word)))
    ;;--- TODO: 此处应当开启一个数据库事务。
    ;;--- TODO: 需要一个类似于pypika的构造SQL的工具。
    (let ((sql (format nil "INSERT INTO `t_word`")))
      (execute-sql connection sql)
      (let ((word-id (get-last-insert-id connection)))
        (dolist (letter letters)
          (let ((sql (format nil "INSERT INTO `t_letter_word` SET `letter_id` = ~D, `word_id` = ~D" (letter-id letter) word-id)))
            (execute-sql connection sql)))
        (setf (word-id word) word-id)
        word))))
