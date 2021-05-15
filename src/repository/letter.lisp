(in-package #:cl-user)

(defpackage #:naming.repository.letter
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:<letter>
                #:<pinyin>
                #:add
                #:find-by-pinyin
                #:letter-content
                #:letter-id
                #:letter-pinyins
                #:pinyin-content
                #:pinyin-tone)
  (:import-from #:naming.repository.connection-interface
                #:execute-sql
                #:fetch-all
                #:fetch-one
                #:get-last-insert-id))

(in-package #:naming.repository.letter)

(defclass <mysql-letter-repository> ()
  ((connection
    :initarg :connection
    :reader mysql-letter-repository-connection))
  (:documentation "基于MySQL的汉字仓库"))

(defun find-letter-pinyins (connection pinyin)
  (check-type pinyin <pinyin>)
  (let ((sql (format nil "SELECT * FROM `t_letter_pinyin` WHERE `content` = '~A' AND `tone` = ~D"
                     (pinyin-content pinyin)
                     (pinyin-tone pinyin))))
    (execute-sql connection sql)
    (fetch-all connection)))

(defun find-pinyins-by-letter (connection letter-id)
  (check-type letter-id integer)
  (let ((sql (format nil "SELECT * FROM `t_letter_pinyin` WHERE `letter_id` = ~D" letter-id)))
    (execute-sql connection sql)
    (fetch-all connection)))

(defun get-letter (connection id)
  (check-type id integer)
  (let ((sql (format nil "SELECT * FROM `t_letter` WHERE `id` = ~D" id)))
    (execute-sql connection sql)
    (fetch-one connection)))

(defmethod add ((repository <mysql-letter-repository>) (letter <letter>))
  (let* ((connection (mysql-letter-repository-connection repository))
         (sql (format nil "INSERT INTO `t_letter` SET `content` = '~A'"
                      (letter-content letter))))
    (execute-sql connection sql)
    (let ((id (get-last-insert-id connection)))
      ;; 写完t_letter表再写t_letter_pinyin表
      (dolist (pinyin (letter-pinyins letter))
        (execute-sql connection
                     (format nil "INSERT INTO `t_letter_pinyin` SET `content` = '~A', `letter_id` = ~D, `tone` = ~D"
                             (pinyin-content pinyin)
                             id
                             (pinyin-tone pinyin))))
      (setf (letter-id letter) id)
      letter)))

(defmethod find-by-pinyin ((repository <mysql-letter-repository>) (pinyin <pinyin>))
  (let* ((connection (mysql-letter-repository-connection repository))
         (letter-pinyins (find-letter-pinyins connection pinyin))
         (letters '()))
    (dolist (letter-pinyin letter-pinyins)
      (let* ((letter-id (getf letter-pinyin :|letter_id|))
             (letter-plist (get-letter connection letter-id))
             (pinyins (find-pinyins-by-letter connection letter-id))
             (pinyins (mapcar #'(lambda (pinyin-plist)
                                  (make-instance '<pinyin>
                                                 :content (getf pinyin-plist :|content|)
                                                 :tone (getf pinyin-plist :|tone|)))
                              pinyins)))
        (push (make-instance '<letter>
                             :content (getf letter-plist :|content|)
                             :pinyins pinyins)
              letters)))))
