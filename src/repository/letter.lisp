(in-package #:cl-user)

(defpackage #:naming.repository.letter
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:<letter>
                #:<pinyin>
                #:add
                #:find-by-content
                #:find-by-pinyin
                #:letter-content
                #:letter-id
                #:letter-pinyins
                #:letter-radicals
                #:letter-stroke
                #:pinyin-content
                #:pinyin-content-bound-p
                #:pinyin-tone
                #:pinyin-tone-bound-p
                #:query)
  (:import-from #:naming.lib.sql-builder
                #:<sql-builder>
                #:to-sql
                #:set-pair
                #:where)
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
  (let ((builder (make-instance '<sql-builder> :table "t_letter_pinyin" :type :select)))
    (when (pinyin-content pinyin)
      (where builder (list := "content" (pinyin-content pinyin))))
    (when (pinyin-tone pinyin)
      (where builder (list := "tone" (pinyin-tone pinyin))))
    (let ((sql (to-sql builder)))
      (execute-sql connection sql)
      (fetch-all connection))))

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
  "将一个字LETTER保存到存储仓库REPOSITORY中。

如果LETTER的槽ID不为空，则更新REPOSITORY中已经存在的数据。"
  (if (letter-id letter)
      ;; 更新已有的字。先更新t_letter_pinyin表，再更新t_letter表。
      (with-slots (connection) repository
        (let (builder
              (letter-id (letter-id letter))
              sql)
          (setf builder (make-instance '<sql-builder>
                                       :table "t_letter_pinyin"
                                       :type :delete))
          (where builder (list := "letter_id" letter-id))
          (setf sql (to-sql builder))
          (execute-sql connection sql)

          (dolist (pinyin (letter-pinyins letter))
            (check-type pinyin <pinyin>)
            (setf builder (make-instance '<sql-builder>
                                         :table "t_letter_pinyin"
                                         :type :insert))
            (set-pair builder "content" (pinyin-content pinyin))
            (set-pair builder "letter_id" letter-id)
            (set-pair builder "tone" (pinyin-tone pinyin))
            (setf sql (to-sql builder))
            (execute-sql connection sql))

          (setf builder (make-instance '<sql-builder>
                                       :table "t_letter"
                                       :type :update))
          (set-pair builder "content" (letter-content letter))
          (set-pair builder "radicals" (letter-radicals letter))
          (set-pair builder "stroke" (letter-stroke letter))
          (where builder (list := "id" letter-id))
          (setf sql (to-sql builder))
          (execute-sql connection sql)))
      ;; 写入新的字
      (let* ((connection (mysql-letter-repository-connection repository))
             (sql (format nil "INSERT INTO `t_letter` SET `content` = '~A', `radicals` = '~C', `stroke` = ~D"
                          (letter-content letter)
                          (letter-radicals letter)
                          (letter-stroke letter))))
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
          letter))))

(defmethod find-by-content ((repository <mysql-letter-repository>) (content character))
  "找出写做CONTENT的字对象。"
  (let ((builder (make-instance '<sql-builder>
                                :table "t_letter"
                                :type :select))
        sql)
    (where builder (list := "content" content))
    (setf sql (to-sql builder))
    (format t "sql is ~A~%" sql)
    (let ((connection (mysql-letter-repository-connection repository)))
      (execute-sql connection sql)
      (let ((plist (fetch-one connection)))
        (unless plist
          (return-from find-by-content nil))
        (make-instance '<letter>
                       :content (char (getf plist :|content|) 0)
                       :id (getf plist :|id|)
                       ;;--- TODO: 补充获取拼音列表的逻辑
                       )))))

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
              letters)))
    (nreverse letters)))

(defmethod query ((repository <mysql-letter-repository>) &rest args
                  &key pinyin radicals)
  (declare (ignorable args))
  (let ((builder (make-instance '<sql-builder>
                                :table "t_letter"
                                :type :select))
        (connection (mysql-letter-repository-connection repository)))
    ;;--- TODO: 将这里优化为一段子查询
    ;; 由于拼音存储在表t_letter_pinyin表中的，因此需要额外查询。
    (when pinyin
      (let ((letter-ids '()))
        (dolist (pinyin pinyin)
          (let* ((letter-pinyins (find-letter-pinyins connection pinyin))
                 (partial-ids (mapcar #'(lambda (letter-pinyin)
                                          (getf letter-pinyin :|letter_id|))
                                      letter-pinyins)))
            (setf letter-ids (append letter-ids partial-ids))))
        (where builder (list :in "id" letter-ids))))
    (when radicals
      (where builder (list :in "radicals" radicals)))
    (let ((sql (to-sql builder)))
      (format t "待执行的SQL语句为~S~%" sql)
      (execute-sql connection sql)
      (let ((rows (fetch-all connection)))
        (mapcar #'(lambda (row)
                    (make-instance '<letter>
                                   :content (getf row :|content|)
                                   :id (getf row :|id|)
                                   :pinyins (mapcar #'(lambda (pinyin-plist)
                                                        (make-instance '<pinyin>
                                                                       :content (getf pinyin-plist :|content|)
                                                                       :tone (getf pinyin-plist :|tone|)))
                                                    (find-pinyins-by-letter connection (getf row :|id|)))
                                   :radicals (getf row :|radicals|)))
                rows)))))
