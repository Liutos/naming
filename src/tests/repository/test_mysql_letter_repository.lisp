(in-package #:cl-user)

(defpackage #:naming.tests.repository.test-mysql-letter-repository
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:<letter>
                #:<pinyin>)
  (:import-from #:naming.infra.db-connection
                #:close-mysql-connection
                #:execute-sql
                #:open-mysql-connection)
  (:import-from #:naming.repository.letter
                #:<mysql-letter-repository>
                #:add
                #:find-letter-pinyins))

(in-package #:naming.tests.repository.test-mysql-letter-repository)

(defparameter *letter*
  (make-instance '<letter>
                 :content "血"
                 :pinyins (list (make-instance '<pinyin> :content "xie" :tone 3)
                                (make-instance '<pinyin> :content "xue" :tone 4))))

(defun test-find-letter-pinyins ()
  "测试搜索t_letter_pinyin表的逻辑。"
  (let* ((conn (open-mysql-connection))
         (repository (make-instance '<mysql-letter-repository>
                                    :connection conn)))
    (unwind-protect
         (progn
           (add repository *letter*)
           (let* ((pinyin (make-instance '<pinyin> :content "xie" :tone 3))
                  (letter-pinyins (find-letter-pinyins conn pinyin)))
             (assert (= (length letter-pinyins) 1))))
      (execute-sql conn "DELETE FROM `t_letter_pinyin`")
      (execute-sql conn "DELETE FROM `t_letter`")
      (close-mysql-connection conn))))
