(in-package #:cl-user)

(defpackage #:naming.tests.repository.test-rdbms-word-repository
  (:use #:cl)
  (:import-from #:naming.repository.word
                #:add))

(in-package #:naming.tests.repository.test-rdbms-word-repository)

(defun test-add ()
  "测试往数据库中添加一个词的功能。"
  ;;--- TODO: 创建测试数据库连接的代码要与test_rdbms_letter_repository.lisp中的统一起来。
  (let* ((connection (dbi:connect :mysql
                                  :database-name "naming_unittest"
                                  :password "2617267"
                                  :username "root")))
    (unwind-protect
         ()
      (dbi:disconnect connection))))
