(in-package #:cl-user)

(defpackage #:naming.web.controller.find-idiom-by-letter
  (:use #:cl)
  (:import-from #:naming.app.entity.idiom
                #:idiom-id
                #:idiom-content)
  (:import-from #:naming.app.use-case.find-idiom-by-letter
                #:<use-case>
                #:get-letter-content
                #:run)
  (:import-from #:naming.infra.db-connection
                #:open-mysql-connection)
  (:import-from #:naming.repository.idiom
                #:<mysql-idiom-repository>)
  (:import-from #:naming.repository.letter
                #:<mysql-letter-repository>)
  (:import-from #:naming.web.app
                #:*app*))

(in-package #:naming.web.controller.find-idiom-by-letter)

(defmethod get-letter-content ((params list))
  "从query string中提取出待查询的字。"
  (char (cdr (assoc "letter" params :test #'string=)) 0))

(defun find-idiom-by-letter (params)
  (let* ((mysql-connection (open-mysql-connection))
         (idiom-repository (make-instance '<mysql-idiom-repository>
                                          :connection mysql-connection))
         ;;--- TODO: 此处需要一个repository的工厂来减少代码量。
         (letter-repository (make-instance '<mysql-letter-repository>
                                           :connection mysql-connection))
         (use-case (make-instance '<use-case>
                                  :idiom-repository idiom-repository
                                  :letter-repository letter-repository
                                  :params params)))
    (let ((idioms (run use-case)))
      (jonathan:to-json
       (mapcar #'(lambda (idiom)
                   (list :content (idiom-content idiom)
                         :id (idiom-id idiom)))
               idioms)))))

(setf (ningle:route *app* "/idiom") #'find-idiom-by-letter)
