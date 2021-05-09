(in-package #:cl-user)

(defpackage #:naming.web.controller.find-letter-by-pinyin
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:<pinyin>)
  (:import-from #:naming.infra.db-connection
                #:open-mysql-connection)
  (:import-from #:naming.repository.letter
                #:<mysql-letter-repository>)
  (:import-from #:naming.app.use-case.find-letter-by-pinyin
                #:<use-case>
                #:get-pinyin
                #:run)
  (:import-from #:naming.web.app
                #:*app*))

(in-package #:naming.web.controller.find-letter-by-pinyin)

(defclass <http-params> ()
  ((params
    :documentation "来自ningle框架的输入参数"
    :initarg :params
    :reader http-params-params)))

(defmethod get-pinyin ((params <http-params>))
  "从HTTP请求中提取拼音和声调。"
  (let* ((params (http-params-params params))
         (content (cdr (assoc "content" params :test #'string=)))
         (tone (cdr (assoc "tone" params :test #'string=))))
    ;;--- TODO: 检查content和tone是否存在。
    (make-instance '<pinyin> :content content :tone tone)))

(defun find-letter-by-pinyin (params)
  (let* ((http-params (make-instance '<http-params> :params params))
         (mysql-connection (open-mysql-connection))
         (letter-repository (make-instance '<mysql-letter-repository>
                                           :connection mysql-connection))
         (use-case (make-instance '<use-case>
                                  :letter-repository letter-repository
                                  :params http-params)))
    (let ((letters (run use-case))))))

(setf (ningle:route *app* "/letter") #'find-letter-by-pinyin)
