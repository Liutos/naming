(in-package #:cl-user)

(defpackage #:naming.web.controller.find-letter-by-pinyin
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:<pinyin>
                #:letter-content
                #:pinyin-tone)
  (:import-from #:naming.infra.db-connection
                #:open-mysql-connection)
  (:import-from #:naming.repository.letter
                #:<mysql-letter-repository>)
  (:import-from #:naming.app.use-case.find-letter-by-pinyin
                #:<use-case>
                #:get-pinyin
                #:get-radicals
                #:run)
  (:import-from #:naming.web.app
                #:*app*))

(in-package #:naming.web.controller.find-letter-by-pinyin)

(defclass <http-params> ()
  ((params
    :documentation "来自ningle框架的输入参数"
    :initarg :params
    :reader http-params-params)))

(define-condition <missing-param-error> ()
  ((param-name
    :initarg :param-name
    :reader missing-param-error-param-name))
  (:documentation "缺少参数的错误"))

(defmethod get-pinyin ((params <http-params>))
  "从HTTP请求中提取拼音和声调。"
  (let* ((params (http-params-params params))
         (pinyins (cdr (assoc "pinyins" params :test #'string=))))
    (mapcar #'(lambda (pinyin)
                (let ((content (cdr (assoc "content" pinyin :test #'string=)))
                      (tone (cdr (assoc "tone" pinyin :test #'string=))))
                  (make-instance '<pinyin>
                                 :content content
                                 :tone tone)))
            pinyins)))

(defmethod get-radicals ((params <http-params>))
  "从HTTP请求中提取偏旁部首。"
  (let* ((params (http-params-params params))
         (radicals (cdr (assoc "radicals" params :test #'string=))))
    (mapcar #'(lambda (radical)
                (char radical 0))
            radicals)))

(defun find-letter (params)
  (let* ((http-params (make-instance '<http-params> :params params))
         (mysql-connection (open-mysql-connection))
         (letter-repository (make-instance '<mysql-letter-repository>
                                           :connection mysql-connection))
         (use-case (make-instance '<use-case>
                                  :letter-repository letter-repository
                                  :params http-params)))
    (handler-case
        (let ((letters (run use-case)))
          (list
           200
           (list :content-type "application/json")
           (list (jonathan:to-json
                  (mapcar #'(lambda (letter)
                              (list :content (letter-content letter)))
                          letters)))))
      (<missing-param-error> (e)
        (format nil "缺少必备参数~A" (missing-param-error-param-name e))))))

(setf (ningle:route *app* "/letter" :method :post) #'find-letter)
