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
         (content (cdr (assoc "content" params :test #'string=)))
         (tone (cdr (assoc "tone" params :test #'string=))))
    (when (null content)
      (error '<missing-param-error> :param-name "content"))
    
    (let ((pinyin (make-instance '<pinyin> :content content)))
      (when tone
        (setf (pinyin-tone pinyin) tone))
      pinyin)))

(defun find-letter-by-pinyin (params)
  (let* ((http-params (make-instance '<http-params> :params params))
         (mysql-connection (open-mysql-connection))
         (letter-repository (make-instance '<mysql-letter-repository>
                                           :connection mysql-connection))
         (use-case (make-instance '<use-case>
                                  :letter-repository letter-repository
                                  :params http-params)))
    (handler-case
        (let ((letters (run use-case)))
          (jonathan:to-json
           (mapcar #'(lambda (letter)
                       (list :content (letter-content letter)))
                   letters)))
      (<missing-param-error> (e)
        (format nil "缺少必备参数~A" (missing-param-error-param-name e))))))

(setf (ningle:route *app* "/letter") #'find-letter-by-pinyin)
