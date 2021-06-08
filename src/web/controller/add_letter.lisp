(in-package #:cl-user)

(defpackage #:naming.web.controller.add-letter
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:<pinyin>
                #:letter-id)
  (:import-from #:naming.app.use-case.add-letter
                #:<use-case>
                #:get-content
                #:get-pinyins
                #:run)
  (:import-from #:naming.infra.db-connection
                #:close-mysql-connection
                #:open-mysql-connection)
  (:import-from #:naming.repository.letter
                #:<mysql-letter-repository>)
  (:import-from #:naming.web.app
                #:*app*))

(in-package #:naming.web.controller.add-letter)

(defclass <http-params> ()
  ((params
    :documentation "来自ningle框架的输入参数"
    :initarg :params
    :reader http-params-params)))

(defmethod get-content ((params <http-params>))
  "从HTTP请求中提取字本身。"
  (let* ((params (http-params-params params)))
    ;;--- TODO: 要求content必填。
    (char (cdr (assoc "content" params :test #'string=)) 0)))

(defmethod get-pinyins ((params <http-params>))
  "从HTTP请求中提取拼音和声调。"
  (let* ((params (http-params-params params))
         (pinyins (cdr (assoc "pinyins" params :test #'string=))))
    (mapcar #'(lambda (pinyin-alist)
                ;; JSON读进来是alist
                (make-instance '<pinyin>
                               :content (cdr (assoc "content" pinyin-alist :test #'string=))
                               :tone (cdr (assoc "tone" pinyin-alist :test #'string=))))
            pinyins)))

;;; add-letter需要符合ningle框架的要求。
(defun add-letter (params)
  (let* ((http-params (make-instance '<http-params> :params params))
         (mysql-connection (open-mysql-connection)))
    (unwind-protect
         (let* ((letter-repository (make-instance '<mysql-letter-repository>
                                                  :connection mysql-connection))
                (use-case (make-instance '<use-case>
                                         :letter-repository letter-repository
                                         :params http-params)))
           (let ((letter (run use-case)))
             (jonathan:to-json
              (list :id (letter-id letter)))))
      (close-mysql-connection mysql-connection))))

(setf (ningle:route *app* "/letter" :method :post) #'add-letter)
