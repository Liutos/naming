(in-package #:cl-user)

(defpackage #:naming.web.controller.find-letter-group
  (:use #:cl)
  (:import-from #:naming.app.entity.idiom
                #:idiom-content)
  (:import-from #:naming.app.entity.letter
                #:<pinyin>
                #:letter-content
                #:pinyin-tone)
  (:import-from #:naming.infra.db-connection
                #:open-mysql-connection)
  (:import-from #:naming.repository.idiom
                #:<mysql-idiom-repository>)
  (:import-from #:naming.repository.letter
                #:<mysql-letter-repository>)
  (:import-from #:naming.app.use-case.find-letter-group
                #:<letter-specification>
                #:<use-case>
                #:get-specification
                #:run)
  (:import-from #:naming.web.app
                #:*app*))

(in-package #:naming.web.controller.find-letter-group)

(defclass <http-params> ()
  ((params
    :documentation "来自ningle框架的输入参数"
    :initarg :params
    :reader http-params-params)))

(defmethod get-specification ((params <http-params>))
  "从HTTP请求中提取对汉字的要求。"
  (let* ((params (http-params-params params))
         (specifications (cdr (assoc "specifications" params :test #'string=))))
    (mapcar #'(lambda (specification)
                (let ((pinyins (cdr (assoc "pinyins" specification :test #'string=)))
                      (radicals (cdr (assoc "radicals" specification :test #'string=))))
                  (make-instance '<letter-specification>
                                 :pinyins (mapcar #'(lambda (pinyin)
                                                      (make-instance '<pinyin>
                                                                     :content (cdr (assoc "content" pinyin :test #'string=))
                                                                     :tone (cdr (assoc "tone" pinyin :test #'string=))))
                                                  pinyins)
                                 :radicals (mapcar #'(lambda (radicals)
                                                       (char radicals 0))
                                                   radicals))))
            specifications)))

(defun find-letter-group (params)
  (let* ((http-params (make-instance '<http-params> :params params))
         (mysql-connection (open-mysql-connection))
         (idiom-repository (make-instance '<mysql-idiom-repository>
                                          :connection mysql-connection))
         (letter-repository (make-instance '<mysql-letter-repository>
                                           :connection mysql-connection))
         (use-case (make-instance '<use-case>
                                  :idiom-repository idiom-repository
                                  :letter-repository letter-repository
                                  :params http-params)))
    (let ((result (run use-case)))
      (list
       200
       (list :content-type "application/json")
       (list (jonathan:to-json
              (mapcar #'(lambda (result)
                          (destructuring-bind (&key first idioms second)
                              result
                            (list :first (letter-content first)
                                  :idioms (mapcar #'(lambda (idiom)
                                                      (list :content (idiom-content idiom)))
                                                  idioms)
                                  :second (letter-content second))))
                      result)))))))

(setf (ningle:route *app* "/letter-group" :method :post) #'find-letter-group)
