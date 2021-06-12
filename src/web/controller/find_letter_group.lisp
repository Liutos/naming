(in-package #:cl-user)

(defpackage #:naming.web.controller.find-letter-group
  (:use #:cl)
  (:import-from #:naming.app.entity.idiom
                #:idiom-content)
  (:import-from #:naming.app.entity.letter
                #:<pinyin>
                #:letter-content
                #:pinyin-tone)
  (:import-from #:naming.helper.repository-factory
                #:*mysql-repository-factory*
                #:make-repository)
  (:import-from #:naming.infra.db-connection
                #:open-mysql-connection)
  (:import-from #:naming.repository.idiom
                #:<mysql-idiom-repository>)
  (:import-from #:naming.repository.letter
                #:<mysql-letter-repository>)
  (:import-from #:naming.app.use-case.find-letter-group
                #:<letter-specification>
                #:<use-case>
                #:get-source
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

(defmethod get-source ((params <http-params>))
  (with-slots (params) params
    (let ((source (cdr (assoc "source" params :test #'string=))))
      (when (stringp source)
        (intern (string-upcase source) :keyword)))))

(defmethod get-specification ((params <http-params>))
  "从HTTP请求中提取对汉字的要求。"
  (let* ((params (http-params-params params))
         (specifications (cdr (assoc "specifications" params :test #'string=))))
    (mapcar #'(lambda (specification)
                (let ((contents (cdr (assoc "contents" specification :test #'string=)))
                      (pinyins (cdr (assoc "pinyins" specification :test #'string=)))
                      (radicals (cdr (assoc "radicals" specification :test #'string=))))
                  (make-instance '<letter-specification>
                                 :contents (mapcar #'(lambda (content)
                                                       (char content 0))
                                                   contents)
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
         (idiom-repository (make-repository *mysql-repository-factory* :idiom))
         (letter-repository (make-repository *mysql-repository-factory* :letter))
         (poetry-repository (make-repository *mysql-repository-factory* :poetry))
         (use-case (make-instance '<use-case>
                                  :idiom-repository idiom-repository
                                  :letter-repository letter-repository
                                  :params http-params
                                  :poetry-repository poetry-repository)))
    (let ((result (run use-case)))
      (list
       200
       (list :content-type "application/json")
       (list (jonathan:to-json
              (mapcar #'(lambda (result)
                          (destructuring-bind (&key first idioms poetry-sentences second)
                              result
                            (list :first (string (letter-content first))
                                  :idioms (mapcar #'(lambda (idiom)
                                                      (list :content (idiom-content idiom)))
                                                  idioms)
                                  :poetry-sentences poetry-sentences
                                  :second (string (letter-content second)))))
                      result)))))))

(setf (ningle:route *app* "/letter-group" :method :post) #'find-letter-group)
