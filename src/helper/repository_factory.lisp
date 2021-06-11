(in-package #:cl-user)

(defpackage #:naming.helper.repository-factory
  (:use #:cl)
  (:import-from #:naming.infra.db-connection
                #:open-mysql-connection)
  (:import-from #:naming.repository.idiom
                #:<mysql-idiom-repository>)
  (:import-from #:naming.repository.letter
                #:<mysql-letter-repository>)
  (:import-from #:naming.repository.poetry
                #:<mysql-poetry-repository>))

(in-package #:naming.helper.repository-factory)

(defclass <repository-factory> ()
  ((connection
    :initarg :connection))
  (:documentation "仓库的工厂。"))

(defgeneric make-repository (factory type)
  (:documentation "生成不同的实体的仓库。"))

(defmethod make-repository ((factory <repository-factory>) (type (eql :idiom)))
  (with-slots (connection) factory
    (make-instance '<mysql-idiom-repository>
                   :connection connection)))

(defmethod make-repository ((factory <repository-factory>) (type (eql :letter)))
  (with-slots (connection) factory
    (make-instance '<mysql-letter-repository>
                   :connection connection)))

(defmethod make-repository ((factory <repository-factory>) (type (eql :poetry)))
  (with-slots (connection) factory
    (make-instance '<mysql-poetry-repository>
                   :connection connection)))

(defvar *mysql-repository-factory*
  (make-instance '<repository-factory>
                 :connection (open-mysql-connection)))
