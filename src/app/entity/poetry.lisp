(in-package #:cl-user)

(defpackage #:naming.app.entity.poetry
  (:use #:cl))

(in-package #:naming.app.entity.poetry)

(defclass <poetry-sentences> ()
  ((contents
    :documentation "诗中完整的两句的内容。"
    :initarg :contents
    :reader poetry-sentences-contents)
   (id
    :initarg :id)
   (letter-ids
    :accessor poetry-sentences-letter-ids
    :documentation "这两句诗中的字的ID"
    :initarg :letter-ids
    :initform nil))
  (:documentation "诗中的两句。"))

(defclass <poetry> ()
  ((author
    :initarg :author
    :reader poetry-author
    :type string)
   (id
    :accessor poetry-id
    :initarg :id
    :initform nil)
   (pairs
    :documentation "诗中的每两句组成的列表。"
    :initarg :pairs
    :initform nil
    :reader poetry-pairs)
   (title
    :initarg :title
    :reader poetry-title
    :type string)))

(defgeneric add (repository poetry)
  (:documentation "保存一首诗。"))

(defgeneric find-sentences-if-contain (repository letter-ids)
  (:documentation "找出同时含有LETTER-IDS中的字的诗句。"))
