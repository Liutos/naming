(in-package #:cl-user)

(defpackage #:naming.app.entity.letter
  (:use #:cl))

(in-package #:naming.app.entity.letter)

(defclass letter ()
  ((content
    :initarg :content
    :reader letter-content
    :type character)
   (id
    :accessor letter-id
    :initarg :id
    :type (or integer null)))
  (:documentation "一个中文的字"))

(defgeneric add (repository letter)
  (:documentation "将一个字保存起来，供稍后查询。"))
