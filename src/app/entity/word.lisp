(in-package #:cl-user)

(defpackage #:naming.app.entity.word
  (:use #:cl))

(in-package #:naming.app.entity.word)

(defclass word ()
  ((id
    :documentation "一个词的唯一标识"
    :accessor word-id)
   (letters
    :documentation "构成这个词语的每一个字"
    :initarg :letters))
  (:documentation "一个中文的词语"))

(defgeneric add (repository word)
  (:documentation "将一个词保存起来，供稍后查询。这个词的每一个字必须先保存了。"))
