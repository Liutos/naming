(in-package #:cl-user)

(defpackage #:naming.app.entity.idiom
  (:use #:cl))

(in-package #:naming.app.entity.idiom)

(defclass <idiom> ()
  ((id
    :accessor idiom-id)
   (letters
    :documentation "构成这个成语的每一个字"
    :initarg :letters
    :reader idiom-letters))
  (:documentation "一个成语"))

(defgeneric add (repository idiom)
  (:documentation "将一个成语保存起来，供稍后查询。这个成语的每一个字必须先保存了。"))
