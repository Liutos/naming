(in-package #:cl-user)

(defpackage #:naming.app.entity.idiom
  (:use #:cl))

(in-package #:naming.app.entity.idiom)

(defclass <idiom> ()
  ((content
    :initarg :content
    :reader idiom-content)
   (id
    :accessor idiom-id
    :initarg :id)
   (letter-ids
    :documentation "该成语含有的字的ID。"
    :initarg :letter-ids
    :initform nil
    :reader idiom-letter-ids)
   (letters
    :documentation "构成这个成语的每一个字"
    :initarg :letters
    :reader idiom-letters))
  (:documentation "一个成语"))

(defgeneric add (repository idiom)
  (:documentation "将一个成语保存起来，供稍后查询。这个成语的每一个字必须先保存了。"))

(defgeneric find-if-contain (repository letter-ids)
  (:documentation "搜索出同时含有LETTER-IDS中的字的成语。"))

(defgeneric query (repository &rest args &key idiom-ids letter-id)
  (:documentation "搜索出符合要求的成语。"))
