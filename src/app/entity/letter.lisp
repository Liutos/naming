(in-package #:cl-user)

(defpackage #:naming.app.entity.letter
  (:use #:cl))

(in-package #:naming.app.entity.letter)

(defclass <pinyin> ()
  ((content
    :initarg :content
    :reader pinyin-content)
   (tone
    :initarg :tone
    :reader pinyin-tone))
  (:documentation "拼音值类型"))

(defclass <letter> ()
  ((content
    :initarg :content
    :reader letter-content
    :type character)
   (id
    :accessor letter-id)
   (pinyins
    :initarg :pinyins
    :reader letter-pinyins))
  (:documentation "汉字值类型"))

(defgeneric add (repository letter)
  (:documentation "将汉字保存到仓库中"))

(defgeneric find-by-pinyin (repository pinyin)
  (:documentation "找出相同发音的汉字"))

(defun pinyin-content-bound-p (pinyin)
  (slot-boundp pinyin 'content))

(defun pinyin-tone-bound-p (pinyin)
  (slot-boundp pinyin 'tone))
