(in-package #:cl-user)

(defpackage #:naming.app.entity.letter
  (:use #:cl))

(in-package #:naming.app.entity.letter)

(defclass <pinyin> ()
  ((content
    :initarg :content
    :reader pinyin-content)
   (tone
    :accessor pinyin-tone
    :initarg :tone))
  (:documentation "拼音值类型"))

(defclass <letter> ()
  ((content
    :initarg :content
    :reader letter-content
    :type character)
   (id
    :accessor letter-id
    :initarg :id
    :initform nil
    :type integer)
   (pinyins
    :accessor letter-pinyins
    :initarg :pinyins)
   (radicals
    :accessor letter-radicals
    :documentation "字的偏旁部首。

有些字没有偏旁部首，如桛，因此该字段有可能为空。"
    :initarg :radicals
    :initform nil
    :type character)
   (stroke
    :accessor letter-stroke
    :documentation "笔画数"
    :initarg :stroke
    :initform nil
    :type integer))
  (:documentation "汉字值类型"))

(defgeneric add (repository letter)
  (:documentation "将汉字保存到仓库中"))

(defgeneric find-by-content (repository content)
  (:documentation "找出这么写的字。"))

(defgeneric find-by-pinyin (repository pinyin)
  (:documentation "找出相同发音的汉字"))

(defgeneric query (repository &rest args &key pinyin radicals)
  (:documentation "找出符合要求的汉字。

如果PINYIN不为空，则必须为一个`<PINYIN>'类型的对象，查询结果的汉字必须为该发音；
如果RADICALS不为空，则查询结果的汉字必须为该部首。"))

(defun pinyin-content-bound-p (pinyin)
  (slot-boundp pinyin 'content))

(defun pinyin-tone-bound-p (pinyin)
  (slot-boundp pinyin 'tone))
