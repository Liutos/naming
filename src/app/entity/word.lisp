(in-package #:cl-user)

(defpackage #:naming.app.entity.word
  (:use #:cl))

(in-package #:naming.app.entity.word)

(defclass word ()
  ((letters
    :initarg :letters))
  (:documentation "一个中文的词语"))
