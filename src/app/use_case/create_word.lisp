(in-package #:cl-user)

(defpackage #:naming.app.use-case.create-word
  (:use #:cl)
  (:import-from #:naming.app.entity.word
                #:add
                #:word))

(in-package #:naming.app.use-case.create-word)

(defgeneric get-letters (params)
  (:documentation "获取组成这个词的每一个字。"))

(defclass use-case ()
  ((params
    :initarg :params
    :reader use-case-params)
   (word-repository
    :initarg :word-repository
    :reader use-case-word-repository))
  (:documentation "新建一个词的用例。"))

(defgeneric run (use-case)
  (:documentation "新建一个词。"))

(defmethod run ((use-case use-case))
  "执行新建一个字的用例。"
  (let ((params (use-case-params use-case))
        (word-repository (use-case-word-repository use-case)))
    (let* ((letters (get-letters params))
           (word (make-instance 'word :letters letters)))
      (add word-repository word)
      word)))
