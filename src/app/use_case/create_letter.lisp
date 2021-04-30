(in-package #:cl-user)

(defpackage #:naming.app.use-case.create-letter
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:add
                #:letter))

(in-package #:naming.app.use-case.create-letter)

(defgeneric get-content (params)
  (:documentation "获取字的内容。"))

(defclass use-case ()
  ((letter-repository
    :initarg :letter-repository
    :reader use-case-letter-repository)
   (params
    :initarg :params
    :reader use-case-params))
  (:documentation "新建一个字的用例。"))

;;;--- TODO: run函数应当在一个单独的文件中定义
(defgeneric run (use-case)
  (:documentation "新建一个字。"))

(defmethod run ((use-case use-case))
  "执行新建一个字的用例。"
  (let ((params (use-case-params use-case))
        (letter-repository (use-case-letter-repository use-case)))
    (let ((content (get-content params)))
      (let ((letter (make-instance 'letter :content content)))
        (add letter-repository letter)
        letter))))
