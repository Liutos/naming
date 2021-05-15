(in-package #:cl-user)

(defpackage #:naming.app.use-case.add-letter
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:<letter>
                #:add))

(in-package #:naming.app.use-case.add-letter)

(defgeneric get-content (params)
  (:documentation "接收该字本身。"))

(defgeneric get-pinyins (params)
  (:documentation "接收该字所有的读音。"))

(defclass <use-case> ()
  ((params
    :initarg :params
    :reader use-case-params)
   (letter-repository
    :initarg :letter-repository
    :reader use-case-letter-repository))
  (:documentation "添加一个字的用例。"))

(defgeneric run (use-case)
  (:documentation "执行用例。"))

(defmethod run ((use-case <use-case>))
  (let* ((params (use-case-params use-case))
         (letter-repository (use-case-letter-repository use-case))
         (content (get-content params))
         (pinyins (get-pinyins params))
         (letter (make-instance '<letter>
                                :content content
                                :pinyins pinyins)))
    (add letter-repository letter)
    letter))
