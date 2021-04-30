(in-package #:cl-user)

(defpackage #:naming.tests.use-case.create-letter
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:add
                #:letter
                #:letter-content
                #:letter-id)
  (:import-from #:naming.app.use-case.create-letter
                #:get-content
                #:run
                #:use-case))

(in-package #:naming.tests.use-case.create-letter)

(defclass mock-letter-repository ()
  ())

(defmethod add ((repository mock-letter-repository) (letter letter))
  "给一个字的实体添加唯一标识。"
  (declare (ignorable repository))
  (setf (letter-id letter) 233))

(defclass mock-params ()
  ())

;;;--- TODO: 如何约束defmethod返回正确的类型的值呢？
(defmethod get-content ((params mock-params))
  (return-from get-content #\汉))

(defun test-create-letter ()
  (let ((use-case (make-instance 'use-case
                                 :letter-repository (make-instance 'mock-letter-repository)
                                 :params (make-instance 'mock-params))))
    (let ((letter (run use-case)))
      (assert (char= (letter-content letter) #\汉))
      (assert (= (letter-id letter) 233)))))
