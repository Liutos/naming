(in-package #:cl-user)

(defpackage #:naming.app.use-case.find-idiom-by-letter
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:find-by-content
                #:letter-id)
  (:import-from #:naming.app.entity.idiom
                #:query))

(in-package #:naming.app.use-case.find-idiom-by-letter)

(defgeneric get-letter-content (params)
  (:documentation "获取要查找的字。"))

(defgeneric run (use-case)
  (:documentation "执行用例。"))

(defclass <use-case> ()
  ((idiom-repository
    :initarg :idiom-repository)
   (letter-repository
    :initarg :letter-repository)
   (params
    :initarg :params
    :reader use-case-params))
  (:documentation "根据文字查询成语的用例。"))

(defmethod run ((use-case <use-case>))
  (with-slots (idiom-repository letter-repository params) use-case
    (let* ((letter-content (get-letter-content params))
           letter)
      (check-type letter-content character)
      (setf letter (find-by-content letter-repository letter-content))
      (unless letter
        (return-from run nil))
      (query idiom-repository
             :letter-id (letter-id letter)))))
