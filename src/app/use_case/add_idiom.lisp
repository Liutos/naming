(in-package #:cl-user)

(defpackage #:naming.app.use-case.add-idiom
  (:use #:cl)
  (:import-from #:naming.app.entity.idiom
                #:<idiom>
                #:add
                #:idiom-id)
  (:import-from #:naming.app.entity.letter
                #:find-by-content))

(in-package #:naming.app.use-case.add-idiom)

(defclass <use-case> ()
  ((idiom-repository
    :initarg :idiom-repository
    :reader use-case-idiom-repository)
   (letter-repository
    :initarg :letter-repository
    :reader use-case-letter-repository)
   (params
    :initarg :params
    :reader use-case-params))
  (:documentation "添加一个成语的用例。"))

(define-condition <letter-not-found> ()
  ((letter-content
    :initarg :letter-content))
  (:documentation "表示成语中有一个字没找到。"))

(defgeneric get-content (params)
  (:documentation "从参数中获取成语的内容。"))

(defgeneric get-ignore-not-found-p (params)
  (:documentation "获取是否忽略不存在字符的参数。"))

(defgeneric run (use-case)
  (:documentation "执行用例。"))

(defmethod run ((use-case <use-case>))
  (with-slots (idiom-repository letter-repository params)
      use-case
    (let* ((content (get-content params))
           (letters '())
           (letter-contents (coerce content 'list)))
      (dolist (letter-content letter-contents)
        (let ((letter (find-by-content letter-repository letter-content)))
          ;;--- TODO: 用CL的restart机制来代替参数。
          (when (and (null letter)
                     (get-ignore-not-found-p params))
            (signal '<letter-not-found> :letter-content letter-content))
          (when letter
            (push letter letters))))
      (setf letters (nreverse letters))
      (let ((idiom (make-instance '<idiom> :letters letters)))
        (add idiom-repository idiom)
        idiom))))
