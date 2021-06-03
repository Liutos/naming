(in-package #:cl-user)

(defpackage #:naming.tests.use-case.add-idiom
  (:use #:cl)
  (:import-from #:naming.app.entity.idiom
                #:<idiom>
                #:add
                #:idiom-id
                #:idiom-letters)
  (:import-from #:naming.app.entity.letter
                #:<letter>
                #:find-by-content
                #:letter-content)
  (:import-from #:naming.app.use-case.add-idiom
                #:<use-case>
                #:get-content
                #:run))

(in-package #:naming.tests.use-case.add-idiom)

(defclass <mock-params> ()
  ())

(defclass <mock-idiom-repository> ()
  ())

(defclass <mock-letter-repository> ()
  ())

(defmethod add ((repository <mock-idiom-repository>) (idiom <idiom>))
  (setf (idiom-id idiom) 233))

(defmethod find-by-content ((repository <mock-letter-repository>) (content character))
  (declare (ignorable repository))
  (make-instance '<letter> :content content))

(defmethod get-content ((params <mock-params>))
  "刘郎前度")

(defun test-add-idiom ()
  "测试添加成语的用例逻辑。"
  (let* ((idiom-repository (make-instance '<mock-idiom-repository>))
         (letter-repository (make-instance '<mock-letter-repository>))
         (params (make-instance '<mock-params>))
         (use-case (make-instance '<use-case>
                                  :idiom-repository idiom-repository
                                  :letter-repository letter-repository
                                  :params params)))
    (let ((idiom (run use-case)))
      (assert (= (idiom-id idiom) 233))
      (let ((letters (idiom-letters idiom)))
        (assert (= (length letters) 4))
        (assert (char= (letter-content (nth 0 letters)) #\刘))
        (assert (char= (letter-content (nth 1 letters)) #\郎))
        (assert (char= (letter-content (nth 2 letters)) #\前))
        (assert (char= (letter-content (nth 3 letters)) #\度))))))
