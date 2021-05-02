(in-package #:cl-user)

(defpackage #:naming.tests.use-case.create-word
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:letter)
  (:import-from #:naming.app.entity.word
                #:add
                #:word
                #:word-id)
  (:import-from #:naming.app.use-case.create-word
                #:get-letters
                #:run
                #:use-case))

(in-package #:naming.tests.use-case.create-word)

(defclass mock-params ()
  ())

(defclass mock-word-repository ()
  ())

(defmethod add ((repository mock-word-repository) (word word))
  (setf (word-id word) 233))

(defmethod get-letters ((params mock-params))
  (let* ((contents (coerce "永垂不朽" 'list))
         (id 1)
         (letters '()))
    (dolist (content contents)
      (push (make-instance 'letter
                           :content content
                           :id id)
            letters)
      (incf id))
    (nreverse letters)))

(defun test-create-word ()
  (let* ((params (make-instance 'mock-params))
         (word-repository (make-instance 'mock-word-repository))
         (use-case (make-instance 'use-case
                                  :params params
                                  :word-repository word-repository)))
    (let ((word (run use-case)))
      (assert (= (word-id word) 233)))))
