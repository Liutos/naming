(in-package #:cl-user)

(defpackage #:naming.app.use-case.add-poetry
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:find-by-content
                #:find-letter-ids
                #:letter-id)
  (:import-from #:naming.app.entity.poetry
                #:<poetry>
                #:<poetry-sentences>
                #:add))

(in-package #:naming.app.use-case.add-poetry)

(defclass <use-case> ()
  ((letter-repository
    :initarg :letter-repository
    :reader use-case-letter-repository)
   (params
    :initarg :params
    :reader use-case-params)
   (poetry-repository
    :initarg :poetry-repository))
  (:documentation "添加一首诗的用例。"))

(defgeneric get-author (params)
  (:documentation "获取诗的作者。"))

(defgeneric get-content (params)
  (:documentation "获取诗的内容。

诗的内容以每两句为一个最小单元，以列表的形式返回。"))

(defgeneric get-title (params)
  (:documentation "获取诗的名字。"))

(defgeneric run (use-case))

(defmethod run ((use-case <use-case>))
  (with-slots (letter-repository params poetry-repository)
      use-case
    (let* ((author (get-author params))
           (content (get-content params))
           (title (get-title params)))
      ;; 找出CONTENT中每一个字的ID，作为LETTER-IDS
      (check-type content list)
      (let* ((pairs '()))
        (dolist (sentence content)
          (let* ((letter-contents (coerce sentence 'list))
                 (letter-ids (find-letter-ids letter-repository letter-contents)))
            ;; (dolist (content letter-contents)
            ;;   (let ((letter (find-by-content letter-repository content)))
            ;;     (when letter
            ;;       (push (letter-id letter) letter-ids))))
            (push (make-instance '<poetry-sentences>
                                 :contents sentence
                                 :letter-ids letter-ids)
                  pairs)))
        (let ((poetry (make-instance '<poetry>
                                     :author author
                                     :pairs pairs
                                     :title title)))
          (add poetry-repository poetry)
          poetry)))))
