(in-package #:cl-user)

(defpackage #:naming.app.use-case.find-letter-group
  (:use #:cl)
  (:import-from #:naming.app.entity.idiom
                #:find-if-contain
                #:idiom-id
                #:idiom-letter-ids)
  (:import-from #:naming.app.entity.letter
                #:<pinyin>
                #:letter-content
                #:letter-id
                #:letter-stroke
                #:query)
  (:import-from #:naming.app.entity.poetry
                #:find-sentences-if-contain
                #:poetry-sentences-contents
                #:poetry-sentences-letter-ids))

(in-package #:naming.app.use-case.find-letter-group)

(defclass <letter-specification> ()
  ((contents
    :documentation "这个字本身"
    :initarg :contents
    :initform nil)
   (exclusives
    :documentation "要排除的汉字列表。"
    :initarg :exclusives
    :initform nil)
   (pinyins
    :initarg :pinyins
    :initform nil)
   (radicals
    :initarg :radicals
    :initform nil))
  (:documentation "对一个目标汉字的描述。"))

(defgeneric get-source (params)
  (:documentation "指定两个字来源于哪里。

- :IDIOM，表示来自于同一个成语；
- :POETRY，表示来自于同一首诗。"))

(defgeneric get-specification (params)
  (:documentation "获取对两个汉字的部首、拼音的要求。

返回值为一个长度为2的列表，其元素类型为`<LETTER-SPECIFICATION>'。"))

(defgeneric run (use-case))

(defclass <use-case> ()
  ((idiom-repository
    :initarg :idiom-repository)
   (letter-repository
    :initarg :letter-repository
    :reader use-case-letter-repository)
   (params
    :initarg :params
    :reader use-case-params)
   (poetry-repository
    :initarg :poetry-repository))
  (:documentation "搜索符合要求的成对的汉字。"))

(define-condition <param-error> ()
  ((message
    :initarg :message
    :reader param-error-message))
  (:documentation "表示参数存在错误。"))

(defun validate-specification (specification)
  "检查CONTENTS/PINYINS/RADICALS是否至少存在一个。"
  (with-slots (contents pinyins radicals)
      specification
    (when (every #'null (list contents pinyins radicals))
      (error '<param-error> :message "CONTENTS/PINYINS/RADICALS必须至少填充一个"))))

(defmethod run ((use-case <use-case>))
  "返回结果为一个列表，其中每个元素都为一个plist。plist中有三个属性：

- :FIRST，表示第一个字；
- :SECOND，表示第二个字；
- :IDIOMS，表示含有这两个字的成语的列表。"
  (with-slots (idiom-repository letter-repository params poetry-repository)
      use-case
    (let ((specifications (get-specification params)))
      (assert (= (length specifications) 2))
      ;; 先分别找出符合条件的第一和第二个字，再对它们的笛卡尔积判断是否有成语。
      (let ((idiom-table (make-hash-table))
            idioms
            (letter-id-to-idiom-ids (make-hash-table))
            letters1
            letters2
            result
            (spec1 (first specifications))
            (spec2 (second specifications)))
        (validate-specification spec1)
        (validate-specification spec2)
        (setf letters1
              (query letter-repository
                     :content (slot-value spec1 'contents)
                     :exclusives (slot-value spec1 'exclusives)
                     :pinyin (slot-value spec1 'pinyins)
                     :radicals (slot-value spec1 'radicals)))
        (setf letters2
              (query letter-repository
                     :content (slot-value spec2 'contents)
                     :exclusives (slot-value spec2 'exclusives)
                     :pinyin (slot-value spec2 'pinyins)
                     :radicals (slot-value spec2 'radicals)))
        (case (get-source params)
          (:idiom
           (progn
             ;; 一次性找出所有的成语，再逐个匹配。
             (setf idioms (naming.app.entity.idiom::query
                           idiom-repository
                           :letter-id (append (mapcar #'letter-id letters1)
                                              (mapcar #'letter-id letters2))))
             ;; 建立字的ID到成语ID的映射，之后只需要检查两个字是否有相同的成语ID即可。
             (dolist (idiom idioms)
               (let ((idiom-id (idiom-id idiom))
                     (letter-ids (idiom-letter-ids idiom)))
                 (setf (gethash idiom-id idiom-table) idiom)
                 (dolist (letter-id letter-ids)
                   (multiple-value-bind (idiom-ids found)
                       (gethash letter-id letter-id-to-idiom-ids)
                     (cond ((not found)
                            (setf (gethash letter-id letter-id-to-idiom-ids) (list idiom-id)))
                           ((not (position idiom-id idiom-ids :test #'eql))
                            (push idiom-id
                                  (gethash letter-id letter-id-to-idiom-ids))))))))
             (dolist (letter1 letters1)
               (dolist (letter2 letters2)
                 ;; 两个字对应的成语有交集即可
                 (let* ((idiom-ids1 (gethash (letter-id letter1) letter-id-to-idiom-ids))
                        (idiom-ids2 (gethash (letter-id letter2) letter-id-to-idiom-ids))
                        (common-idiom-ids (intersection idiom-ids1 idiom-ids2)))
                   (when common-idiom-ids
                     (push (list :first letter1
                                 :idioms (mapcar #'(lambda (idiom-id)
                                                     (gethash idiom-id idiom-table))
                                                 common-idiom-ids)
                                 :second letter2)
                           result)))))))
          (:poetry
           (let ((sentences-list (find-sentences-if-contain poetry-repository
                                                            (append (mapcar #'letter-id letters1)
                                                                    (mapcar #'letter-id letters2))))
                 (sentences-table (make-hash-table)))
             (format t "sentences-list is ~D~%" (length sentences-list))
             ;; 转换为字的ID到<POETRY-SENTENCES>对象的哈希表，方便查找。
             (dolist (sentences sentences-list)
               (let ((letter-ids (poetry-sentences-letter-ids sentences)))
                 (dolist (letter-id letter-ids)
                   (multiple-value-bind (sentences-list found)
                       (gethash letter-id sentences-table)
                     (cond ((not found)
                            (setf (gethash letter-id sentences-table) (list sentences)))
                           ((not (member sentences sentences-list))
                            (push sentences
                                  (gethash letter-id sentences-table))))))))
             ;; 同样是判断两个字有交集即可。
             (dolist (letter1 letters1)
               (dolist (letter2 letters2)
                 (let* ((sentences-list1 (gethash (letter-id letter1) sentences-table))
                        (sentences-list2 (gethash (letter-id letter2) sentences-table))
                        (common-sentences (intersection sentences-list1 sentences-list2)))
                   (when common-sentences
                     (let ((poetry-sentences-list '()))
                       (dolist (sentences common-sentences)
                         (let ((contents (poetry-sentences-contents sentences))
                               (letter-content1 (letter-content letter1))
                               (letter-content2 (letter-content letter2)))
                           (when (< (position letter-content1 contents :test #'char=)
                                    (position letter-content2 contents :test #'char=))
                             (push contents poetry-sentences-list))))
                       (when poetry-sentences-list
                         (push (list :first letter1
                                     :poetry-sentences poetry-sentences-list
                                     :second letter2)
                               result)))))))))
          (t
           (dolist (letter1 letters1)
             (dolist (letter2 letters2)
               (push (list :first letter1
                           :second letter2)
                     result)))))

        (setf result (nreverse result))
        (setf result (sort result #'(lambda (e1 e2)
                                      ;; 总笔画数少的汉字组合排在前面
                                      (let ((first-letter1 (getf e1 :first))
                                            (first-letter2 (getf e2 :first))
                                            (second-letter1 (getf e1 :second))
                                            (second-letter2 (getf e2 :second)))
                                        (< (+ (letter-stroke first-letter1) (letter-stroke second-letter1))
                                           (+ (letter-stroke first-letter2) (letter-stroke second-letter2)))))))))))
