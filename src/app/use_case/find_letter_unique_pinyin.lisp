(in-package #:cl-user)

(defpackage #:naming.app.use-case.find-letter-unique-pinyin
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:<pinyin>
                #:letter-pinyins
                #:letter-stroke
                #:pinyin-content
                #:pinyin-tone
                #:query)
  (:import-from #:ruyi
                #:vertical-let*))

(in-package #:naming.app.use-case.find-letter-unique-pinyin)

(defclass <use-case> ()
  ((letter-repository
    :initarg :letter-repository)
   (params
    :initarg :params))
  (:documentation "为每个拼音找到一个汉字。"))

(defgeneric get-exclusive-initials (params)
  (:documentation "指定要排除的声母。"))

(defgeneric get-exclusive-vowels (params)
  (:documentation "指定要排除的韵母。"))

(defgeneric run (use-case))

(defun match-exclusive-initials (use-case pinyin-content)
  "如果拼音属于要排除的声母，就返回T。"
  (check-type use-case <use-case>)
  (check-type pinyin-content string)
  (with-slots (params) use-case
    (let ((initials (get-exclusive-initials params)))
      (and initials
           (some #'(lambda (initial)
                     (str:starts-with-p initial pinyin-content))
                 initials)))))

(defun match-exclusive-vowels (use-case pinyin-content)
  "如果拼音属于要排除的韵母，就返回T。"
  (check-type use-case <use-case>)
  (check-type pinyin-content string)
  (with-slots (params) use-case
    (let ((vowels (get-exclusive-vowels params)))
      (and vowels
           (some #'(lambda (vowel)
                     (str:starts-with-p vowel pinyin-content))
                 vowels)))))

(defmethod run ((use-case <use-case>))
  (with-slots (letter-repository params) use-case
    (vertical-let*
      :with exclusive-initials = (get-exclusive-initials params)
      ;;--- TODO: 暂时固定为3、4声。
      :with pinyins = (list
                       (make-instance '<pinyin> :tone 3)
                       (make-instance '<pinyin> :tone 4))
      :with letters = (query letter-repository :pinyin pinyins)
      (format t "找到了~D个汉字~%" (length letters))
      ;; 对于所有搜索到的字，按照“拼音+声调”聚合为笔画数最少的一个
      ;; 同时要剔除所有以j为声母的字。
      :with pinyin-letter-table = (make-hash-table :test #'equal)
      (dolist (letter letters)
        (vertical-let*
          :with pinyin = (first (letter-pinyins letter))
          :with key = (format nil "~A~D" (pinyin-content pinyin) (pinyin-tone pinyin))
          :with (:values letters found) = (gethash key pinyin-letter-table)
          (declare (ignorable letters))
          :with pinyin-content = (pinyin-content pinyin)
          (cond
            ((match-exclusive-initials use-case pinyin-content)
             ;; 什么也不做
             )
            ((match-exclusive-vowels use-case pinyin-content)
             ;; 什么也不做
             )
            ((not found)
             (setf (gethash key pinyin-letter-table) (list letter)))
            (t
             (push letter
                   (gethash key pinyin-letter-table))))))
      ;; 重新遍历哈希表并按照笔画数递增排列即可。
      :with result = nil
      (maphash #'(lambda (pronunciation letters)
                   (let ((sorted (sort letters
                                       #'(lambda (l1 l2)
                                           (< (letter-stroke l1)
                                              (letter-stroke l2))))))
                     (push (list :pronunciation pronunciation
                                 :letter (first sorted))
                           result)))
               pinyin-letter-table)
      result)))
