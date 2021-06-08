(in-package #:cl-user)

(defpackage #:naming.app.use-case.find-letter-by-pinyin
  (:use #:cl)
  (:import-from #:naming.app.entity.letter
                #:find-by-pinyin
                #:query))

(in-package #:naming.app.use-case.find-letter-by-pinyin)

(defgeneric get-pinyin (params)
  (:documentation "要求的发音"))

(defgeneric get-radicals (params)
  (:documentation "要求的部首。"))

(defclass <use-case> ()
  ((letter-repository
    :initarg :letter-repository
    :reader use-case-letter-repository)
   (params
    :initarg :params
    :reader use-case-params))
  (:documentation "根据拼音搜索汉字"))

;;;--- TODO: run函数应当在一个单独的文件中定义
(defgeneric run (use-case)
  (:documentation "执行用例"))

(defmethod run ((use-case <use-case>))
  (let ((params (use-case-params use-case))
        (letter-repository (use-case-letter-repository use-case)))
    (let ((pinyin (get-pinyin params))
          (radicals (get-radicals params)))
      (query letter-repository
             :pinyin pinyin
             :radicals radicals))))
