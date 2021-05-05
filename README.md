# 约定

- 中文的一个字的标识符为`letter`；
- 中文的一个词的标识符为`word`。

# 系统要实现的功能

- 根据拼音搜索汉字；

# 系统中的实体及其业务规则

- 字；
  - 一个字的信息有：Unicode字符、拼音、ID；
  - 一个字可以有多个读音；
- 拼音；
  - 拼音是一种值对象，只要写出来是相同的，那么不管这个拼音属于哪一个字，都是一样的拼音；
  - 拼音的内容只能包含26个英文字母；
  - 拼音的声调只有5种；
  
拼音值对象的类型定义为：

```lisp
(defclass <pinyin> ()
  ((content
    :initarg :content
    :reader pinyin-content)
   (tone
    :initarg :tone
    :reader pinyin-tone)))
```

字实体类型的定义为：

```lisp
(defclass <letter> ()
  ((content
    :initarg :content
    :reader letter-content)
   (id
    :accessor letter-id
    :initarg :id)
   (pinyins
    :initarg :pinyins
    :reader letter-pinyins)))
```
  
# 围绕字的用例

- 录入一个字，输入为：Unicode字符、拼音。输出为：字的实体对象；
- 查询出给定发音的所有字，输入为：拼音。输出为：字的实体对象的列表；

用例的类定义为：

```lisp
;;; 文件app/use_case/use_case_interface.lisp
(defgeneric run (use-case)
  (:documentation "执行用例。"))

;;; 文件app/use_case/add_letter.lisp
(defclass <use-case> ()
  ((letter-repository
    :initarg :letter-repository
    :reader use-case-letter-repository)
   (params
    :initarg :params
    :reader use-case-params))
  (:documentation "录入一个字的用例。"))

(defgeneric get-content (params)
  (:documentation "获取要录入的字的字符。"))

(defgeneric get-pinyins (params)
  (:documentation "获取该字的拼音列表。"))

(defmethod run ((use-case use-case))
  ())
```

# 仓库提供的方法

- 负责将一个字保管到仓库中的`add`方法，输入为：`letter`字对象。输出为：`letter`，其ID被填充；
- 负责根据发音查询字的`find_by_pinyin`方法，输入为：`pinyin`，拼音值对象。输出为：`letter`列表；

# 实现用例的形态

- 优先实现cli形态的应用，以便在脱离微信小程序的场景中使用；

# 数据库存储结构

存储拼音、字的关系的表结构为：

```sql
CREATE TABLE `t_letter` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `content` CHAR(1) NOT NULL COMMENT '字本身',
  PRIMARY KEY (`id`)
);

CREATE TABLE `t_letter_pinyin` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `letter_id` INT NOT NULL,
  `content` VARCHAR(10) NOT NULL,
  `tone` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `ix__letter_id` (`letter_id`),
  INDEX `ix__content__tone` (`content`, `tone`)
);
```

# 参考资料

- https://www.cliki.net/naming+conventions
