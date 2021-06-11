DROP TABLE IF EXISTS `t_letter`;
CREATE TABLE `t_letter` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `content` CHAR(1) NOT NULL COMMENT '字本身',
  `radicals` CHAR(1) DEFAULT '' COMMENT '偏旁部首',
  `stroke` INT NOT NULL COMMENT '笔画数',
  PRIMARY KEY (`id`),
  INDEX `ix__radicals` (`radicals`)
);

DROP TABLE IF EXISTS `t_letter_pinyin`;
CREATE TABLE `t_letter_pinyin` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `letter_id` INT NOT NULL,
  `content` VARCHAR(10) NOT NULL,
  `tone` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `ix__letter_id` (`letter_id`),
  INDEX `ix__content__tone` (`content`, `tone`)
);

DROP TABLE IF EXISTS `t_idiom`;
CREATE TABLE `t_idiom` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `content` VARCHAR(100) NOT NULL COMMENT '成语本身',
  PRIMARY KEY (`id`),
  INDEX `ix__content` (`content`)
);

DROP TABLE IF EXISTS `t_idiom_letter`;
CREATE TABLE `t_idiom_letter` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `idiom_id` INT NOT NULL,
  `letter_id` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `ix__idiom_id` (`idiom_id`),
  INDEX `ix__letter_id` (`letter_id`)
);

DROP TABLE IF EXISTS `t_poetry`;
CREATE TABLE `t_poetry` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `author` VARCHAR(100) DEFAULT '',
  `title` VARCHAR(1000) DEFAULT '',
  PRIMARY KEY (`id`),
  INDEX `ix__content` (`content`(255))
);

DROP TABLE IF EXISTS `t_poetry_sentence`;
CREATE TABLE `t_poetry_sentence` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `poetry_id` INT NOT NULL,
  `content` TEXT NOT NULL COMMENT '诗中两句的内容',
  PRIMARY KEY (`id`),
  INDEX `ix__content` (`content`(255)),
  INDEX `ix__poetry_id` (`poetry_id`)
);

DROP TABLE IF EXISTS `t_letter_poetry_sentence`;
CREATE TABLE `t_letter_poetry_sentence` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `letter_id` INT NOT NULL,
  `poetry_sentence_id` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `ix__letter_id` (`letter_id`),
  INDEX `ix__poetry_sentence_id` (`poetry_sentence_id`)
);
