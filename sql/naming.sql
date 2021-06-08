DROP TABLE IF EXISTS `t_letter`;
CREATE TABLE `t_letter` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `content` CHAR(1) NOT NULL COMMENT '字本身',
  `radicals` CHAR(1) DEFAULT '' COMMENT '偏旁部首',
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
