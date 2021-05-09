DROP TABLE IF EXISTS `t_letter`;
CREATE TABLE `t_letter` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `content` CHAR(1) NOT NULL COMMENT '字本身',
  PRIMARY KEY (`id`)
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
