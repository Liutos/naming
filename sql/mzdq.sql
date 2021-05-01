CREATE TABLE `t_letter` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `content` CHAR(1) NOT NULL COMMENT '字本身',
  PRIMARY KEY (`id`),
  INDEX `ix__content` (`content`)
);
