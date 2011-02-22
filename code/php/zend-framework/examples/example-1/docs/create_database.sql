CREATE  TABLE `e`.`notebook` (
  `notebook_id` INT NOT NULL ,
  `title` VARCHAR(45) NOT NULL ,
  `description` MEDIUMTEXT NULL ,
  `date_joined` DATETIME NOT NULL ,
  `status` TINYINT(1)  NULL ,
  PRIMARY KEY (`notebook_id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;
