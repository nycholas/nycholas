SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

CREATE SCHEMA IF NOT EXISTS `cruddb` DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci ;
USE `cruddb`;

-- -----------------------------------------------------
-- Table `cruddb`.`notebook`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `cruddb`.`notebook` (
  `id` INT ZEROFILL NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(100) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `description` LONGTEXT CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `date_joined` DATETIME NOT NULL ,
  `date_changed` DATETIME NULL ,
  `is_active` TINYINT NOT NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
