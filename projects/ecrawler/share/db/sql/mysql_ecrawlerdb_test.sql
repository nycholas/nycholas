SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

CREATE SCHEMA IF NOT EXISTS `ecrawlerdb_test` DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci ;
USE `ecrawlerdb_test`;

-- -----------------------------------------------------
-- Table `ecrawlerdb_test`.`emails`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `ecrawlerdb_test`.`emails` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `email_id` INT NOT NULL ,
  `email_from` VARCHAR(50) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `email_to` VARCHAR(50) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL COMMENT '		' ,
  `email_subject` VARCHAR(200) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `email_date` DATETIME NOT NULL ,
  `email_content` LONGTEXT CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NULL ,
  `email_annex` LONGBLOB NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
