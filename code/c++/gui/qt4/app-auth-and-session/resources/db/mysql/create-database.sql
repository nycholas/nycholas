SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

DROP SCHEMA IF EXISTS `appdb` ;
CREATE SCHEMA IF NOT EXISTS `appdb` DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci ;
USE `appdb`;

-- -----------------------------------------------------
-- Table `appdb`.`auth_group`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`auth_group` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(80) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;


-- -----------------------------------------------------
-- Table `appdb`.`app_content_type`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`app_content_type` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(100) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `app_label` VARCHAR(100) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `model` VARCHAR(100) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;


-- -----------------------------------------------------
-- Table `appdb`.`auth_permission`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`auth_permission` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `app_content_type_id` INT NOT NULL ,
  `name` VARCHAR(100) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `codename` VARCHAR(100) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `fk_auth_permission_app_content_type` (`app_content_type_id` ASC) ,
  CONSTRAINT `fk_auth_permission_app_content_type`
    FOREIGN KEY (`app_content_type_id` )
    REFERENCES `appdb`.`app_content_type` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;


-- -----------------------------------------------------
-- Table `appdb`.`auth_group_permissions`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`auth_group_permissions` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `auth_permission_id` INT NOT NULL ,
  `auth_group_id` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `fk_auth_group_permissions_auth_permission1` (`auth_permission_id` ASC) ,
  INDEX `fk_auth_group_permissions_auth_group1` (`auth_group_id` ASC) ,
  CONSTRAINT `fk_auth_group_permissions_auth_permission1`
    FOREIGN KEY (`auth_permission_id` )
    REFERENCES `appdb`.`auth_permission` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_auth_group_permissions_auth_group1`
    FOREIGN KEY (`auth_group_id` )
    REFERENCES `appdb`.`auth_group` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci
PACK_KEYS = DEFAULT;


-- -----------------------------------------------------
-- Table `appdb`.`auth_user`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`auth_user` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `username` VARCHAR(45) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `first_name` VARCHAR(45) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `last_name` VARCHAR(80) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `email` VARCHAR(75) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `password` VARCHAR(128) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `is_staff` TINYINT NOT NULL ,
  `is_active` TINYINT NOT NULL COMMENT '	' ,
  `is_superuser` TINYINT NOT NULL ,
  `last_login` DATETIME NOT NULL ,
  `date_joined` DATETIME NOT NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;


-- -----------------------------------------------------
-- Table `appdb`.`auth_user_groups`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`auth_user_groups` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `auth_user_id` INT NOT NULL ,
  `auth_group_id` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `fk_auth_user_groups_auth_user1` (`auth_user_id` ASC) ,
  INDEX `fk_auth_user_groups_auth_group1` (`auth_group_id` ASC) ,
  CONSTRAINT `fk_auth_user_groups_auth_user1`
    FOREIGN KEY (`auth_user_id` )
    REFERENCES `appdb`.`auth_user` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_auth_user_groups_auth_group1`
    FOREIGN KEY (`auth_group_id` )
    REFERENCES `appdb`.`auth_group` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;


-- -----------------------------------------------------
-- Table `appdb`.`app_session`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`app_session` (
  `session_key` VARCHAR(40) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `session_data` LONGTEXT CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `expire_data` DATETIME NOT NULL ,
  PRIMARY KEY (`session_key`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;


-- -----------------------------------------------------
-- Table `appdb`.`app_log`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`app_log` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `auth_user_id` INT NOT NULL ,
  `app_content_type_id` INT NULL ,
  `action_time` DATETIME NOT NULL ,
  `object_id` LONGTEXT CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NULL ,
  `object_repr` VARCHAR(200) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  `action_flag` SMALLINT(5) UNSIGNED NOT NULL ,
  `change_message` LONGTEXT CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NOT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `fk_app_log_auth_user1` (`auth_user_id` ASC) ,
  INDEX `fk_app_log_app_content_type1` (`app_content_type_id` ASC) ,
  CONSTRAINT `fk_app_log_auth_user1`
    FOREIGN KEY (`auth_user_id` )
    REFERENCES `appdb`.`auth_user` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_app_log_app_content_type1`
    FOREIGN KEY (`app_content_type_id` )
    REFERENCES `appdb`.`app_content_type` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;


-- -----------------------------------------------------
-- Table `appdb`.`auth_user_user_permissions`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `appdb`.`auth_user_user_permissions` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `auth_user_id` INT NOT NULL ,
  `auth_permission_id` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `fk_auth_user_user_permissions_auth_user1` (`auth_user_id` ASC) ,
  INDEX `fk_auth_user_user_permissions_auth_permission1` (`auth_permission_id` ASC) ,
  CONSTRAINT `fk_auth_user_user_permissions_auth_user1`
    FOREIGN KEY (`auth_user_id` )
    REFERENCES `appdb`.`auth_user` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_auth_user_user_permissions_auth_permission1`
    FOREIGN KEY (`auth_permission_id` )
    REFERENCES `appdb`.`auth_permission` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
