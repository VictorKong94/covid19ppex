-- ----------------------------------------------------------------------------
-- MySQL Database Creation
-- ----------------------------------------------------------------------------

SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------------------------------------------------------
-- Schema covid19ppex
-- ----------------------------------------------------------------------------
DROP SCHEMA IF EXISTS covid19ppex;
CREATE SCHEMA IF NOT EXISTS covid19ppex;

-- ----------------------------------------------------------------------------
-- Table covid19ppex.clinics
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS covid19ppex.clinics (
    clinic_pk INT(11) NOT NULL AUTO_INCREMENT
  , clinic_desc VARCHAR(90) CHARACTER SET 'utf8' NOT NULL
  , address1 VARCHAR(45) NULL DEFAULT NULL
  , address2 VARCHAR(45) NULL DEFAULT NULL
  , zip VARCHAR(5) NULL DEFAULT NULL
  , PRIMARY KEY (clinic_pk)
  , UNIQUE INDEX clinic_desc_UNIQUE (clinic_desc ASC) VISIBLE
)
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci
;

-- ----------------------------------------------------------------------------
-- Table covid19ppex.equipment
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS covid19ppex.equipment (
    equipment_pk INT(11) NOT NULL AUTO_INCREMENT
  , equipment_desc VARCHAR(255) CHARACTER SET 'utf8' NOT NULL
  , PRIMARY KEY (equipment_pk)
)
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci
;

-- ----------------------------------------------------------------------------
-- Table covid19ppex.group_clinic_interface
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS covid19ppex.group_clinic_interface (
    group_clinic_pk INT(11) NOT NULL AUTO_INCREMENT
  , group_pk INT(11) NOT NULL
  , clinic_pk INT(11) NOT NULL
  , PRIMARY KEY (group_clinic_pk)
  , INDEX fk_gci_clinic_pk_idx (clinic_pk ASC) VISIBLE,
  , INDEX fk_gci_group_pk_idx (group_pk ASC) VISIBLE
  , CONSTRAINT fk_gci_clinic_pk
      FOREIGN KEY (clinic_pk)
      REFERENCES covid19ppex.clinics (clinic_pk)
  , CONSTRAINT fk_gci_group_pk
      FOREIGN KEY (group_pk)
      REFERENCES covid19ppex.groups (group_pk)
)
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARACTER SET = utf8
;

-- ----------------------------------------------------------------------------
-- Table covid19ppex.groups
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS covid19ppex.groups (
    group_pk INT(11) NOT NULL AUTO_INCREMENT
  , group_desc VARCHAR(90) NULL DEFAULT NULL
  , group_admins VARCHAR(45) NOT NULL
  , PRIMARY KEY (group_pk)
  , UNIQUE INDEX group_desc_UNIQUE (group_desc ASC) VISIBLE)
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARACTER SET = utf8
;

-- ----------------------------------------------------------------------------
-- Table covid19ppex.posts
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS covid19ppex.posts (
    post_pk INT(11) NOT NULL AUTO_INCREMENT
  , type VARCHAR(30) NOT NULL
  , equipment_pk INT(11) NOT NULL
  , units INT(11) NOT NULL
  , user_pk INT(11) NOT NULL
  , clinic_pk INT(11) NOT NULL
  , note TEXT NULL DEFAULT NULL
  , status VARCHAR(30) NOT NULL
  , created_by INT(11) NOT NULL
  , created_on DATETIME NOT NULL
  , modified_by INT(11) NULL DEFAULT NULL
  , modified_on DATETIME NULL DEFAULT NULL
  , PRIMARY KEY (post_pk)
  , INDEX fk_offer_clinic_pk_idx (clinic_pk ASC) VISIBLE
  , INDEX fk_offer_equipment_idx (equipment_pk ASC) VISIBLE
  , INDEX fk_offer_user_pk (user_pk ASC) VISIBLE
  , INDEX fk_offer_created_by (created_by ASC) VISIBLE
  , CONSTRAINT fk_offer_clinic_pk
      FOREIGN KEY (clinic_pk)
      REFERENCES covid19ppex.clinics (clinic_pk)
  , CONSTRAINT fk_offer_created_by
      FOREIGN KEY (created_by)
      REFERENCES covid19ppex.users (user_pk)
  , CONSTRAINT fk_offer_equipment_pk
      FOREIGN KEY (equipment_pk)
      REFERENCES covid19ppex.equipment (equipment_pk)
  , CONSTRAINT fk_offer_user_pk
      FOREIGN KEY (user_pk)
      REFERENCES covid19ppex.users (user_pk)
)
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci
;

-- ----------------------------------------------------------------------------
-- Table covid19ppex.user_clinic_interface
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS covid19ppex.user_clinic_interface (
    user_clinic_pk INT(11) NOT NULL AUTO_INCREMENT
  , user_pk INT(11) NOT NULL
  , clinic_pk INT(11) NOT NULL
  , is_site_admin TINYINT(1) NOT NULL DEFAULT '0'
  , PRIMARY KEY (user_clinic_pk)
  , INDEX fk_clinic_pk_idx (clinic_pk ASC) VISIBLE
  , INDEX fk_uc_clinic_pk_idx (clinic_pk ASC) VISIBLE
  , INDEX fk_uc_user_pk_idx (user_pk ASC) VISIBLE
  , CONSTRAINT fk_uci_clinic_pk
      FOREIGN KEY (clinic_pk)
      REFERENCES covid19ppex.clinics (clinic_pk)
  , CONSTRAINT fk_uci_user_pk
      FOREIGN KEY (user_pk)
      REFERENCES covid19ppex.users (user_pk)
)
ENGINE = InnoDB
AUTO_INCREMENT = 1
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci
;

-- ----------------------------------------------------------------------------
-- Table covid19ppex.users
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS covid19ppex.users (
    user_pk INT(11) NOT NULL AUTO_INCREMENT
  , email VARCHAR(147) NOT NULL
  , first_name VARCHAR(45) CHARACTER SET 'utf8' NOT NULL
  , middle_name VARCHAR(45) CHARACTER SET 'utf8' NULL DEFAULT NULL
  , last_name VARCHAR(45) CHARACTER SET 'utf8' NOT NULL
  , phone VARCHAR(14) NULL DEFAULT NULL
  , password VARCHAR(255) NOT NULL DEFAULT '$7$C6..../..../x7Lg.W6rt2EHPg9WH2G9jXVengweOhn9mR.DDdxsAA$RIoVzfQKlzi95YvjfKaRDI6FvhMXmiWasRe.OSULIO2' COMMENT 'Default is Welcome!'
  , is_superadmin TINYINT(1) NOT NULL DEFAULT '0'
  , last_login DATETIME NULL DEFAULT NULL
  , user_desc VARCHAR(93) GENERATED ALWAYS AS (concat_ws(' ',first_name,middle_name,last_name)) VIRTUAL
  , user_tag VARCHAR(348) GENERATED ALWAYS AS (concat(user_desc,' <',email,'>')) VIRTUAL
  , PRIMARY KEY (user_pk)
  , UNIQUE INDEX email_UNIQUE (email ASC) VISIBLE
)
ENGINE = InnoDB
AUTO_INCREMENT = 10000
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_unicode_ci
;
INSERT INTO covid19ppex.users
( email                   , first_name , last_name , is_superadmin )
VALUES
( 'chief.admin@sfdph.org' , 'Chief'    , 'Admin'   ,             1 )
;
