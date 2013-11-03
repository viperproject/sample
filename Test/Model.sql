SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

CREATE SCHEMA IF NOT EXISTS `mydb` DEFAULT CHARACTER SET latin1 ;
USE `mydb` ;

-- -----------------------------------------------------
-- Table `mydb`.`Programs`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`Programs` (
  `ProgramId` INT(11) NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NULL DEFAULT NULL ,
  `LOC` INT(11) NULL DEFAULT NULL ,
  PRIMARY KEY (`ProgramId`) )
ENGINE = InnoDB
AUTO_INCREMENT = 50503
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`TestRun`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`TestRun` (
  `idTestRun` INT(11) NOT NULL AUTO_INCREMENT ,
  `Analysis` VARCHAR(45) NULL DEFAULT NULL ,
  `HeapAnalysis` VARCHAR(45) NULL DEFAULT NULL ,
  `Compiler` VARCHAR(45) NULL DEFAULT NULL ,
  `Property` VARCHAR(45) NULL DEFAULT NULL ,
  `Iterator` VARCHAR(45) NULL DEFAULT NULL ,
  `Date` VARCHAR(45) NULL DEFAULT NULL ,
  PRIMARY KEY (`idTestRun`) )
ENGINE = InnoDB
AUTO_INCREMENT = 108
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`Analyses`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`Analyses` (
  `TestRun` INT(11) NOT NULL ,
  `Program` INT(11) NOT NULL ,
  `CompilerTime` DOUBLE NULL DEFAULT NULL ,
  `AnalysisTime` DOUBLE NULL DEFAULT NULL ,
  `PropertyTime` DOUBLE NULL DEFAULT NULL ,
  `Warnings` INT(11) NULL DEFAULT NULL ,
  `Validated` INT(11) NULL DEFAULT NULL ,
  PRIMARY KEY (`TestRun`, `Program`) ,
  INDEX `fk_Analyses_1` (`Program` ASC) ,
  INDEX `fk_Analyses_2` (`TestRun` ASC) ,
  CONSTRAINT `fk_Analyses_1`
    FOREIGN KEY (`Program` )
    REFERENCES `mydb`.`Programs` (`ProgramId` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Analyses_2`
    FOREIGN KEY (`TestRun` )
    REFERENCES `mydb`.`TestRun` (`idTestRun` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`BrokenAnalyses`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`BrokenAnalyses` (
  `TestRun` INT(11) NOT NULL ,
  `Program` INT(11) NOT NULL ,
  `Error` LONGTEXT NULL DEFAULT NULL ,
  PRIMARY KEY (`TestRun`, `Program`) ,
  INDEX `fk_BrokenAnalyses_1` (`Program` ASC) ,
  INDEX `fk_BrokenAnalyses_2` (`TestRun` ASC) ,
  CONSTRAINT `fk_BrokenAnalyses_1`
    FOREIGN KEY (`Program` )
    REFERENCES `mydb`.`Programs` (`ProgramId` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_BrokenAnalyses_2`
    FOREIGN KEY (`TestRun` )
    REFERENCES `mydb`.`TestRun` (`idTestRun` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`BrokenCompilations`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`BrokenCompilations` (
  `TestRun` INT(11) NOT NULL ,
  `Program` INT(11) NOT NULL ,
  `Error` LONGTEXT NULL DEFAULT NULL ,
  PRIMARY KEY (`TestRun`, `Program`) ,
  INDEX `fk_BrokenCompilations_1` (`TestRun` ASC) ,
  INDEX `fk_BrokenCompilations_2` (`Program` ASC) ,
  CONSTRAINT `fk_BrokenCompilations_1`
    FOREIGN KEY (`TestRun` )
    REFERENCES `mydb`.`TestRun` (`idTestRun` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_BrokenCompilations_2`
    FOREIGN KEY (`Program` )
    REFERENCES `mydb`.`Programs` (`ProgramId` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`Output`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`Output` (
  `Id` INT(11) NOT NULL AUTO_INCREMENT ,
  `TestRun` INT(11) NULL DEFAULT NULL ,
  `Program` INT(11) NULL DEFAULT NULL ,
  `ProgramPoint` VARCHAR(256) NULL DEFAULT NULL ,
  `Message` TEXT NULL DEFAULT NULL ,
  PRIMARY KEY (`Id`) ,
  INDEX `fk_Output_1` (`TestRun` ASC) ,
  INDEX `fk_Output_2` (`Program` ASC) ,
  CONSTRAINT `fk_Output_1`
    FOREIGN KEY (`TestRun` )
    REFERENCES `mydb`.`TestRun` (`idTestRun` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Output_2`
    FOREIGN KEY (`Program` )
    REFERENCES `mydb`.`Programs` (`ProgramId` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
AUTO_INCREMENT = 1136461
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`ProgramTags`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`ProgramTags` (
  `Name` VARCHAR(128) NOT NULL ,
  `Tag` VARCHAR(45) NOT NULL ,
  PRIMARY KEY (`Name`, `Tag`) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`RuntimeErrors`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`RuntimeErrors` (
  `TestRun` INT(11) NOT NULL ,
  `Program` INT(11) NOT NULL ,
  PRIMARY KEY (`Program`, `TestRun`) ,
  INDEX `fk_RuntimeErrors_1` (`TestRun` ASC) ,
  INDEX `fk_RuntimeErrors_2` (`Program` ASC) ,
  CONSTRAINT `fk_RuntimeErrors_1`
    FOREIGN KEY (`TestRun` )
    REFERENCES `mydb`.`TestRun` (`idTestRun` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_RuntimeErrors_2`
    FOREIGN KEY (`Program` )
    REFERENCES `mydb`.`Programs` (`ProgramId` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`TestRunAnalysisParameters`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`TestRunAnalysisParameters` (
  `TestRun` INT(11) NOT NULL ,
  `Name` VARCHAR(45) NOT NULL ,
  `Value` VARCHAR(45) NULL DEFAULT NULL ,
  PRIMARY KEY (`TestRun`, `Name`) ,
  INDEX `fk_TestRunAnalysisParameters_1` (`TestRun` ASC) ,
  CONSTRAINT `fk_TestRunAnalysisParameters_1`
    FOREIGN KEY (`TestRun` )
    REFERENCES `mydb`.`TestRun` (`idTestRun` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`TestRunHeapAnalysisParameters`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`TestRunHeapAnalysisParameters` (
  `TestRun` INT(11) NOT NULL ,
  `Name` VARCHAR(45) NOT NULL ,
  `Value` VARCHAR(45) NULL DEFAULT NULL ,
  PRIMARY KEY (`TestRun`, `Name`) ,
  INDEX `fk_TestRunHeapAnalysisParameters_1` (`TestRun` ASC) ,
  CONSTRAINT `fk_TestRunHeapAnalysisParameters_1`
    FOREIGN KEY (`TestRun` )
    REFERENCES `mydb`.`TestRun` (`idTestRun` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `mydb`.`ToBeAnalyzed`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `mydb`.`ToBeAnalyzed` (
  `TestRun` INT(11) NOT NULL ,
  `Program` INT(11) NOT NULL ,
  PRIMARY KEY (`Program`, `TestRun`) ,
  INDEX `fk_ToBeAnalyzed_1` (`TestRun` ASC) ,
  INDEX `fk_ToBeAnalyzed_2` (`Program` ASC) ,
  CONSTRAINT `fk_ToBeAnalyzed_1`
    FOREIGN KEY (`TestRun` )
    REFERENCES `mydb`.`TestRun` (`idTestRun` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_ToBeAnalyzed_2`
    FOREIGN KEY (`Program` )
    REFERENCES `mydb`.`Programs` (`ProgramId` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
