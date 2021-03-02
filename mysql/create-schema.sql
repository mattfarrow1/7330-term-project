-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------
-- -----------------------------------------------------
-- Schema jeopardy
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema jeopardy
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `jeopardy` DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci ;
USE `jeopardy` ;

-- -----------------------------------------------------
-- Table `jeopardy`.`episode`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `jeopardy`.`episode` (
  `gameid` INT NOT NULL,
  `showid` INT NOT NULL,
  `airdate` DATE NULL DEFAULT NULL,
  PRIMARY KEY (`gameid`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `jeopardy`.`board`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `jeopardy`.`board` (
  `clueid` INT NOT NULL AUTO_INCREMENT,
  `roundnum` INT NOT NULL,
  `chosen` INT NULL DEFAULT NULL,
  `category` VARCHAR(250) NULL DEFAULT NULL,
  `clue` VARCHAR(2000) NULL DEFAULT NULL,
  `answer` VARCHAR(1000) NULL DEFAULT NULL,
  `score` INT NULL DEFAULT NULL,
  `doublejeop` TINYINT(1) NULL DEFAULT NULL,
  `episode_gameid` INT NOT NULL,
  PRIMARY KEY (`clueid`),
  INDEX `fk_board_episode1_idx` (`episode_gameid` ASC) VISIBLE,
  CONSTRAINT `fk_board_episode1`
    FOREIGN KEY (`episode_gameid`)
    REFERENCES `jeopardy`.`episode` (`gameid`))
ENGINE = InnoDB
AUTO_INCREMENT = 402161
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `jeopardy`.`location`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `jeopardy`.`location` (
  `locationID` INT NOT NULL AUTO_INCREMENT,
  `colcat` INT NOT NULL,
  `rowcat` INT NOT NULL,
  `board_clueid` INT NOT NULL,
  PRIMARY KEY (`locationID`),
  INDEX `fk_location_board1_idx` (`board_clueid` ASC) VISIBLE,
  CONSTRAINT `fk_location_board1`
    FOREIGN KEY (`board_clueid`)
    REFERENCES `jeopardy`.`board` (`clueid`))
ENGINE = InnoDB
AUTO_INCREMENT = 402003
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `jeopardy`.`players`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `jeopardy`.`players` (
  `playerid` INT NOT NULL AUTO_INCREMENT,
  `firstname` VARCHAR(20) NULL DEFAULT NULL,
  `lastname` VARCHAR(25) NULL DEFAULT NULL,
  `occupation` VARCHAR(250) NULL DEFAULT NULL,
  `location` VARCHAR(250) NULL DEFAULT NULL,
  PRIMARY KEY (`playerid`))
ENGINE = InnoDB
AUTO_INCREMENT = 13831
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `jeopardy`.`players_has_episode`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `jeopardy`.`players_has_episode` (
  `players_playerid` INT NOT NULL,
  `episode_gameid` INT NOT NULL,
  PRIMARY KEY (`players_playerid`, `episode_gameid`),
  INDEX `fk_players_has_episode_episode1_idx` (`episode_gameid` ASC) VISIBLE,
  INDEX `fk_players_has_episode_players1_idx` (`players_playerid` ASC) VISIBLE,
  CONSTRAINT `fk_players_has_episode_episode1`
    FOREIGN KEY (`episode_gameid`)
    REFERENCES `jeopardy`.`episode` (`gameid`),
  CONSTRAINT `fk_players_has_episode_players1`
    FOREIGN KEY (`players_playerid`)
    REFERENCES `jeopardy`.`players` (`playerid`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `jeopardy`.`synopsis`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `jeopardy`.`synopsis` (
  `episode_gameid` INT NOT NULL,
  `finalscoreid` INT NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(45) NOT NULL,
  `finalscore` INT NULL DEFAULT NULL,
  `coryat` INT NULL DEFAULT NULL,
  `ansRight` INT NOT NULL,
  `ansWrong` INT NOT NULL,
  PRIMARY KEY (`finalscoreid`),
  INDEX `fk_synopsis_episode_idx` (`episode_gameid` ASC) VISIBLE,
  CONSTRAINT `fk_synopsis_episode`
    FOREIGN KEY (`episode_gameid`)
    REFERENCES `jeopardy`.`episode` (`gameid`))
ENGINE = InnoDB
AUTO_INCREMENT = 70420
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `jeopardy`.`synopsis_has_players`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `jeopardy`.`synopsis_has_players` (
  `synopsis_finalscoreid` INT NOT NULL,
  `players_playerid` INT NOT NULL,
  PRIMARY KEY (`synopsis_finalscoreid`, `players_playerid`),
  INDEX `fk_synopsis_has_players_players1_idx` (`players_playerid` ASC) VISIBLE,
  INDEX `fk_synopsis_has_players_synopsis1_idx` (`synopsis_finalscoreid` ASC) VISIBLE,
  CONSTRAINT `fk_synopsis_has_players_players1`
    FOREIGN KEY (`players_playerid`)
    REFERENCES `jeopardy`.`players` (`playerid`),
  CONSTRAINT `fk_synopsis_has_players_synopsis1`
    FOREIGN KEY (`synopsis_finalscoreid`)
    REFERENCES `jeopardy`.`synopsis` (`finalscoreid`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
