install.packages("RMySQL")
install.packages("RODBC")
library(RMySQL)
library(RODBC)

#Establishing the Connection to the Database
databaseConnection <- dbConnect(MySQL(),user = 'root', password = 'Dannysboy092016!', host = 'localhost')

#Creating the Schema in mySQL Workbench
queryCreateDatabase <- "CREATE DATABASE jeopardy"
results <- dbSendQuery(databaseConnection, queryCreateDatabase)
dbClearResult(results)

#Establishing connection to the jeopardy database
databaseConnection <- dbConnect(MySQL(),user = 'root', password = 'Dannysboy092016!', host = 'localhost', dbname = 'jeopardy')

#Creating the Tables based off the csv files
#Airdates Table
queryCreateTableAirdates <- "CREATE TABLE airdates(
  gameid INT,
  showid INT,
  air_date DATE)"

resultsAirdates <- dbSendQuery(databaseConnection, queryCreateTableAirdates)
dbClearResult(resultsAirdates)

dbSendQuery( databaseConnection, "LOAD DATA LOCAL INFILE '/path/to/airdates.csv'
                                              INTO TABLE episode
                                              FIELDS TERMINATED by ','
                                              ENCLOSED BY '"'
                                              LINES TERMINATED BY '\\n'")
