###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################
# rm(list = ls()); gc();

# setwd("~/nbaSpreads/")
package_list = c(
  "data.table",
  "foreach",
  "magrittr",
  "iterators",
  "stringr",
  "tools",

  "ggplot2",
  "pdftools",
  "gridExtra",
  "corrplot",
  "DiagrammeR",

  "randomForest",
  "xgboost",
  "caret",
  "Metrics",

  "rvest",
  "devtools"
)

# Install packages only if not already installed
for (pkg in package_list) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
}

library(devtools)
devtools::install_github("abresler/nbastatR")

# Source other functions 
source("98_get_lines.R")

# load libraries and data
# Coding
library(data.table)
library(foreach)
library(magrittr)
library(iterators)
library(stringr)
library(tools)

# Visualisation
library(ggplot2)
library(pdftools)
library(gridExtra)
library(corrplot)
library(DiagrammeR)

# Modelling
library(randomForest)
library(xgboost)
library(caret)
library(Metrics)

# Web scraping
library(rvest)
library("nbastatR") 

files <- list.files("data/")

# Pathing 
saveFile <- function(object, filePath){
  # Save files
  extension <- file_ext(filePath) %>% tolower() 
  if (extension == "csv") fwrite(object, filePath)
  else if (extension == "rds") saveRDS(object, filePath)
  else warning("incompatible filetype")
  return(invisible(NULL))
}

readFile <- function(filePath){
  # Read files 
  extension <- file_ext(filePath) %>% tolower() 
  if (extension == "csv") object <- fread(filePath)
  else if (extension == "rds") object <- readRDS(filePath)
  else warning("incompatible filetype")
  return(object)
}

archiveAndSave <- function(object, filePath) {
  # Save file and copy existing to archive
  if (file.exists(filePath)) {
    baseName        <- basename(filePath)
    archiveFilePath <- gsub(baseName, paste0("archive/", getSysTime(), "_", baseName), filePath)
    dir.create(dirname(archiveFilePath), showWarnings = FALSE, recursive = TRUE, mode = "0777")
    file.copy(filePath, archiveFilePath)
    saveFile(object, filePath)
  } else {
    saveFile(object, filePath)
  }
  return(invisible(NULL))
}

getSysTime <- function(){
  format(Sys.time(), "%Y-%m-%d_%H_%M_%S")
}

assert <- function(condition, msg) {
  # Check condition and errors if false
    if (condition == FALSE) {stop(msg)}
  return(invisible(NULL))
}

printTitle <- function(string) {
  cat("\n ############ \n ", string,
      "\n ############ \n")
  return(invisible(NULL))
}

# paths
getGameDataPath <- function() {
  "data/game_data.csv"
}

getOddsRawDataPath <- function() {
  "data/odds_raw_data.rds"
}

getTeamFactDataPath <- function() {
  "data/team_fact_data.rds"
}

getTeamMappingPath <- function() {
  "data/team_mapping.csv"
}

getSeasonDatesPath <- function() {
  "data/season_dates.csv"
}

getModelFilePath <- function() {
  "data/model_file.csv"
}

getModelFileFeaturesPath <- function() {
  "data/model_file_features.csv"
}

# Read
readSeasonDates <- function() {
  readFile(getSeasonDatesPath()) %>%
    .[, ":="(start_date = as.Date(start_date, format = "%d/%m/%Y"),
             end_date = as.Date(end_date, format = "%d/%m/%Y"))] %>% 
    return()
}

# Data cleaning
oddsToDecimal <- function(odds) {
  if (odds > 0) 1 + (odds/100)
  else 1 - (100/odds)
}

convertOdds <- function(inputVector, type = "spread"){
  # Convert Odds string to spreads or odds
  outputTable  <- 
    inputVector %>% 
    as.character() %>%
    gsub("PK", "+0 ", .) %>% 
    gsub("�", ".5", .) %>%
    str_split(string = ., pattern = "\\s") %>%
    unlist(.) %>% 
    matrix(.,ncol=2,byrow=TRUE) %>%
    as.data.table %>%
    .[, lapply(.SD, as.numeric), .SDcols = c("V1", "V2")] %>%
    setnames(c("V1", "V2"), c("spread", "odds")) 
  return(outputTable[, ..type])
}

convertDateToString <- function(date){
  date %>% as.character() %>% gsub("-", "", .)
}

plotMeanError <- function(dt, column) {
  dtCopy <- copy(dt)[, input := get(column)]
  errorTable <- 
    dtCopy[, .(.N, error = calcError(prediction, response)), 
         keyby = .(holdout_flag, input)]
  errorTable %>% 
    ggplot() + 
    geom_point(aes(x = input, y = error, group = as.factor(holdout_flag), col = as.factor(holdout_flag))) + 
    geom_line(aes(x = input, y = error, group = as.factor(holdout_flag), col = as.factor(holdout_flag))) + 
    xlab(column) + 
    ggtitle(column) + 
    ylim(pmin(0.4, min(errorTable$error)), 
         pmax(0.6, max(errorTable$error))) %>%
    return()
}

# ETL Functions
createSchemaTable <- function(schema) {
  table <- matrix(ncol = length(schema),
                  nrow = 0) %>%
    as.data.table()
  setnames(table, old = names(table), new = schema)
  return(table)
}

getGamesSchema <- function(){
  return(c("game_start_time",   "visitor_team_name", "visitor_pts"   ,   
           "home_team_name" ,   "home_pts"         , "box_score_text",   
           "overtimes"      ,   "attendance"       , "game_remarks"  ,   
           "dates"          ,   "gameId"           , "url"           ,   
           "yr"             ,   "month"              ))
}

getOddsSchema <- function() {
  return(c("date",
           "sport",
           "bet_type",
           "period",
           "away_Team",
           "home_Team",
           "away_1Q",
           "away_2Q",
           "away_3Q",
           "away_4Q",
           "home_1Q",
           "home_2Q",
           "home_3Q",
           "home_4Q",
           "away_score",
           "home_score",
           "away_open",
           "home_open",
           "pinnacle1",
           "pinnacle2",
           "fiveDimes1",
           "fiveDimes2",
           "bookmaker1",
           "bookmaker2",
           "BOL1",
           "BOL2",
           "bovada1",
           "bovada2",
           "heritage1",
           "heritage2",
           "intertops1",
           "intertops2",
           "youwager1",
           "youwager2",
           "justbet1",
           "justbet2",
           "sportsbet1",
           "sportsbet2",
           "oddsURL"))
}

getTeamFactSchema <- function() {
  return(c("nameTeam"
           # ,"rangeDateFrom" # future
           # ,"rangeDateTo"
           ,"isPlusMinus"
           ,"isPaceAdjust"
           ,"isRank"
           ,"idPlayoffRound"
           ,"idMonth"
           ,"idPeriod"
           ,"typeMeasure"
           ,"idTeamOpponent"
           ,"countLastNGames"
           ,"idTeam"
           ,"gp"
           ,"pctWins"
           ,"fgm"
           ,"fga"
           ,"pctFG"
           ,"fg3m"
           ,"fg3a"
           ,"pctFG3"
           ,"pctFT"
           ,"gpRank"
           ,"pctWinsRank"
           ,"minutesRank"
           ,"fgmRank"
           ,"fgaRank"
           ,"pctFGRank"
           ,"fg3mRank"
           ,"fg3aRank"
           ,"pctFG3Rank"
           ,"pctFTRank"
           ,"fg2m"
           ,"fg2a"
           ,"pctFG2"
           ,"wins"
           ,"losses"
           ,"minutes"
           ,"ftm"
           ,"fta"
           ,"oreb"
           ,"dreb"
           ,"treb"
           ,"ast"
           ,"tov"
           ,"stl"
           ,"blk"
           ,"blka"
           ,"pf"
           ,"pfd"
           ,"pts"
           ,"plusminus"
           ,"winsRank"
           ,"lossesRank"
           ,"rankFTM"
           ,"rankFTA"
           ,"orebRank"
           ,"drebRank"
           ,"trebRank"
           ,"astRank"
           ,"tovRank"
           ,"stlRank"
           ,"blkRank"
           ,"blkaRank"
           ,"pfRank"
           ,"pfdRank"
           ,"ptsRank"
           ,"plusminusRank"
           ,"pctEFG"
           ,"pctTOVTeam"
           ,"pctOREB"
           ,"pctEFGOpponent"
           ,"pctTOVOpponent"
           ,"pctOREBOpponent"
           ,"pctEFGRank"
           ,"pctTOVTmRank"
           ,"pctOREBRank"
           ,"pctEFGOpponentRank"
           ,"pctTOVOpponentRank"
           ,"pctOREBOpponentRank"
           ,"rateFTA"
           ,"rateFTAOpponent"
           ,"rateFTARank"
           ,"rateFTAOpponentRank"
           ,"pctAST"
           ,"pctDREB"
           ,"pctTREB"
           ,"pctTS"
           ,"pctASTRank"
           ,"pctDREBRank"
           ,"pctTREBRank"
           ,"pctTSRank"
           ,"ortgE"
           ,"ortg"
           ,"drtgE"
           ,"drtg"
           ,"netrtgE"
           ,"netrtg"
           ,"ratioASTtoTO"
           ,"ratioAST"
           ,"paceE"
           ,"pace"
           ,"pacePer40PACE_PER40"
           ,"possessions"
           ,"ratioPIE"
           ,"ortgRank"
           ,"drtgRank"
           ,"netrtgRank"
           ,"ratioASTtoTORank"
           ,"ratioASTRank"
           ,"paceRank"
           ,"pieRank"
           ,"ptsOffTOVOpponent"
           ,"ptsSecondChanceOpponent"
           ,"ptsFastBreakOpponent"
           ,"ptsPaintOpponent"
           ,"potsOffTOVOpponentRank"
           ,"ptsSecondChanceOpponentRank"
           ,"ptsFastBreakOpponentRank"
           ,"ptsPaintOpponentRank"
           ,"ptsOffTOV"
           ,"ptsSecondChance"
           ,"ptsFastBreak"
           ,"ptsPaint"
           ,"ptsOffTOVRank"
           ,"ptsSecondChanceRank"
           ,"ptsFastBreakRank"
           ,"ptsPaintRank"
           ,"pctFGAasFG2"
           ,"pctFGAasFG3"
           ,"pctPTSasFG2"
           ,"pctPTSasFG2asMR"
           ,"pctsPTSasFG3"
           ,"pctPTSasFB"
           ,"pctPTSasFT"
           ,"pctPTSasOffTOV"
           ,"pctPTSasPaint"
           ,"pctFG2MasAssisted"
           ,"pctFG2MasUnassisted"
           ,"pctFG3MasAssisted"
           ,"pctFG3MasUnassisted"
           ,"pctFGMasAssisted"
           ,"pctFGMasUnassisted"
           ,"pctFGAasFG2Rank"
           ,"pctFGAasFG3Rank"
           ,"pctPTSasFG2Rank"
           ,"pctPTSasFG2asMRRank"
           ,"pctsPTSasFG3Rank"
           ,"pctPTSasFBRank"
           ,"pctPTSasFTRank"
           ,"pctPTSasOffTOVRank"
           ,"pctPTSasPaintRank"
           ,"pctFG2MasAssistedRank"
           ,"pctFG2MasUnassistedRank"
           ,"pctFG3MasAssistedRank"
           ,"pctFG3MasUnassistedRank"
           ,"pctFGMasAssistedRank"
           ,"pctFGMasUnassistedRank"
  ))
}

# Modelling 
dataSummary <- function(dt){
  dt[, .(column     = names(dt),
         num_zeros  = sapply(.SD, function(x) {sum(x == 0, na.rm = TRUE)}),
         prop_zeros = round(100 * sapply(.SD, function(x) {sum(x == 0, na.rm = TRUE) / .N}), 2),
         num_NA     = sapply(.SD, function(x) {sum(is.na(x))}),
         prop_zeros = round(100 * sapply(.SD, function(x) {sum(is.na(x)) / .N}), 2),
         type       = sapply(.SD, class),
         unique     = sapply(.SD, function(x) {uniqueN(x, na.rm = TRUE)})
  )]
}

# Split modelFile into train, test, holdout
splitTrainTestHoldout <- function(inputModelFile) {
  set.seed(123)
  validationRows <- 100
  output <- 
    inputModelFile[, holdout_flag := 0
                   ][sample(.N, round(.N * 0.3, 0)), holdout_flag := 1
                     ][season == 2020, holdout_flag := -1
                       ][season != 2021]
  return(output)
}
getCM <- function(dt) {
  confusionMatrix(dt[, prediction > 0.5] %>% as.factor,
                  dt[, response] %>% as.factor,
                  positive = "TRUE")
}

getCMprecision <- function(dt){
  cm <- getCM(dt)
  return(cm$byClass["Precision"])
}

calcError <- function(preds, resp){
  mean(as.numeric(preds > 0.5) != resp, na.rm = TRUE)
}