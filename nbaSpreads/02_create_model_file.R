###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################
rm(list = ls()); gc();

setwd("~/kaggle_projects/kaggle/nbaSpreads/")
source("99_functions.R")

# To do
# Pull in last 2 seasons worth of data
# Build simple pipeline with 1-2 features

## Create base table (from game data)
## Append odds + lines
## Create one feature
## build model and assessment

loadAndCleanBaseGamesTable <- function() {
  games       <- readFile(getGameDataPath())  
  teamMapping <- readFile(getTeamMappingPath())
  
  games[, date := as.Date(as.character(dates), format = "%a, %b %d, %Y")]
  gamesLongTable <- 
    rbind(copy(games)[, ":=" (target_team  = home_team_name,
                              target_score = home_pts,
                              oppo_team    = visitor_team_name,
                              oppo_score   = visitor_pts,
                              location     = "home")],
          copy(games)[, ":=" (target_team  = visitor_team_name,
                              target_score = visitor_pts,
                              oppo_team    = home_team_name,
                              oppo_score   = home_pts,
                              location     = "away")])
  
  baseTable <- gamesLongTable %>% 
    merge(teamMapping[, .(teamString, target_team_name = teamName, target_team_nbr = teamNum)],
          by.x = "target_team",
          by.y = "teamString",
          all.x = TRUE) %>%
    merge(teamMapping[, .(teamString, oppo_team_name = teamName, oppo_team_nbr = teamNum)],
          by.x = "oppo_team",
          by.y = "teamString",
          all.x = TRUE)
  
  assert(condition = baseTable[is.na(target_team_name) | is.na(oppo_team_name), .N] == 0, 
         msg       = "Missing team names")
  
  return(
    baseTable[, .(season_year = yr, date, target_team_name, target_team_nbr, target_score, oppo_team_name, oppo_team_nbr, oppo_score, location, gameId, yr, month)]
  )
}

loadAndCleanOdds <- function() {
  
  oddsRaw     <- readFile(getOddsRawDataPath())
  teamMapping <- readFile(getTeamMappingPath())
  
  oddsRaw[, date_source := date]
  oddsRaw[, date := as.Date(as.character(date_source), format = "%Y%m%d")]
  
  cols <- c("away_score", "home_score")
  oddsRaw[, (cols) := lapply(.SD, function(x){x %>% as.character() %>% as.numeric()}), .SDcols = cols]
  
  oddsLongTable <- 
    rbind(copy(oddsRaw)[, ":="(targetTeam = home_Team,
                               targetOpen = home_open,
                               targetScore = home_score,
                               targetDiff = home_score - away_score,
                               location = "home")],
          copy(oddsRaw)[, ":="(targetTeam = away_Team,
                               targetOpen = away_open,
                               targetScore = away_score,
                               targetDiff = away_score - home_score,
                               location = "away")])
  
  removeGames <- c("Team Giannis", "Team LeBron ", "Team USA", "Team World",
                   "East", "West", "Team LeBron", "Team Stephen", 
                   "USA All Stars", "World All Stars")
  
  oddsLongTableClean <- oddsLongTable %>%
    .[targetOpen != ""] %>%
    .[!(targetTeam %in% removeGames)] %>%
    .[, targetSpread := convertOdds(targetOpen, "spread")] %>%
    .[, covered := (targetDiff + targetSpread) > 0] %>% 
    .[, won := targetDiff > 0]
  
  oddsTable <- oddsLongTableClean %>%
    merge(teamMapping[, .(teamString, target_team_name = teamName, target_team_nbr = teamNum)],
          by.x = "targetTeam",
          by.y = "teamString",
          all.x = TRUE)
  
  assert(condition = oddsTable[is.na(target_team_name), .N] == 0, 
         msg       = paste0("Missing team names: ", 
                            paste0(oddsTable[is.na(target_team_name), unique(targetTeam)],
                            collapse = ", ")))
  return(
    oddsTable[, .(season_year, date, target_team_name, target_team_nbr, targetScore, targetDiff, targetSpread, covered, won, location)]
  )
}

filterModelFileToSeason <- function(modelFileDt) {
  dates     <- readSeasonDates() 
  modelFileDt[, date_copy := date]
  setkey(modelFileDt, date, date_copy)
  setkey(dates, start_date, end_date)
  modelFileDtCopy <- 
    foverlaps(modelFileDt, 
              dates, 
              by.x = c("date", "date_copy"), 
              by.y = c("start_date", "end_date")) %>% 
    .[!is.na(season)] %>%
    .[, date_copy := NULL]
  return(modelFileDtCopy[, .SD, .SDcols = names(modelFileDtCopy)])
}

baseTable <- loadAndCleanBaseGamesTable()
oddsTable <- loadAndCleanOdds()

modelFile <- 
  merge(baseTable,
        oddsTable,
        by = c("date", "target_team_nbr", "target_team_name", "location", "season_year"),
        all.x = TRUE) %>% 
  filterModelFileToSeason() %>% 
  .[, !c("start_date", "end_date", "season_year", "yr")]

modelFile %>% archiveAndSave(getModelFilePath())

if (FALSE) {
  modelFile <- readFile(getModelFilePath())
  
  modelFile[, sum(is.na(targetScore)) / .N, keyby = date] %>% 
    ggplot() + geom_line(aes(x = date, y = V1))
  
   filterModelFileToSeason(modelFile) %>%
    .[, sum(is.na(targetScore)) / .N, keyby = date] %>% 
    ggplot() + geom_line(aes(x = date, y = V1))
  
  modelFile[, .N, by = is.na(targetScore)]
  modelFile %>% summary
  
  modelFile[, .(mean(covered), .N), by = won]                 # the team that wins covers the spread more often
  modelFile[!is.na(won), .(mean(covered), .N), by = target_team_name][order(V1)] # some teams are better at covering the spread
  modelFile[!is.na(won), .(mean(covered), .N), by = location][order(V1)] # away team covers slightly more 
  modelFile[!is.na(won), .(mean(covered), .N), by = targetSpread] %>% 
    ggplot() +  
    geom_col(aes(x = targetSpread, y = N / sum(N))) +         # ~ 0-5 underdogs cover most, 0-5 favourites lose most
    geom_point(aes(x = targetSpread, y = V1))             
}