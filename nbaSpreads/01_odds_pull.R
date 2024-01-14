###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################
rm(list = ls()); gc();

setwd("~/kaggle_projects/kaggle/nbaSpreads/")
source("99_functions.R")

# Test
# startDate <- as.Date("2020-12-22", format = "%Y-%m-%d")
# endDate <- as.Date("2021-01-10", format = "%Y-%m-%d")

# 2018-2019 regular season 
   # Due to some games missing, (1-5 per team per season)
   # Need to left join to games played to find 
season_dates <- readSeasonDates()

seasonData <-
  foreach (row = iter(season_dates, by = "row"), .combine = rbind) %do% {

  # startDate <- as.Date("2018-10-16", format = "%Y-%m-%d")
  # endDate <- as.Date("2019-04-10", format = "%Y-%m-%d")
  startDate <- row$start_date
  endDate   <- row$end_date
  season    <- row$season
    
  diff <- as.integer(endDate - startDate)
  days <- startDate + seq(0, diff)
  
  data <- 
    foreach(day = days, .combine = rbind) %do% {
      
      dt <- tryCatch(
        {get_lines(sport      = "NBA", 
                   bet_type   = "spread", 
                   period     = "full", 
                   start_date = convertDateToString(day))},
        error = function(cond){return(NA)})
      
      if (is.na(dt)) {
        table <- createSchemaTable(getOddsSchema())
        } else {
        table <- dt
        }
      
     table
    } %>% as.data.table
  
  data[, season_year := season]
  data
  
}
seasonData %>% archiveAndSave(getOddsRawDataPath())

# Data checks
if (FALSE) {
  data2 <- readFile(getOddsRawDataPath())
  # Will only pull complete days with odds (not good for future looking)
  names(data2)
  data2[, .N, keyby = oddsURL]
  data2[, .N, keyby = home_Team]
  data2[, .N, keyby = away_Team]
  data2[, .N, keyby = season_year]
  # LA is clippers?
  data[, !c("oddsURL")]
}

      





