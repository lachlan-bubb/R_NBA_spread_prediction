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
# install.packages("devtools")
# devtools::install_github("abresler/nbastatR")

mergeReduceTeamFact <- function(dt1, dt2){
  mergeCols <- c("nameTeam", 
                 # "rangeDateFrom",
                 # "rangeDateTo",
                 "isPlusMinus",
                 "isPaceAdjust",
                 "isRank",
                 "idPlayoffRound",
                 "idMonth",
                 "idPeriod")
  addCols <- c(mergeCols, setdiff(names(dt2), names(dt1))) %>% unique
  merge(as.data.table(dt1), 
        as.data.table(dt2)[, ..addCols], 
        by = mergeCols,
        all.x = TRUE)
}

# startDate <- as.Date("01/01/2021", format = "%d/%m/%Y")
# endDate   <- as.Date("10/01/2021", format = "%d/%m/%Y")
season_dates  <- readSeasonDates()
season_months <- CJ(season = season_dates$season, month = 1:12)

teamFact <-
  foreach (row = iter(season_months, by = "row"), .combine = rbind) %do% {
    
    season <- row$season %T>% print
    month  <- row$month  %T>% print
    
    dt <- tryCatch(
      {teams_players_stats(seasons   = season,
                           months    = month,       # Simple implementation
                           # date_from = startDate, # future use case
                           # date_to   = endDate,
                           type      = "team",
                           tables    = "general",
                           measures  = c("Base", "Four Factors", "Advanced",
                                         "Defense", "Misc", "Scoring", "Usage"), 
                           modes = c("PerGame"))},
      error = function(cond){return(NA)})
    
    if (nrow(dt) == 0) dt <- NA
    if (is.na(dt)) {
      combinedTables <- createSchemaTable(getTeamFactSchema())
    } else {
      combinedTables <- Reduce(mergeReduceTeamFact, dt$dataTable)
    }
    
    combinedTables[, season := season]
    combinedTables[, month  := month]
    
    MonthStartDates <- 
      as.Date(paste0(season, "-", 
                     ((month + 8) %% 12) + 1,  "-01"))
    MonthEndDates <-
      as.Date(paste0(season, "-", 
                     ((month + 9) %% 12 + 1),  "-01")) - 1
    combinedTables[, ":="(month_start_dates = MonthStartDates,
                          month_end_dates   = MonthEndDates)]
    
    combinedTables
}

teamFact %>% archiveAndSave(getTeamFactDataPath())

# Data checks
if (FALSE) {
  data2 <- readFile(getTeamFactDataPath())
  names(data2)
  data2[, .N, keyby = .(season, month, month_start_dates, month_end_dates)]
  data2[,.N, keyby = .(season, month, nameTeam)][order(N)]
  data2[,.N, keyby = .(season, month)]
  data2[, .(.N, uniqueN(month)), keyby = .(season)]
  data2[, .(.N, uniqueN(season)), keyby = .(month)]
  # No 8,9,or 12 month
  # Season 2020 has added 10,11 but no 7 
  # Season 2021 starts on 3, has 3,4,5
  data2[season == 2018 & month == 1, unique(nameTeam)]
}







