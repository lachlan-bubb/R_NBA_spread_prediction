###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################
rm(list = ls()); gc();

setwd("~/kaggle_projects/kaggle/nbaSpreads/")
source("99_functions.R")

modelFile <- readFile(getModelFilePath())

##############
# Last X games results features 
##############
# Create ordering of dates / games
lags <- 1:3
createTeamLag <- function(lag, dt, col, keyCol, keyName) {
  setkeyv(dt, c(keyCol, "date"))
  dt[, paste0(keyName, "_", col, "_lag", lag) := shift(get(col), type = "lag", n = lag), by = c(keyCol, "season")]
  return(invisible(NULL))
}
# Create lag wins, covered, spreads and diffs for home team
for (metric in c("won", "covered", "targetSpread", "targetDiff", "location")) {
  lapply(lags, 
         createTeamLag, 
         modelFile,
         metric,
         "target_team_nbr",
         "target")
  
  lapply(lags, 
         createTeamLag, 
         modelFile,
         metric,
         "oppo_team_nbr",
         "oppo")
}

modelFile[, target_sum_covered  := target_covered_lag1 + target_covered_lag2 + target_covered_lag3]
modelFile[, oppo_sum_covered    := oppo_covered_lag1 + oppo_covered_lag2 + oppo_covered_lag3]
modelFile[, target_sum_won      := target_won_lag1 + target_won_lag2 + target_won_lag3]
modelFile[, oppo_sum_won        := oppo_won_lag1 + oppo_won_lag2 + oppo_won_lag3]
modelFile[, target_sum_location := ((target_location_lag1 == "home") + (target_location_lag2 == "home") + (target_location_lag3 == "home"))]
modelFile[, oppo_sum_location   := ((oppo_location_lag1 == "home") + (oppo_location_lag2 == "home") + (oppo_location_lag3 == "home"))]
modelFile[, home := ifelse(location == "home", 1, 0)]

##############
# Team Fact features 
##############
# modelFile <- readFile(getModelFilePath())

teamFact    <- getTeamFactDataPath() %>% readFile
teamMapping <- readFile(getTeamMappingPath())
teamFact <- 
  teamFact %>%
  merge(teamMapping[, .(teamString, target_team_name = teamName, target_team_nbr = teamNum)],
        by.x = "nameTeam",
        by.y = "teamString",
        all.x = TRUE)

assert(condition = teamFact[is.na(target_team_name), .N] == 0, 
       msg       = paste0("Missing team names: ", 
                          paste0(teamFact[is.na(target_team_name), unique(targetTeam)],
                                 collapse = ", ")))

season_month_id <- 
  teamFact[, .(.N), keyby = .(month_start_dates, month_end_dates)
           ][, N := NULL
             ][, season_month_rnk := frank(month_start_dates)]

season_month_id[, endDateCopy := month_end_dates + 1]
modelFile[, dateCopy := date]

setkey(season_month_id, endDateCopy)
setkey(modelFile, dateCopy)  
modelFile <- 
  season_month_id[modelFile, roll = Inf
                  ][, !c("endDateCopy")]

teamFact <- teamFact %>% 
  merge(season_month_id[, .(month_start_dates, month_end_dates, season_month_rnk)],
        by = c("month_start_dates", "month_end_dates"))

modelFile <- 
  modelFile %>% 
  merge(teamFact[, !c("month", "target_team_name", "season")],
        by = c("month_start_dates", "month_end_dates", "season_month_rnk", "target_team_nbr"),
        all.x = TRUE) %>%
  merge(teamFact[, !c("month", "target_team_name", "season")],
        by.x = c("month_start_dates", "month_end_dates", "season_month_rnk", "oppo_team_nbr"),
        by.y = c("month_start_dates", "month_end_dates", "season_month_rnk", "target_team_nbr"),
        all.x = TRUE,
        suffixes = c("_target", "_oppo"))

# modelFile[, .N, by = .(month_start_dates, month_end_dates, target_team_name)]
# names(modelFile)

##############
# Last games in last X days features 
##############
teamGames <- modelFile[, .(team = target_team_name, date)] %>% unique
teamGames[, joiner := 1]
crossTeamGames <-
  merge(teamGames,
        teamGames,
        by = c("team", "joiner"),
        allow.cartesian = TRUE)

daysLag <- 5
gamesInLastDays <- 
  crossTeamGames[date.y %between% list(date.x - daysLag, date.x - 1)
                 ][, .(gameInLastdays = .N), keyby = .(team, date = date.x)
                   ] 
modelFile <- 
  modelFile %>%
  merge(gamesInLastDays[, .(target_team_name = team, date, target_gamesInLastDays = gameInLastdays)], 
        by = c("target_team_name", "date"),
        all.x = TRUE)  %>%
  merge(gamesInLastDays[, .(oppo_team_name = team, date, oppo_gamesInLastDays = gameInLastdays)], 
        by = c("oppo_team_name", "date"),
        all.x = TRUE) 

modelFile[is.na(target_gamesInLastDays), target_gamesInLastDays := 0]
modelFile[is.na(oppo_gamesInLastDays), oppo_gamesInLastDays := 0]

# modelFile[, .(mean(covered, na.rm = TRUE), .N), by = .(target = gameInLast_days)] %>% 
#   ggplot() +  
#   geom_col(aes(x = target, y = N / sum(N))) +     
#   geom_point(aes(x = target, y = V1))   

modelFile %>% archiveAndSave(getModelFileFeaturesPath())


if (FALSE) {
  
  modelFile[game_rank == 1]
  modelFile[, sum(covered), by = game_rank][,.N, by = V1]
  
  modelFile[, check := sum(covered), by = game_rank]
  modelFile[check != 1] # Ties can happen which give money back
}
