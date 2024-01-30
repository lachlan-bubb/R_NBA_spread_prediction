###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################

# IdeasS
# - Team stats 
#   - Need to find source, can we calculate ourselves? or take monthly score and append
# Offensive Rating: estimated number of points a team scores per 100 possessions (higher is better)
# Defensive Rating: estimated number of points a team gives up per 100 possessions (lower is better)
# Field Goal Percentage
# Field Goal Attempts
# 3-point Percentage
# 3-point Attempts
# Assist/Turnover Ratio
# Rebound Differential
# Pace: estimated number of possessions per 48 minutes
# - 4 factors to make a line (number is approx weighting)
#      Shooting percentage (10)
#           Eff FG% = (FGM + 0.5*FG3M)/FGA
#      Turnovers per possession (5-6)
#           Poss. = FGA - OR + TO + 0.4 * FTA
#           Turnovers per Poss = turnovers / Poss.
#      Offensive rebounding percentage (4-5)
#           OR% = OR / (OR + Opponents Def Reb)
#      Getting to the foul line (2-3)
#           FTA/FGA or FTM/FGA
#     Compare these metrics to league average for a year
#     Also compare on the offensive and defensive end
#     e.g. a teams shooting percentage vs. the games theyve played
# Alternative models
# - Elo (as model or feature in booster)
# - use a regression against spread

# Data sources 
# - Manuall scrape or use API?
#     - Use nbastatR API 
#     - Scrape weekly for season to date, last 4 weeks?
#     - Week of season?

rm(list = ls()); gc();

# setwd("~/nbaSpreads/")
source("99_functions.R")

# http://asbcllc.com/nbastatR/reference/teams_players_stats.html
# https://github.com/abresler/nbastatR/
  
# install.packages("devtools")
library(devtools)
# devtools::install_github("abresler/nbastatR")
library("nbastatR") 

dt <- 
  teams_tables(seasons  = 2019,
               teams = "Atlanta Hawks",
               # team_ids =  = team
               # all_active_teams = T,
               tables = "passes",
               measures = c("Base"), 
               modes = c("PerGame"))
check <- as.data.table(dt)
dt$dataTable[[1]]

dt <- 
  teams_tables(seasons  = 2019,
               teams = "Atlanta Hawks",
               tables = "player",
               measures = c("Advanced"), 
               modes = c("PerGame"))

dt$dataTable[[1]] %>% as.data.table()


dt <- 
  teams_players_stats(seasons  = 2019,
                    type = "team",
                    tables = "general",
                    measures = c("Four Factors"), 
                    modes = c("PerGame"))
dt$dataTable[[1]] %>% as.data.table()


dt <- 
  teams_players_stats(seasons  = 2019,
                      months = 2,
                      type = "team",
                      tables = "general",
                      measures = c("Four Factors"), 
                      modes = c("PerGame"))
dt$dataTable[[1]] %>% as.data.table()

# BY month and between dates works
dt <- 
  teams_players_stats(seasons = 2021,
                      date_from = as.Date("01/01/2021", format = "%d/%m/%Y"),
                      date_to   = as.Date("10/01/2021", format = "%d/%m/%Y"),
                      type = "team",
                      tables = "general",
                      measures = c("Base", "Four Factors", "Advanced",
                                   "Defense", "Misc", "Scoring", "Usage"), 
                      modes = c("PerGame"))
dt$dataTable[[3]] %>% as.data.table() %>% str


dt$dataTable 



test <- Reduce(function(dt1, dt2){
  mergeCols <- c("nameTeam", 
                 "rangeDateFrom",
                 "rangeDateTo",
                 "isPlusMinus",
                 "isPaceAdjust",
                 "isRank",
                 "idPlayoffRound",
                 "idMonth",
                 "idPeriod");
  addCols <- c(mergeCols, setdiff(names(dt2), names(dt1))) %>% unique;
  merge(as.data.table(dt1), 
        as.data.table(dt2)[, ..addCols], 
        by = mergeCols,
      all.x = TRUE)},
dt$dataTable)

test
test %>% names

Reduce(function(dt1, dt2){mergeCols <- setdiff(names(dt1),
                                                 names(dt2));
                            merge(as.data.table(dt1), 
                                  as.data.table(dt2), 
                                  by = mergeCols, 
                                  all.x = TRUE)},
dt$dataTable[1:2])

Reduce(function(dt1, dt2){merge(as.data.table(dt1), as.data.table(dt2))},
       dt$dataTable[1:2])

Reduce(function(dt1, dt2){merge(as.data.table(dt1), 
                                as.data.table(dt2), 
                                by = c("nameTeam", 
                                       "rangeDateFrom",
                                       "rangeDateTo",
                                       "isPlusMinus",
                                       "isPaceAdjust",
                                       "isRank",
                                       "idPlayoffRound",
                                       "idMonth",
                                       "idPeriod"), 
                                all = FALSE)},
       dt$dataTable[1:2])


library(tidyverse)
dt$dataTable %>% reduce(inner_join, 
                        by = c("nameTeam", 
                               "rangeDateFrom",
                               "rangeDateTo",
                               "isPlusMinus",
                               "isPaceAdjust",
                               "isRank",
                               "idPlayoffRound",
                               "idMonth",
                               "idPeriod"
                               )) %>% names


$ typeMeasure        : chr  "Advanced" "Advanced" "Advanced" "Advanced" ...
$ isPlusMinus        : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
$ isPaceAdjust       : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
$ isRank             : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
$ idPlayoffRound     : int  0 0 0 0 0 0 0 0 0 0 ...
$ idMonth            : int  0 0 0 0 0 0 0 0 0 0 ...
$ rangeDateFrom      : chr  "2021-01-01" "2021-01-01" "2021-01-01" "2021-01-01" ...
$ rangeDateTo        : chr  "2021-01-10" "2021-01-10" "2021-01-10" "2021-01-10" ...
$ idTeamOpponent     : int  0 0 0 0 0 0 0 0 0 0 ...
$ idPeriod           : int  0 0 0 0 0 0 0 0 0 0 ...
$ countLastNGames    : int  0 0 0 0 0 0 0 0 0 0 ...
$ nameTeam           : chr  "Atlanta Hawks" "Boston Celtics" "Brooklyn Nets" "Charlotte Hornets" ...
$ idTeam
Base
Advanced
Defense
Four Factors
Misc
Opponent
Scoring
Usage



x <- teams_tables(teams = c("Brooklyn Nets", "New York Knicks"),
                  seasons = 2017:2018, tables = c("splits", "shooting"), 
                  measures = "Base", modes = c("PerGame", "Totals"))
x$dataTable[[5]]

# Dud
install_github('stephematician/statsnbaR')
library(statsnbaR)
statsnbaR::team_game_logs()
team_game_logs(season=2014,
               season_type='playoff')
