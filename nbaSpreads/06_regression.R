###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################
rm(list = ls()); gc();

# setwd("~/nbaSpreads/")
source("99_functions.R")

modelFile <- readFile(getModelFileFeaturesPath()) %>% 
  .[!is.na(targetSpread) & !is.na(targetDiff)] %>% 
  .[, response := -targetDiff]

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
modelFile <- splitTrainTestHoldout(modelFile)
modelFile[, .N, keyby = .(season, holdout_flag)]

# Test GBM model
printTitle("GBM")
grep("_target", names(modelFile), value = TRUE)

modelFile[, efg_diff_target := 100 * (pctEFG_target - pctEFGOpponent_target)]
modelFile[, efg_diff_oppo := 100 * (pctEFG_oppo - pctEFGOpponent_oppo)]
modelFile[, tov_diff_target := 100 * (pctTOVTeam_target - pctTOVOpponent_target)]
modelFile[, tov_diff_oppo := 100 * (pctTOVTeam_oppo - pctTOVOpponent_oppo)]
modelFile[, offreb_diff_target := 100 * (pctOREB_target - pctOREBOpponent_target)]
modelFile[, offreb_diff_oppo := 100 * (pctOREB_oppo - pctOREBOpponent_oppo)]
modelFile[, FTFGA_diff_target := 100 * (rateFTA_target - rateFTAOpponent_target)]
modelFile[, FTFGA_diff_oppo := 100 * (rateFTA_oppo - rateFTAOpponent_oppo)]
modelFile[, FTFGA_diff_target := 100 * (rateFTA_target - rateFTAOpponent_target)]
modelFile[, FTFGA_diff_oppo := 100 * (rateFTA_oppo - rateFTAOpponent_oppo)]

# modelFile[, hist(pctEFG_target - pctEFGOpponent_target)]



features <- c(
  "targetSpread"
  ,"home"
  ,"target_gamesInLastDays"
  ,"oppo_gamesInLastDays"
  ,"target_sum_won"
  ,"oppo_sum_won"
  ,"target_sum_location"
  ,"oppo_sum_location"
  ,"efg_diff_target"
  ,"efg_diff_oppo"
  ,"tov_diff_target"
  ,"tov_diff_oppo"
  ,"offreb_diff_target"
  ,"offreb_diff_oppo"
  ,"FTFGA_diff_target"
  ,"FTFGA_diff_oppo"
  ,"netrtg_target"
  ,"netrtg_oppo"
  )
dtrain <- xgb.DMatrix(data = data.matrix(modelFile[holdout_flag == 0, ..features]), 
                      label = data.matrix(modelFile[holdout_flag == 0, response]))
dholdout <- xgb.DMatrix(data = data.matrix(modelFile[holdout_flag == 1, ..features]), 
                        label = data.matrix(modelFile[holdout_flag == 1, response]))
# Set up other params
baseScore <- modelFile[holdout_flag == 0, mean(response)] # starting loc of 
params <- list(
  booster     = "gbtree", # Use gbtree for classification? Else use default
  eta         = 0.05,     # Learning rate - use between 0.01 - 0.05
  objective   = "reg:squarederror", # for classification, can use Binary: logistic, Multi:softmax, Multi:softprob
  max_depth   = 4,       # the maximum depth of each decision tree
  base_score  = baseScore  # Evaluation metric (error is default for classification, RMSE for regression)
) 
# Need to decide on best eval_metric + error vs log loss vs auc - impacting convergence?
# Hyper parameter tuning is highly sensitive

# Is it easier to predict lines as a regression? and bet the spread?

xgb <- xgb.train(data                  = dtrain,     # Training data
                 params                = params,
                 nrounds               = 1000,       # max number of boosting iterations
                 early_stopping_rounds = 100,         # Stop if no improvement for X rounds
                 verbose               = TRUE,          # Print out results
                 print_every_n         = 10,
                 watchlist             = list(train   = dtrain, # Set train + holdout sets
                                              holdout = dholdout))

modelFile[, prediction := predict(xgb, data.matrix(.SD[, ..features]))]

modelFile[, rmse(prediction, response), keyby = holdout_flag] %>% print

modelFile[order(prediction)
          ][, rnk := 1:.N, by = holdout_flag
            ][, group := floor(100 * rnk / .N), by = holdout_flag
              ][group == 100, group := 99
                ][, .(.N,
                      meanPred = mean(prediction),
                      meanResponse = mean(response)),
                  by = .(group, holdout_flag)] %>%
  ggplot() + 
  geom_line(aes(x = group, y = meanPred, colour = "prediction")) + 
  geom_line(aes(x = group, y = meanResponse, colour = "response")) + 
  facet_wrap(holdout_flag~.)


getCM2 <- function(dt) {
  confusionMatrix(dt[, prediction > targetSpread] %>% as.factor,
                  dt[, covered] %>% as.factor,
                  positive = "TRUE")
}

getCMprecision2 <- function(dt){
  cm <- getCM2(dt)
  return(cm$byClass["Precision"])
}
modelFile[, .(getCMprecision2(.SD), .N), by = holdout_flag]

ggplot(modelFile) + 
  geom_freqpoly(aes(x = response,  colour = "response")) + 
  geom_freqpoly(aes(x = prediction, colour = "prediction")) + 
  facet_wrap(holdout_flag~.)

modelFile[response < targetSpread, .N, by = covered]
modelFile[covered == TRUE, .N, by = response < targetSpread]


modelFile[prediction < targetSpread, 
          .(target_team_name,oppo_team_name,target_score, oppo_score, targetSpread, targetDiff, covered, prediction, response)
          ][1]

modelFile[prediction - targetSpread < 0, 
          .N, by = .(covered)
          ][, percent := N/sum(N)
            ][]

modelFile[prediction - targetSpread < -1, 
          .N, by = .(holdout_flag, covered)
          ][, percent := N/sum(N), by = holdout_flag
            ][(covered)]

modelFile[prediction - targetSpread < -2, 
          .N, by = covered
          ][, percent := N/sum(N)][]

modelFile[, plot(prediction - targetSpread, response)]
modelFile[order(prediction-targetSpread)
          ][, .(date, target_team_name,oppo_team_name,target_score, oppo_score, targetSpread, targetDiff, covered, prediction, response)
            ][2]
if (FALSE){
  # # XGB diagnostics
  xgb.importance(model = xgb)[, cumGain := cumsum(Gain)][]
  xgb.plot.deepness(model = xgb)
  xgb.plot.importance(importance_matrix = xgb.importance(model = xgb))
  xgb.plot.shap(data  = data.matrix(modelFile[, .SD, .SDcols = features]),
                model = xgb,
                top_n = 12,
                n_col = 3)
}
