###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################
rm(list = ls()); gc();

setwd("~/kaggle_projects/kaggle/nbaSpreads/")
source("99_functions.R")

modelFile <- readFile(getModelFileFeaturesPath()) %>% 
  .[!is.na(covered)] %>% 
  .[, response := covered]

modelFile <- splitTrainTestHoldout(modelFile)
modelFile[, .N, keyby = .(season, holdout_flag)]

# Test coinflip model
printTitle("Coinflip")
set.seed(123)
modelFile[, prediction := runif(.N)]
err <- modelFile[, .(.N, error = calcError(prediction, response)), keyby = .(is.na(prediction), holdout_flag)]
print(err)
err <- modelFile[prediction >= quantile(modelFile$prediction, 0.9, na.rm = TRUE), 
                 .(.N, error = calcError(prediction, response)), keyby = .(is.na(prediction), holdout_flag)]
print(err)
getCMprecision(modelFile) %>% print
modelFile[, .(getCMprecision(.SD), .N), by = holdout_flag] %>% print
# plotMeanError(modelFile, "test")
# plotMeanError(modelFile, "response")
# col <- names(modelFile)[25]
# plotMeanError(modelFile, col)

# Test home team model
printTitle("Home team")
modelFile[, prediction := ifelse(location == "home", TRUE, FALSE)]
err <- modelFile[, .(.N, error = calcError(prediction, response)), keyby = .(is.na(prediction), holdout_flag)]
print(err)
err <- modelFile[prediction >= quantile(modelFile$prediction, 0.9, na.rm = TRUE), 
                 .(.N, error = calcError(prediction, response)), keyby = .(is.na(prediction), holdout_flag)]
print(err)
getCMprecision(modelFile) %>% print
modelFile[, .(getCMprecision(.SD), .N), by = holdout_flag] %>% print


# Test logistic model
printTitle("Logistic")
model <- glm(response ~ targetSpread +  
               target_gamesInLastDays + 
               target_won_lag1 + target_won_lag2 + 
               target_covered_lag1 + target_covered_lag2,
             family = binomial(link = 'logit'), 
             data   = modelFile[holdout_flag != -1])

modelFile[, prediction := predict(model, .SD, type = "response")]
err <- modelFile[, .(.N, error = calcError(prediction, response)), keyby = .(is.na(prediction), holdout_flag)]
print(err)
err <- modelFile[prediction >= quantile(modelFile$prediction, 0.9, na.rm = TRUE), 
                 .(.N, error = calcError(prediction, response)), keyby = .(is.na(prediction), holdout_flag)]
print(err)

hist(modelFile$prediction)
getCMprecision(modelFile) %>% print
modelFile[, .(getCMprecision(.SD), .N), by = holdout_flag] %>% print

# Test GBM model
printTitle("GBM")

features <- c("targetSpread", 
              "home",
              # "target_won_lag1", "target_won_lag2",
              # "target_covered_lag1", "target_covered_lag2",
              # "oppo_won_lag1", "oppo_won_lag2",
              # "oppo_covered_lag1", "oppo_covered_lag2",
              "target_gamesInLastDays",
              "oppo_gamesInLastDays",
              # "target_sum_covered",
              # "oppo_sum_covered")
              "target_sum_won",
              "oppo_sum_won",
              "target_sum_location",
              "oppo_sum_location")
dtrain <- xgb.DMatrix(data = data.matrix(modelFile[holdout_flag == 0, ..features]), 
                      label = data.matrix(modelFile[holdout_flag == 0, response]))
dholdout <- xgb.DMatrix(data = data.matrix(modelFile[holdout_flag == 1, ..features]), 
                        label = data.matrix(modelFile[holdout_flag == 1, response]))
# Set up other params
baseScore <- modelFile[holdout_flag == 0, mean(response)] # starting loc of 
params <- list(
  booster     = "gbtree", # Use gbtree for classification? Else use default
  eta         = 0.01,     # Learning rate - use between 0.01 - 0.05
  objective   = "binary:logistic", # for classification, can use Binary: logistic, Multi:softmax, Multi:softprob
  max_depth   = 6,       # the maximum depth of each decision tree
  base_score  = baseScore,  # initial prediction - can speed up convergence
  eval_metric = "error"    # Evaluation metric (error is default for classification, RMSE for regression)
) 
# Need to decide on best eval_metric + error vs log loss vs auc - impacting convergence?
# Hyper parameter tuning is highly sensitive

# Is it easier to predict lines as a regression? and bet the spread?

xgb <- xgb.train(data                  = dtrain,     # Training data
                 params                = params,
                 nrounds               = 1000,       # max number of boosting iterations
                 early_stopping_rounds = 10,         # Stop if no improvement for X rounds
                 verbose               = TRUE,          # Print out results
                 print_every_n         = 10,
                 watchlist             = list(train   = dtrain, # Set train + holdout sets
                                              holdout = dholdout))

modelFile[, prediction := predict(xgb, data.matrix(.SD[, ..features]))]
err <- modelFile[, .(.N, error = calcError(prediction, response)), keyby = .(holdout_flag)]
print(err)
err <- modelFile[prediction >= quantile(modelFile$prediction, 0.9), 
                 .(.N, error = calcError(prediction, response)), keyby = .(holdout_flag)]
print(err)


getCM(modelFile)
modelFile[, getCMprecision(.SD)] %>% print
modelFile[, .(getCMprecision(.SD), .N), by = holdout_flag]
modelFile[, .(precision = getCMprecision(.SD), 
              .N, 
              P = sum(response), 
              TP = sum(response == TRUE & prediction > 0.5)), 
          keyby = holdout_flag] %>% print

# Test GBM model
printTitle("GBM ALL FEATURES")


names_features <- readFile(getTeamFactDataPath()) %>% 
  names %>%  
  setdiff(c("month_start_dates", "month_end_dates", "nameTeam", "isPlusMinus",
            "isPaceAdjust", "isRank", "idPlayoffRound", "idMonth" , "idPeriod", "typeMeasure", 
            "idTeamOpponent", "countLastNGames", "idTeam", "season", "month")) %>%
  CJ(feature = ., team = c("_target", "_oppo")) %>% 
  .[, paste0(feature, team)]

modelFile[, efg_diff_target := round(pctEFG_target - pctEFGOpponent_target, 1)]
modelFile[, efg_diff_oppo := pctEFG_oppo - pctEFGOpponent_oppo]
modelFile[, hist(pctEFG_target - pctEFGOpponent_target)]

features <- c("targetSpread", 
              "home",
              # "target_won_lag1", "target_won_lag2",
              # "target_covered_lag1", "target_covered_lag2",
              # "oppo_won_lag1", "oppo_won_lag2",
              # "oppo_covered_lag1", "oppo_covered_lag2",
              "target_gamesInLastDays",
              "oppo_gamesInLastDays",
              # "target_sum_covered",
              # "oppo_sum_covered")
              "target_sum_won",
              "oppo_sum_won",
              "target_sum_location",
              "oppo_sum_location",
              "efg_diff_target",
              "efg_diff_oppo")

names_features[1:50]
                # ,names_features)

dtrain <- xgb.DMatrix(data = data.matrix(modelFile[holdout_flag == 0, ..features]), 
                      label = data.matrix(modelFile[holdout_flag == 0, response]))
dholdout <- xgb.DMatrix(data = data.matrix(modelFile[holdout_flag == 1, ..features]), 
                        label = data.matrix(modelFile[holdout_flag == 1, response]))
# Set up other params
baseScore <- modelFile[holdout_flag == 0, mean(response)] # starting loc of 
params <- list(
  booster     = "gbtree", # Use gbtree for classification? Else use default
  eta         = 0.05,     # Learning rate - use between 0.01 - 0.05
  objective   = "binary:logistic", # for classification, can use Binary: logistic, Multi:softmax, Multi:softprob
  max_depth   = 4,       # the maximum depth of each decision tree
  base_score  = baseScore,  # initial prediction - can speed up convergence
  eval_metric = "error"    # Evaluation metric (error is default for classification, RMSE for regression)
) 
# Need to decide on best eval_metric + error vs log loss vs auc - impacting convergence?
# Hyper parameter tuning is highly sensitive

xgb <- xgb.train(data                  = dtrain,     # Training data
                 params                = params,
                 nrounds               = 100,       # max number of boosting iterations
                 early_stopping_rounds = 50,         # Stop if no improvement for X rounds
                 verbose               = TRUE,          # Print out results
                 print_every_n         = 10,
                 watchlist             = list(train   = dtrain, # Set train + holdout sets
                                              holdout = dholdout))

modelFile[, prediction := predict(xgb, data.matrix(.SD[, ..features]))]
err <- modelFile[, .(.N, error = calcError(prediction, response)), keyby = .(holdout_flag)]
print(err)
err <- modelFile[prediction >= quantile(modelFile$prediction, 0.9), 
                 .(.N, error = calcError(prediction, response)), keyby = .(holdout_flag)]
print(err)


getCM(modelFile)
modelFile[, getCMprecision(.SD)] %>% print
modelFile[, .(getCMprecision(.SD), .N), by = holdout_flag]
modelFile[, .(precision = getCMprecision(.SD), 
              .N, 
              P = sum(response), 
              TP = sum(response == TRUE & prediction > 0.5)), 
          keyby = holdout_flag] %>% print




# Cv model
# printTitle("GBM CV")
# dtrain <- xgb.DMatrix(data = data.matrix(modelFile[holdout_flag %in% c(0, 1), ..features]),
#                       label = data.matrix(modelFile[holdout_flag %in% c(0, 1), response]))
# xgb.cv <- xgb.cv(data                  = dtrain,     # Training data
#                  params                = params,
#                  nfold                 = 5,         # Number of folds
#                  nrounds               = 1000,       # max number of boosting iterations
#                  early_stopping_rounds = 100,         # Stop if no improvement for X rounds
#                  verbose               = FALSE,          # Print out results
#                  print_every_n         = 10)
# 
# xgb <- xgb.train(data                 = dtrain,     # Training data
#                  params                = params,
#                  nrounds               = xgb.cv$best_iteration,       # max number of boosting iterations
#                  verbose               = TRUE,          # Print out results
#                  print_every_n         = 10)
# 
# modelFile[, prediction := predict(xgb, data.matrix(.SD[, ..features]))]
# err <- modelFile[, .(.N, error = calcError(prediction, response)), keyby = .(holdout_flag)]
# print(err)
# err <- modelFile[prediction >= quantile(modelFile$prediction, 0.9),
#                  .(.N, error = calcError(prediction, response)), keyby = .(holdout_flag)]
# print(err)
# 
# getCM(modelFile)
# modelFile[, getCMprecision(.SD)] %>% print
# modelFile[, .(getCMprecision(.SD), .N), by = holdout_flag]
# modelFile[, .(precision = getCMprecision(.SD),
#               .N,
#               P = sum(response),
#               TP = sum(response == TRUE & prediction > 0.5)),
#           keyby = holdout_flag] %>% print


if (FALSE) {
  printTitle("GBM Diagnostic plots")
  
  modelFile[holdout_flag == -1][order(-prediction)][1:10]
  ggplot(modelFile) + 
    geom_histogram(aes(x = prediction, fill = as.factor(holdout_flag))) + 
    facet_grid(holdout_flag~.)
  
  # # XGB diagnostics
  xgb.importance(model = xgb)[, cumGain := cumsum(Gain)][]
  xgb.plot.deepness(model = xgb)
  xgb.plot.importance(importance_matrix = xgb.importance(model = xgb))
  xgb.plot.shap(data  = data.matrix(modelFile[, .SD, .SDcols = features]),
                model = xgb,
                top_n = 12,
                n_col = 3)
  # xgb.plot.tree(data  = data.matrix(modelFile[, .SD, .SDcols = features]),
  #               model = xgb,
  #               trees = 2)
  # xgb.ggplot.shap.summary(data  = data.matrix(modelFile[, .SD, .SDcols = features]),
  #                         model = xgb,
  #                         top_n = 2)
  # dt   <- modelFile[, xgb$feature_names, with = FALSE]
  # shap_values <- predict(xgb, 
  #                        xgb.DMatrix(data = data.matrix(dt), missing = NA), 
  #                        predcontrib = TRUE, 
  #                        approxcontrib = FALSE)
  # summary(shap_values)
  # 
  # xgb.plot.shap(model = xgb, 
  #               data         = as.matrix(dt),
  #               shap_contrib = shap_values,
  #               features     = xgb$feature_names,
  #               top_n = 1)
  # 
  # xgb.plot.shap(model        = xgb,
  #               data         = as.matrix(dt, missing = NA),
  #               shap_contrib = shap_values,
  #               features     = xgb$feature_names,
  #               top_n = 1)
}

