## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#install.packages("devtools")
#devtools::install_github("SchlossLab/mikropml")
library(mikropml)
head(otu_mini_bin)

## ---- eval = FALSE------------------------------------------------------------
#  results <- run_ml(otu_mini_bin,
#                    'glmnet',
#                    outcome_colname = 'dx',
#                    seed = 2019)

## ---- echo = FALSE------------------------------------------------------------
# reduce vignette runtime by using precomputed results
results <- otu_mini_bin_results_glmnet

## -----------------------------------------------------------------------------
names(results)

## -----------------------------------------------------------------------------
names(results$trained_model)

## -----------------------------------------------------------------------------
head(results$test_data)

## -----------------------------------------------------------------------------
results$performance

## -----------------------------------------------------------------------------
results$feature_importance

## -----------------------------------------------------------------------------
results_custom <- run_ml(otu_mini_bin,
                         'glmnet',
                         kfold = 2,
                         cv_times = 5,
                         training_frac = 0.5,
                         seed = 2019)

## ---- echo=FALSE--------------------------------------------------------------
# TODO: can we get these programmatically somehow instead of hard-coding them?
c("logLoss", "AUC", "prAUC", "Accuracy", "Kappa", "Mean_F1", "Mean_Sensitivity", "Mean_Specificity", "Mean_Pos_Pred_Value", "Mean_Neg_Pred_Value", "Mean_Precision", "Mean_Recall", "Mean_Detection_Rate", "Mean_Balanced_Accuracy")

## ---- echo=FALSE--------------------------------------------------------------
c("RMSE", "Rsquared", "MAE")

## -----------------------------------------------------------------------------
results_pr <- run_ml(otu_mini_bin, 
                     'glmnet', 
                     cv_times = 5, 
                     perf_metric_name = 'prAUC', 
                     seed = 2019)

## -----------------------------------------------------------------------------
results_pr$performance

## ---- eval = FALSE------------------------------------------------------------
#  # make random groups
#  set.seed(2019)
#  grps <- sample(LETTERS[1:8], nrow(otu_mini_bin),replace=TRUE)
#  results_grp <- run_ml(otu_mini_bin,
#                        'glmnet',
#                        cv_times = 5,
#                        training_frac = 0.8,
#                        groups = grps,
#                        seed = 2019)

## ---- eval = FALSE------------------------------------------------------------
#  results_imp <- run_ml(otu_mini_bin,
#    "rf",
#    outcome_colname = "dx",
#    find_feature_importance = TRUE,
#    seed = 2019
#  )

## ---- echo = FALSE------------------------------------------------------------
results_imp <- otu_mini_bin_results_rf

## -----------------------------------------------------------------------------
results_imp$feature_importance

## -----------------------------------------------------------------------------
results_imp_corr <- run_ml(otu_mini_bin,
                           'glmnet',
                           cv_times = 5,
                           find_feature_importance = TRUE,
                           corr_thresh = 0.2,
                           seed = 2019)
results_imp_corr$feature_importance

## ---- eval = FALSE------------------------------------------------------------
#  results_rf <- run_ml(otu_mini_bin,
#                       'rf',
#                       cv_times = 5,
#                       seed = 2019)

## ---- eval = FALSE------------------------------------------------------------
#  results_rf_nt <- run_ml(otu_mini_bin,
#                          'rf',
#                          cv_times = 5,
#                          ntree = 10,
#                          seed = 2019)

## ---- eval = FALSE------------------------------------------------------------
#  results_dt <- run_ml(otu_mini_bin,
#                       'rpart2',
#                       cv_times = 5,
#                       seed = 2019)

## ---- eval = FALSE------------------------------------------------------------
#  results_svm <- run_ml(otu_mini_bin,
#                        'svmRadial',
#                        cv_times = 5,
#                        seed = 2019)

## -----------------------------------------------------------------------------
otu_mini_multi %>% dplyr::pull('dx') %>% unique()

## ---- eval = FALSE------------------------------------------------------------
#  results_multi <- run_ml(otu_mini_multi,
#                          outcome_colname = "dx",
#                          seed = 2019
#  )

## ---- echo = FALSE------------------------------------------------------------
results_multi <- otu_mini_multi_results_glmnet

## -----------------------------------------------------------------------------
results_multi$performance

## ---- eval = FALSE------------------------------------------------------------
#  results_cont <- run_ml(otu_mini_bin[, 2:11],
#                         'glmnet',
#                         outcome_colname = 'Otu00001',
#                         seed = 2019)

## ---- echo = FALSE------------------------------------------------------------
results_cont <- otu_mini_cont_results_glmnet

## -----------------------------------------------------------------------------
results_cont$performance

