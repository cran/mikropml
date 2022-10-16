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

## ----custom_train_indices, warning=FALSE--------------------------------------
n_obs <- otu_mini_bin %>% nrow()
training_size <- 0.8 * n_obs
training_rows <- sample(n_obs, training_size)
results_custom_train <- run_ml(otu_mini_bin,
                               'glmnet',
                               kfold = 2,
                               cv_times = 5,
                               training_frac = training_rows,
                               seed = 2019
                               )

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

## ----custom_groups, warning=FALSE---------------------------------------------
# make random groups
set.seed(2019)
grps <- sample(LETTERS[1:8], nrow(otu_mini_bin), replace=TRUE)
results_grp <- run_ml(otu_mini_bin, 
                      'glmnet', 
                      cv_times = 2, 
                      training_frac = 0.8, 
                      groups = grps, 
                      seed = 2019)

## ----group_partitions, warning=FALSE------------------------------------------
results_grp_part <- run_ml(otu_mini_bin, 
                      'glmnet', 
                      cv_times = 2, 
                      training_frac = 0.8, 
                      groups = grps, 
                      group_partitions = list(train = c('A', 'B'),
                                              test = c('C', 'D')
                                              ),
                      seed = 2019)

## ----only_group_A_train, warning = FALSE--------------------------------------
results_grp_trainA <- run_ml(otu_mini_bin, 
                      'glmnet', 
                      cv_times = 2, 
                      kfold = 2,
                      training_frac = 0.5, 
                      groups = grps, 
                      group_partitions = list(train = c("A", "B", "C", "D", "E", "F"),
                                              test = c("A", "B", "C", "D", "E", "F", "G", "H")
                                              ),
                      seed = 2019)

## ----calc-case-weights, message = FALSE---------------------------------------
library(dplyr)
case_weights_dat <- otu_mini_bin %>%
    count(dx) %>%
    mutate(p = n / sum(n)) %>%
    select(dx, p) %>% 
    right_join(otu_mini_bin, by = 'dx') %>%
    select(-starts_with("Otu")) %>% 
    mutate(in_train = sample(c(TRUE, FALSE), size = nrow(otu_mini_bin), 
                             replace = TRUE, prob = c(0.70, 0.30)),
           row_num = row_number()) %>% 
    filter(in_train)
head(case_weights_dat)
nrow(case_weights_dat) / nrow(otu_mini_bin)

## ----weighted-results, eval = FALSE-------------------------------------------
#  results_weighted <- run_ml(otu_mini_bin,
#                    'glmnet',
#                    outcome_colname = 'dx',
#                    seed = 2019,
#                    training_frac = case_weights_dat %>% pull(row_num),
#                    weights = case_weights_dat %>% pull(p))

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
#                          ntree = 1000,
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

