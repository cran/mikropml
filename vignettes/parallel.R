## ---- include = FALSE---------------------------------------------------------
options(future.supportsMulticore.unstable = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(mikropml)
library(future.apply)

## ----register, eval = FALSE---------------------------------------------------
#  doFuture::registerDoFuture()
#  future::plan(future::multicore, workers = 2)

## ----run_single---------------------------------------------------------------
otu_data_preproc <- preprocess_data(otu_small, 'dx')$dat_transformed
result1 <- run_ml(otu_data_preproc, 'regLogistic')

## ----multi_seeds--------------------------------------------------------------
results_multi <- future_lapply(seq(100, 102), function(seed) {
# NOTE: use more seeds for real-world data
  run_ml(otu_data_preproc, 'regLogistic', seed = seed)
  }, future.seed = TRUE)

## ----bind_multi_seeds---------------------------------------------------------
perf_df <- future_lapply(results_multi, function(result) {
  result[['performance']]
  }, future.seed = TRUE) %>% 
  dplyr::bind_rows()
perf_df

