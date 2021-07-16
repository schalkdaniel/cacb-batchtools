ncores = parallel::detectCores()
cwb_pars = list(
  patience = 10L,
  mstop = 5000L,
  oob_seed = 1618,
  eps_for_break = 0.00001,
  ncores = ncores,
  stop_both = FALSE)

updatePars = function(lrn, pars) {
  lvalues = lrn$param_set$values
  return(mlr3misc::insert_named(lvalues, cwb_pars))
}

robustify = po("removeconstants", id = "removeconstants_before") %>>%
  po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
  po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
  po("collapsefactors", target_level_count = 10) %>>%
  po("removeconstants", id = "removeconstants_after")

## Binning benchmark
## ===================================

### CWB - no tuning
lrn_bin_cwb_nb = lrn("classif.compboost", id = "bin_cwb_nb", predict_type = "prob",
  optimizer = "cod", restart = FALSE, learning_rate = 0.1, df_autoselect = TRUE)
lrn_bin_cwb_nb$param_set$values = updatePars(lrn_bin_cwb_nb, cwb_pars)

lrn_bin_cwb_b = lrn("classif.compboost", id = "bin_cwb_b", predict_type = "prob",
  optimizer = "cod", restart = FALSE, learning_rate = 0.1, df_autoselect = TRUE, bin_root = 2L)
lrn_bin_cwb_b$param_set$values = updatePars(lrn_bin_cwb_b, cwb_pars)

## Acceleration benchmark
## ===================================

### CWB - no tuning
lrn_acc_cwb = lrn("classif.compboost", id = "acc_cwb", predict_type = "prob",
  optimizer = "cod", restart = FALSE, learning_rate = 0.01, df_autoselect = TRUE)
lrn_acc_cwb$param_set$values = updatePars(lrn_acc_cwb, cwb_pars)


### ACWB - no tuning
lrn_acc_acwb = lrn("classif.compboost", id = "acc_acwb", predict_type = "prob",
  optimizer = "nesterov", restart = FALSE, learning_rate = 0.01, momentum = 0.0034, df_autoselect = TRUE)
lrn_acc_acwb$param_set$values = updatePars(lrn_acc_acwb, cwb_pars)

### HCWB - no tuning
lrn_acc_hcwb = lrn("classif.compboost", id = "ps_acc_hcwb", predict_type = "prob",
  optimizer = "nesterov", restart = TRUE, learning_rate = 0.01, momentum = 0.03,
  df_autoselect = TRUE, oob_fraction = 0.67, use_stopper = TRUE)
lrn_acc_hcwb$param_set$values = updatePars(lrn_acc_hcwb, cwb_pars)

## Create object of all learners
## ===================================

learners = list(
  bm_bin_cwb_nb = lrn_bin_cwb_nb,
  bm_bin_cwb_b  = lrn_bin_cwb_b,

  bm_acc_cwb  = lrn_acc_cwb,
  bm_acc_acwb = lrn_acc_acwb,
  bm_acc_hcwb = lrn_acc_hcwb
)
learners = lapply(learners, function(ll) GraphLearner$new(robustify %>>% ll$clone(deep = TRUE)))
