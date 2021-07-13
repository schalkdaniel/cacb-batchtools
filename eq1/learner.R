ncores = parallel::detectCores()
cwb_pars = list(
  patience = 10L,
  oob_fraction = 0.67,
  mstop = 5000L,
  oob_seed = 1618,
  eps_for_break = 0.00001,
  use_stopper = FALSE,
  ncores = ncores,
  stop_both = FALSE)

## Binning benchmark
## ===================================

### CWB - no tuning
lrn_bin_cwb_nb = lrn("classif.compboost", id = "ps_cwb_nb", predict_type = "prob",
  optimizer = "cod", restart = FALSE, learning_rate = 0.1, df_autoselect = TRUE)

classif_lrn_cwb_notune$param_set$values = updatePars(classif_lrn_cwb_notune, cwb_pars)

lrn_bin_cwb_b = lrn("classif.compboost", id = "ps_cwb_b", predict_type = "prob",
  optimizer = "cod", restart = FALSE, learning_rate = 0.1, df_autoselect = TRUE, bin_root = 2L)

classif_lrn_cwb_notune_bin$param_set$values = updatePars(classif_lrn_cwb_notune_bin, cwb_pars)

## Acceleration benchmark
## ===================================

### CWB - no tuning
lrn_bin_cwb_nb = lrn("classif.compboost", id = "ps_cwb_nb", predict_type = "prob",
  optimizer = "cod", restart = FALSE, learning_rate = 0.01, df_autoselect = TRUE)

classif_lrn_cwb_notune$param_set$values = updatePars(classif_lrn_cwb_notune, cwb_pars)


### ACWB - no tuning
lrn_acc_acwb = lrn("classif.compboost", id = "ps_acc_acwb", predict_type = "prob",
  optimizer = "nesterov", restart = FALSE, learning_rate = 0.01, momentum = 0.0034, df_autoselect = TRUE)
classif_lrn_acwb_notune$param_set$values = updatePars(classif_lrn_acwb_notune, cwb_pars)

### HCWB - no tuning
lrn_acc_hcwb = lrn("classif.compboost", id = "ps_acc_hcwb", predict_type = "prob",
  optimizer = "nesterov", restart = TRUE, learning_rate = 0.01, momentum = 0.03, df_autoselect = TRUE)
classif_lrn_hcwb_notune$param_set$values = updatePars(classif_lrn_hcwb_notune, cwb_pars)


learners = list(
  bm_bin_cwb_nb = lrn_bin_cwb_nb,
  bm_bin_cwb_b  = lrn_bin_cwb_b,

  bm_acc_cwb  = lrn_acc_cwb,
  bm_acc_acwb = lrn_acc_acwb,
  bm_acc_hcwb = lrn_acc_hcwb
)
