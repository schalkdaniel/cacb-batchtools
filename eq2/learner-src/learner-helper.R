dfAutoselect = function(task, df, nbases) {
  out = list(df = NA, df_cat = NA)
  factor_cols = task$feature_types$id[task$feature_types$type == "factor"]
  if (length(factor_cols) > 0) {
    df_cat_min = min(vapply(
      X = task$data(cols = factor_cols),
      FUN = function(fc) length(unique(fc)),
      FUN.VALUE = integer(1L)
    ))
    df = min(c(df_cat_min, nbases))
    if (df <= 3) df = 5L

    out$num = df
    out$cat = df_cat_min
  } else {
    out$num = nbases
  }
  return(out)
}

checkCores = function(task, ncores) {
  if (ncores > length(task$feature_names)) {
    warning("Number of cores ", ncores, " exceeds number of features ",
      length(task$feature_names), ". The number of cores are set to ", length(task$feature_names), "!")
    return(length(task$feature_names))
  }
  return(ncores)
}

getAUCLoss = function() {
  aucLoss = function(truth, response) return((1 - mlr::measureAUC(response, truth, negative = -1, positive = 1)) * length(truth))
  aucGrad = function(truth, response) return(rep(0, length(truth)))
  aucInit = function(truth) {
    p = mean(truth == 1)
    return(0.5 * p / (1 - p))
  }
  my_auc_loss = LossCustom$new(aucLoss, aucGrad, aucInit)
  return(my_auc_loss)
}

getAUCLossInit = function(init) {
  aucLoss = function(truth, response) return((1 - mlr::measureAUC(response, truth, negative = -1, positive = 1)) * length(truth))
  aucGrad = function(truth, response) return(rep(0, length(truth)))
  aucInit = function(truth) return(init)

  my_auc_loss = LossCustom$new(aucLoss, aucGrad, aucInit)
  return(my_auc_loss)
}

