#' Construct tasks from ids and type of source
#'
#' The function takes vectors of ids and types of the same length and returns the
#' task defined in ids[i] and loads it from types[i]. Ids should be a task id from OpenML or a
#' task name from mlr3. The type should be `"mlr-task"` if id is the name of an mlr task called
#' from `tsk(id)` or `"oml-task"` if id is the task id.
#' @param ids [`character()`] Ids for the tasks.
#' @param types [`character()`] Source type of the tasks specified in ids.
#' @output [`list(mlr3::Task`] List of `mlr3::Task` objects.
constructTasks = function(ids, types) {
  suppressMessages(requireNamespace("mlr3oml"))
  tasks = list()

  for (i in seq_along(ids)) {
    id   = ids[i]
    type = types[i]

    message(as.character(Sys.time()), ": (", i, "/", length(ids),
      ") Process task ", id)
    if (type == "oml-task") {
      e = try({

        nuisance = capture.output({
          ts = tsk("oml", task_id = as.integer(as.character(id)))
        })
        if (as.character(id) == "168335") {
          dat = ts$data()
          for (i in seq_along(dat)) {
            if (is.numeric(dat[[i]])) {
              idx_na = dat[[i]] == -999
              dat[[i]][idx_na] = NA
            }
          }
          ts = TaskClassif$new(id = ts$id, backend = dat, target = "signal")
        }
        ts
      }, silent = TRUE)
      if (! "try-error" %in% class(e)) {
        if ("twoclass" %in% e$properties) {
          if (! all(is.na(e$data()))) tasks[[as.character(id)]] = e
        }
      } else {
        cat(e)
      }
    }
    if (type == "mlr-task") {
      tasks[[as.character(id)]] = tsk(as.character(id))
    }
  }
  return(tasks)
}

#' Create list of resample objects
#'
#' Generate a list of resample objects based on the task. The resample object is
#' defined by passing the `.key` and resample arguments via `...`. The resample
#' object is initialized and instantiated for each task in `tasks`. The seed is
#' set prior to the instantiation to ensure reproducible train-test splits.
#' @param tasks [`list(mlr3::Task)`] List of tasks for which the resampling is initialized.
#' @param seed [`integer(1L)`] Seed set for reproducibility.
#' @param ... Optional args passt to `rsmp(...)`.
#' @output [`list(mlr3::Resampling)`] Named list of resample objects instantiated per task
createResampleSets = function(tasks, seed, ...) {
  resample_sets = lapply(tasks, function (task) {
    ts = task$clone(deep = TRUE)
    ts$col_roles$stratum = ts$target_names
    rr = rsmp(...)

    set.seed(seed)
    rr$instantiate(ts)
    return(rr$clone(deep = TRUE))
  })
  return(resample_sets)
}

#' Construct learner from learner id
#'
#' Constructing a learner from an learner id `lid`. The lid must be one of
#' 'bin_cwb_nb', 'bin_cwb_b', 'acc_cwb', 'acc_acwb', or 'acc_hcwb'. The
#' learner is initialized with parameter and put at the end of a pipeline.
#' The pipeline looks as follows:
#' removeconstant %>>% imputemean %>>% imputemode %>>%
#'   collapsefactor %>>% removeconstants %>>% learner
#' @param lid [`character(1L)`] Id of the returned learner.
#' @param ncores [`integer(1L)`] Number of cores used for training. Default is `parallel::detectCores() - 2`.
#'   If `ncores` is bigger than the the number of features, a wanring is printed and `ncores` is set to the
#'   number of features in the learner to avoid dumping the memory:
#'   https://stackoverflow.com/questions/67131322/what-causes-increasing-memory-consumption-in-openmp-based-simulation
#'   Additionally, `ncores` is set to 1 if `ncores < 1`.
#' @param test_mode [`logical(1L)`] Mode for testing with 100 boosting iterations.
#' @param raw_learner [`logical(1L)`] Return just raw learner without pipeline.
#' @output [`mlr3pipelines::GraphLearner`] Pipeline with learner at last pipe operator
#'   (see above for details).
constructLearner = function(lid, ncores = parallel::detectCores() - 2, test_mode = FALSE, raw_learner = FALSE) {
  mstop = 5000L
  if (test_mode) mstop = 100L
  ## Set default hyper parameters (HPs) for all learners:

  cwb_pars = list(
    patience      = 10L,
    mstop         = mstop,
    oob_seed      = 1618,
    eps_for_break = 0.00001,
    ncores        = max(ncores, 1),
    stop_both     = FALSE,
    show_output   = TRUE)

  ## Helper to merge custom set HPs with default values:
  updatePars = function(lrn, pars) {
    lvalues = lrn$param_set$values
    return(mlr3misc::insert_named(lvalues, cwb_pars))
  }

  ## Robustify pipeline put in front of the learner:
  robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")

  ## Switch to get learner based on learner id (lid). Each learner is first defined with custom HPs
  ## which are then merged with the defaults defined above:
  lout = switch(lid,
    ## CWB no binning
    bin_cwb_nb = {
      l = lrn("classif.compboost", id = "bin_cwb_nb", predict_type = "prob",
        optimizer = "cod", restart = FALSE, learning_rate = 0.1, df_autoselect = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## CWB binning
    bin_cwb_b = {
      l = lrn("classif.compboost", id = "bin_cwb_b", predict_type = "prob",
        optimizer = "cod", restart = FALSE, learning_rate = 0.1, df_autoselect = TRUE,
        bin_root = 2L)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## CWB v2 no binning
    acc_cwb = {
      l = lrn("classif.compboost", id = "acc_cwb", predict_type = "prob",
        optimizer = "cod", restart = FALSE, learning_rate = 0.1, df_autoselect = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## ACWB no binning
    acc_acwb = {
      l = lrn("classif.compboost", id = "acc_acwb", predict_type = "prob",
        optimizer = "nesterov", restart = FALSE, learning_rate = 0.1, momentum = 0.0034,
        df_autoselect = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## HCWB no binning
    acc_hcwb = {
      l = lrn("classif.compboost", id = "acc_hcwb", predict_type = "prob",
        optimizer = "nesterov", restart = TRUE, learning_rate = 0.1, momentum = 0.03,
        df_autoselect = TRUE, oob_fraction = 0.3, use_stopper = FALSE, use_stopper_auc = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    acc_hcwb2 = {
      l = lrn("classif.compboost", id = "acc_hcwb", predict_type = "prob",
        optimizer = "nesterov", restart = TRUE, learning_rate = 0.1, momentum = 0.003,
        df_autoselect = TRUE, oob_fraction = 0.3, use_stopper = FALSE, use_stopper_auc = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    }
  )
  if (is.null(lout))
    stop("'lid' is not valid, use one of 'bin_cwb_nb', 'bin_cwb_n', 'acc_cwb', 'acc_acwb', 'acc_hcwb', or 'acc_hcwb2'!")
  if (raw_learner) {
    return(lout)
  } else {
    return(GraphLearner$new(robustify %>>% lout))
  }
}



#' Construct learner from learner id
#'
#' Constructing a learner from an learner id `lid`. The
#' learner is initialized with parameter and put at the end of a pipeline.
#' The pipeline looks as follows:
#' removeconstant %>>% imputemean %>>% imputemode %>>%
#'   collapsefactor %>>% removeconstants %>>% learner
#' @param lid [`character(1L)`] Id of the returned learner.
#' @param ncores [`integer(1L)`] Number of cores used for training. Default is `parallel::detectCores() - 2`.
#'   If `ncores` is bigger than the the number of features, a wanring is printed and `ncores` is set to the
#'   number of features in the learner to avoid dumping the memory:
#'   https://stackoverflow.com/questions/67131322/what-causes-increasing-memory-consumption-in-openmp-based-simulation
#'   Additionally, `ncores` is set to 1 if `ncores < 1`.
#' Additionally, 'constructLearner2' does the same as 'constructLearner'
#' but depends onto newer less error prone learners. It is also possible to
#' construct XGBoost and EBM learner.
#' @param test_mode [`logical(1L)`] Mode for testing with 100 boosting iterations.
#' @param raw_learner [`logical(1L)`] Return just raw learner without pipeline.
#' @output [`mlr3pipelines::GraphLearner`] Pipeline with learner at last pipe operator
#'   (see above for details).
constructLearner2 = function(lid, ncores = parallel::detectCores() - 2, test_mode = FALSE, raw_learner = FALSE) {
  mstop = 5000L
  if (test_mode) mstop = 100L
  ## Set default hyper parameters (HPs) for all learners:

  cwb_pars = list(
    patience        = 10L,
    mstop           = mstop,
    oob_seed        = 1618,
    eps_for_break   = 0.00001,
    use_stopper     = FALSE,
    just_log        = TRUE,
    use_stopper_auc = TRUE,
    just_log_auc    = TRUE,
    oob_fraction    = 0.3,
    ncores          = max(ncores, 1))

  ## Helper to merge custom set HPs with default values:
  updatePars = function(lrn, pars) {
    lvalues = lrn$param_set$values
    return(mlr3misc::insert_named(lvalues, cwb_pars))
  }

  ## Robustify pipeline put in front of the learner:
  robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")

  ## Switch to get learner based on learner id (lid). Each learner is first defined with custom HPs
  ## which are then merged with the defaults defined above:
  lout = switch(lid,
    ## CWB no binning
    bin_cwb_nb = {
      l = lrn("classif.CWB", id = "bin_cwb_nb", predict_type = "prob",
        learning_rate = 0.1, df_autoselect = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## CWB binning
    bin_cwb_b = {
      l = lrn("classif.CWB", id = "bin_cwb_b", predict_type = "prob",
        learning_rate = 0.1, df_autoselect = TRUE, bin_root = 2L)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## CWB v2 no binning
    acc_cwb = {
      l = lrn("classif.CWB", id = "acc_cwb", predict_type = "prob",
        learning_rate = 0.1, df_autoselect = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## ACWB no binning
    acc_acwb = {
      l = lrn("classif.CWB", id = "acc_acwb", predict_type = "prob",
        learning_rate = 0.1, momentum = 0.0034, df_autoselect = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## HCWB no binning
    acc_hcwb = {
      l = lrn("classif.HCWB", id = "acc_hcwb", predict_type = "prob",
        learning_rate = 0.1, momentum = 0.03, df_autoselect = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    acc_hcwb2 = {
      l = lrn("classif.HCWB", id = "acc_hcwb", predict_type = "prob",
        learning_rate = 0.1, momentum = 0.0034, df_autoselect = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    xgboost = {
      l = po("encode", method = "one-hot") %>>%
        lrn("classif.xgboost", id = "xgboost", predict_type = "prob", nthread = ncores)
      l
    },
    ebm = {
      l = lrn("classif.interpretML_reticulate", id = "ps_interpretML",
        predict_type = "prob", n_jobs = ncores)
      l
    }
  )
  if (is.null(lout))
    stop("'lid' is not valid, use one of 'bin_cwb_nb', 'bin_cwb_n', 'acc_cwb', 'acc_acwb', 'acc_hcwb', 'acc_hcwb2', 'xgboost', or 'ebm'!")
  if (raw_learner) {
    return(lout)
  } else {
    return(GraphLearner$new(robustify %>>% lout))
  }
}

#' Get the log from compboost object
#'
#' This function extracts information about the fitting process for each iteration specified in.
#' @param lrn [`mlr3::Learner`] Trained learner returned from `constructLearner`.
#' @output [`data.frame`] Data frame containing all information..
getCboostLog = function(lrn) {
  mn = names(lrn$model)
  evalCboostFun = function(m, fn)
    eval(parse(text = paste0("lrn$model$", m, "$", fn, "()")))

  out = lapply(mn, function(m) {
    log = evalCboostFun(m, "getLoggerData")
    log = cbind(log,
      blearner = evalCboostFun(m, "getSelectedBaselearner"),
      risk     = evalCboostFun(m, "getInbagRisk")[-1],
      model    = m)

    if (! "oob_risk" %in% names(log))
      log$risk_oob = NA

    return(log)
  })
  lnames = names(out[[1]])
  if (length(out) > 1) {
    for (i in seq_along(out)[-1]) {
      tmax = tail(out[[i-1]]$time, 1)
      out[[i]]$time = out[[i]]$time + tmax
    }
  }
  df_out = do.call(rbind, lapply(out, function(ot) ot[, lnames]))
  df_out$transition = lrn$transition

  return(df_out)
}

#' Get the minimal number of factors
#'
#' @param task [`mlr3::Task`] Task from which the data is used.
#' @output [`integer(1L)`] Minimal number of levels of all factors.
getMinFactor = function(task) {
  factor_cols = task$feature_types$id[task$feature_types$type == "factor"]
  df_cat_min = 20L
  if (length(factor_cols) > 0) {
    df_cat_min = min(vapply(
      X = task$data(cols = factor_cols),
      FUN = function(fc) length(unique(fc)),
      FUN.VALUE = integer(1L)
    ))
  }
  return(df_cat_min)
}

#' Construct the search space of learner
#'
#' @param lid [`character(1L)`] The learner id ('cwb', 'xgboost', or 'ebm').
#' @output [`paradox::ParamSet`] Param set/search space of the learner.
constructSearchSpace = function(lid, task) {
  if (grepl("cwb", lid)) lid = "cwb"

  ss = switch(lid,
   cwb = {
     ss = ps(
       df            = p_dbl(lower = 2, upper = 20),
       df_cat        = p_dbl(lower = 2, upper = getMinFactor(task)),
       learning_rate = p_dbl(lower = 0.001, 0.5),
       mstop         = p_int(lower = MSTOP_MIN, upper = MSTOP_MAX, tags = "budget"))
     ss
   },
   xgboost = {
     ss = ps(
       xgboost.eta               = p_dbl(lower = 0.001, upper = 0.2),
       xgboost.gamma             = p_dbl(lower = -7, upper = 6),
       xgboost.max_depth         = p_int(lower = 3, upper = 20),
       xgboost.colsample_bytree  = p_dbl(lower = 0.5, upper = 1),
       xgboost.colsample_bylevel = p_dbl(lower = 0.5, upper = 1),
       xgboost.lambda            = p_dbl(lower = -10, upper = 10),
       xgboost.alpha             = p_dbl(lower = -10, upper = 10),
       xgboost.subsample         = p_dbl(lower = 0.5, upper = 1),
       xgboost.nrounds           = p_int(lower = MSTOP_MIN, upper = MSTOP_MAX, tags = "budget"))

     ss$trafo = function(x, param_set) {
       idx_gamma = grep("gamma", names(x))
       x[[idx_gamma]] = 2^(x[[idx_gamma]])
       idx_lambda = grep("lambda", names(x))
       x[[idx_lambda]] = 2^(x[[idx_lambda]])
       idx_alpha = grep("alpha", names(x))
       x[[idx_alpha]] = 2^(x[[idx_alpha]])
       x
     }
     ss
    },
    ebm = {
      ss = ps(
        leanring_rate = p_dbl(lower = 0.001, upper = 0.5),
        max_rounds    = p_int(lower = MSTOP_MIN, upper = MSTOP_MAX, tags = "budget"))
      ss
    }
  )
  return(ss)
}
