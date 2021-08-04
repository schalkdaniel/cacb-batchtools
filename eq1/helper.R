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
        if (as.character(id) == "9977") {
          ## Problematic columns without information and not cathed by
          ## the pre-processing:
          feats_remove = paste0("V", 81:86)
          ts = ts$select(setdiff(ts$feature_names, feats_remove))
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

    #if (type == "oml-data") {
      #ts = mlr3oml::read_arff("https://www.openml.org/data/download/19335520/file7b53746cbda2.arff")
      #tasks[[as.character(id)]] = TaskClassif$new(id = "albert", backend = ts, target = "class")
    #}

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
        df_autoselect = TRUE, oob_fraction = 0.3, use_stopper = TRUE)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    acc_hcwb2 = {
      l = lrn("classif.compboost", id = "acc_hcwb", predict_type = "prob",
        optimizer = "nesterov", restart = TRUE, learning_rate = 0.1, momentum = 0.003,
        df_autoselect = TRUE, oob_fraction = 0.3, use_stopper = TRUE)
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
constructLearner2 = function(lid, ncores = parallel::detectCores() - 2, test_mode = FALSE, raw_learner = FALSE) {
  mstop = 5000L
  if (test_mode) mstop = 100L
  ## Set default hyper parameters (HPs) for all learners:

  cwb_pars = list(
    patience        = 10L,
    mstop           = mstop,
    oob_seed        = 1618,
    eps_for_break   = 0.00001,
    use_stopper     = TRUE,
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
    cwb = {
      l = lrn("classif.CWB", id = "cwb", predict_type = "prob",
        learning_rate = 0.1)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## CWB binning
    cwb_b = {
      l = lrn("classif.CWB", id = "cwb_b", predict_type = "prob",
        learning_rate = 0.1, bin_root = 2L)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## CWB v2 no binning
    cwb2 = {
      l = lrn("classif.CWB", id = "cwb2", predict_type = "prob",
        learning_rate = 0.1)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## ACWB no binning
    acwb = {
      l = lrn("classif.CWB", id = "acwb", predict_type = "prob",
        learning_rate = 0.1, momentum = 0.0034)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## ACWB binning
    acwb_b = {
      l = lrn("classif.CWB", id = "acwb_b", predict_type = "prob",
        learning_rate = 0.1, momentum = 0.0034, bin_root = 2)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## HCWB no binning
    hcwb = {
      l = lrn("classif.HCWB", id = "hcwb", predict_type = "prob",
        learning_rate = 0.1, momentum = 0.03)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    ## HCWB binning
    hcwb_b = {
      l = lrn("classif.HCWB", id = "hcwb_b", predict_type = "prob",
        learning_rate = 0.1, momentum = 0.03, bin_root = 2L)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    },
    hcwb2 = {
      l = lrn("classif.HCWB", id = "hcwb2", predict_type = "prob",
        learning_rate = 0.1, momentum = 0.0034)
      l$param_set$values = updatePars(l, cwb_pars)
      l
    }
  )
  if (is.null(lout))
    stop("'lid' is not valid, use one of 'cwb', 'cwb_b', 'cwb2', 'acwb', 'acwb_b', 'hcwb', 'hcwb_b', or 'hcwb2'!")
  if (raw_learner) {
    return(lout)
  } else {
    return(GraphLearner$new(robustify %>>% lout))
  }
}

#' Get information about the fitting process
#'
#' This function extracts information about the fitting process for each iteration specified in
#' `iters`. The information extracted are the measures specified in `score_measures` (default is
#' `"classif.auc"`), the microseconds the model needs to train until the specific iteration, the
#' train risk, and (if present) the validation risk.
#' @param lrn [`mlr3pipelines::GraphLearner`] Trained graph learner returned from `constructLearner`. Note
#'   that just pipelines are valid with a learner at the end with an id containing `cwb`.
#' @param tasks [`list(mlr3::Task)`] List of tasks, the measures are extracted for each task in the list.
#'   This can be used, e.g., to pass a train and test task and get the performance measures for both.
#' @param score_measures [`character()`] Measure passed to `msrs` on each element of `tasks`.
#' @param iters [`integer()`] Vector containing the iteration for which the information are extracted.
#' @output [`data.frame`] Data frame containing all information. The rows are the iterations and tasks per
#'   iteration, so "nrow = length(iters) * length(tasks)".
getCboostMsrsTrace = function(lrn, tasks, score_measures = "classif.auc", iters = NULL) {
  mstop = lrn$param_set$values
  mstop = unlist(mstop[grepl("mstop", names(mstop))])
  lids  = lrn$graph$ids()
  lid   = lids[grepl("cwb", lids)]

  if (is.null(iters[1])) iters = seq_len(mstop)
  if (max(iters) > mstop) {
    warning("Cannot use iters > trained iterations (", mstop, "). Remove all iters > trained iterations.")
    iters = iters[iters <= mstop]
  }
  iters = sort(iters)

  out = list()
  clog = lrn$model[[lid]]$model$cboost$getLoggerData()
  if (! "oob_risk" %in% names(clog))
    clog$oob_risk = NA

  ## Get logs of the learner, containing the training time and train + test risk.
  clog = cbind(clog, risk = lrn$model[[lid]]$model$cboost$getInbagRisk()[-1])
  if ("cboost_restart" %in% names(lrn$model[[lid]]$model)) {
    clog_restart = lrn$model[[lid]]$model$cboost_restart$getLoggerData()
    clog_restart = cbind(clog_restart,
      oob_risk = NA,
      risk = lrn$model[[lid]]$model$cboost_restart$getInbagRisk()[-1])
    clog_restart$time = clog_restart$time + max(clog$time)
    cnames = c("_iterations", "risk", "oob_risk", "time")
    clog = rbind(clog[, cnames], clog_restart[, cnames])
  }

  ## Get transition from, e.g., CWB to ACWB
  transition = lrn$graph$pipeops[[lid]]$learner$transition
  adjust_entered_restart = TRUE
  for (i in seq_along(iters)) {
    message("[", as.character(Sys.time()), "] (", i, "/", length(iters), ") Processing iter ", iters[i])
    ## Check if transition already took place. If so, we have to reset the iteration of the restarted model:
    if (transition < iters[i]) {
      if (adjust_entered_restart) {
        lrn$model[[lid]]$model$cboost$train(transition)
        adjust_entered_restart = FALSE
      }
      if ("cboost_restart" %in% names(lrn$model[[lid]]$model))
        lrn$model[[lid]]$model$cboost_restart$train(iters[i] - transition)
    } else {
      lrn$model[[lid]]$model$cboost$train(iters[i])
    }
    lrn$graph$pipeops[[lid]]$learner$iter = iters[i]

    tl = data.frame()
    for (tn in names(tasks)) {
      pred = lrn$predict(tasks[[tn]])
      scrs = pred$score(msrs(score_measures))
      scrs = do.call(data.frame, c(lapply(scrs, function(x) x), task = tasks[[tn]]$id,
        learner = lid, iteration = iters[i], tset = tn, microseconds = clog$time[iters[i]],
        risk_oob = clog$oob_risk[iters[i]], risk = clog$risk[iters[i]], transition = transition))
      tl = rbind(tl, scrs)
    }
    out[[i]] = tl
  }
  return(do.call(rbind, out))
}


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
