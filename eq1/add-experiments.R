## Add problems and algorithms based on the design
## ===========================================================

for (i in seq_along(TASKS)) {
  resampling = RESAMPLE_SETS[[i]]

  robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")

  ## Split the resample (5-CV) into all folds. Hence, a
  ## task - resampling combination is split into combinations
  ## task - reampling (iter 1), ..., task - resampling (iter K).
  for (k in seq_len(resampling$iters)) {
    ts = robustify$train(TASKS[[i]])[[1]]$clone(deep = TRUE)

    rcustom = rsmp("custom")
    rcustom$instantiate(ts,
      train = list(resampling$train_set(k)),
      test  = list(resampling$test_set(k))
    )
    prob = list(
      task       = ts$clone(deep = TRUE),
      resampling = rcustom
    )
    id = paste0(names(TASKS)[i], "-fold", k)
    addProblem(name = id, data = prob) #, fun = function(job, data) return(data))
  }
}

addAlgorithm(name = "evaluate-learner", fun = function(job, data, instance, lid) {

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("mlr3")$set_threshold("trace")

  task       = data$task$clone(deep = TRUE)
  resampling = data$resampling$clone(deep = TRUE)

  ## Learner is constructed two times, one for logging and one for the
  ## actual training with time tracking:
  if (grepl("-new", lid)) {
    lid = gsub("-new", "", lid)

    learner    = constructLearner2(lid, raw_learner = TRUE)
    learner0   = constructLearner2(lid, raw_learner = TRUE)

    task_train = task$clone(deep = TRUE)$filter(resampling$train_set(1L))
    task_test  = task$clone(deep = TRUE)$filter(resampling$test_set(1L))

    learner$param_set$values$additional_auc_task = task_test
    learner$param_set$values$use_stopper = FALSE
    learner$param_set$values$use_stopper_auc = TRUE

    learner0$param_set$values$use_stopper = FALSE
    learner0$param_set$values$use_stopper_auc = TRUE

    learner$train(task_train)
    learner0$train(task_train)

  } else {
    learner    = constructLearner(lid, raw_learner = TRUE)
    learner0   = constructLearner(lid, raw_learner = TRUE)

    task_train = task$clone(deep = TRUE)$filter(resampling$train_set(1L))
    task_test  = task$clone(deep = TRUE)$filter(resampling$test_set(1L))

    learner$param_set$values$task_extra_log = task_test
    learner$param_set$values$log_auc = TRUE

    learner$train(task_train)
    learner0$train(task_train)
  }

  log  = getCboostLog(learner)
  log0 = getCboostLog(learner0)

  log$time = log0$time

  return(log)
})

addExperiments(algo.design = list('evaluate-learner' = data.table(lid = LEARNER_IDS)))
#addExperiments(algo.design = list('evaluate-learner' = data.table(lid = "acc_hcwb2")))
addExperiments(algo.design = list('evaluate-learner' = data.table(lid = "acc_hcwb2-new")))

