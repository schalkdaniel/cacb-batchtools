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
    ts = robustiry$train(TASKS[[i]])[[1]]$clone(deep = TRUE)

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
  learner    = constructLearner(lid, raw_learner = TRUE)
  lerner0    = constructLearner(lid, raw_learner = TRUE)

  task_train = task$clone(deep = TRUE)$filter(resampling$train_set(1L))
  task_test  = task$clone(deep = TRUE)$filter(resampling$test_set(1L))

  ## Add AUC logger to track test AUC during training:
  aucLoss = function(truth, response)
    return(mlr::measureAUC(response, truth, negative = -1, positive = 1) * length(truth))
  my_auc_loss = LossCustom$new(aucLoss, function(t, r) rep(0, length(t)), function(t) return(0))
  learner$additional_risk_log = list(auc = list(data = task_test$data(), loss = my_auc_loss))

  learner$train(task_train)
  learner0$train(task_train)

  #auc_trace = getCboostMsrsTrace(learner, list(train = task_train, test = task_test),
    #SCORE_MEASURES, iters = seq(4, 5000, by = 4))

  log  = getCboostLog(learner)
  log0 = getCboostLog(learner0)

  log$time = log0$time

  return(log)
})

addExperiments(algo.design = list('evaluate-learner' = data.table(lid = LEARNER_IDS)))

