## Add problems and algorithms based on the design
## ===========================================================

for (i in seq_along(TASKS)) {
  resampling = RESAMPLE_SETS[[i]]
  
  ## Split the resample (5-CV) into all folds. Hence, a
  ## task - resampling combination is split into combinations
  ## task - reampling (iter 1), ..., task - resampling (iter K).
  for (k in seq_len(resampling$iters)) {
    ts = TASKS[[i]]$clone(deep = TRUE)
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
  learner    = constructLearner(lid)

  task_train = task$clone(deep = TRUE)$filter(resampling$train_set(1L))
  task_test  = task$clone(deep = TRUE)$filter(resampling$test_set(1L))

  learner$train(task_train)
  auc_trace = getCboostMsrsTrace(learner, list(train = task_train, test = task_test), 
    score_measures, iters = seq(4, 5000, by = 4))

  return(auc_trace)
})

addExperiments(algo.design = list('evaluate-learner' = data.table(lid = LEARNER_IDS)))

