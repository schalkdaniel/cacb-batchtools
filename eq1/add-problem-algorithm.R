## Add problems and algorithms based on the design
## ===========================================================

extractTaskId = function(tid) {
  if (grepl("Task", tid)) {
    tidn = strsplit(tid, ": ", fixed = TRUE)[[1]][[2]]
    tidn = strsplit(tidn, " (", fixed = TRUE)[[1]][[1]]
  } else {
    tidn = tid
  }
  return(gsub(" ", "", tidn))
}

for (i in seq_len(nrow(design))) {
  resampling = design$resampling[[i]]
  for (k in seq_len(resampling$iters)) {
    ts = design$task[[i]]$clone(deep = TRUE)
    rcustom = rsmp("custom")
    rcustom$instantiate(ts,
      train = list(resampling$train_set(k)),
      test  = list(resampling$test_set(k))
    )
    prob = list(
      task       = ts$clone(deep = TRUE),
      learner    = design$learner[[i]]$clone(deep = TRUE),
      resampling = rcustom
    )
    id = paste0(extractTaskId(design$task[[i]]$id), "-", tail(design$learner[[i]]$graph$ids(), 1), "-fold", k)
    addProblem(name = id, data = prob) #, fun = function(job, data) return(data))
  }
}

addAlgorithm(name = "evaluate-learner", fun = function(job, data, instance) {
  task       = instance$task$clone(deep = TRUE)
  learner    = instance$learner$clone(deep = TRUE)
  resampling = instance$resampling$clone(deep = TRUE)

  task_train = task$clone(deep = TRUE)$filter(resampling$train_set(1L))
  task_test  = task$clone(deep = TRUE)$filter(resampling$test_set(1L))

  learner$train(task_train)
  auc_trace = getCboostMsrsTrace(learner, list(train = task_train, test = task_test), score_measures,
    iters = seq(4, 5000, by = 4))

  return(auc_trace)
})


