library(batchtools)

extractString = function(string, split, lor = "left") {
  sp  = strsplit(string, split, fixed = TRUE)
  idx = 1
  if (lor == "right") idx = 2
  return(sp[[1]][idx])
}

BM_DIR         = paste0(here::here(), "/eq2/")
BATCHTOOLS_DIR = paste0(BM_DIR, "batchtools")

loadRegistry(BATCHTOOLS_DIR, work.dir = BM_DIR)

ids_done = findDone()$job.id
#ids_done = ids_done[1:5]

jt = getJobTable()
jt$learner = unlist(jt$algo.pars)

ll = list()

### Process id by id:
for (id in ids_done) {
  message("[", Sys.time(), "] Processing ", which(id == ids_done), "/", length(ids_done))
  res = reduceResultsList(id)

  narcv = res[[1]]$archive$n_evals

  auc_best = res[[1]]$tuning_results$classif.auc
  ttrain   = 0
  tpred    = 0

  for (i in seq_len(narcv)) {
    lrns = res[[1]]$archive$learners(i)
    for (j in seq_along(lrns)) {
      tt = lrns[[j]]$state[c("train_time", "predict_time")]
      ttrain = ttrain + tt$train_time
      tpred  = tpred + tt$predict_time
    }
  }

  ll = c(ll, list(data.frame(job_id = id, classif.auc = auc_best, time_train = ttrain, time_predict = tpred,
    time_both = ttrain + tpred, learner = jt$learner[id], task = extractString(jt$problem[id], "-"), 
    fold = extractString(jt$problem[id], "-fold", "right"))))
}

