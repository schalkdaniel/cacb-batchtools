library(batchtools)

extractString = function(string, split, lor = "left") {
  sp  = strsplit(string, split, fixed = TRUE)
  idx = 1
  if (lor == "right") idx = 2
  return(sp[[1]][idx])
}

SERVER = "322"
FNAME  = paste0("df_res", SERVER, ".Rda")

BM_DIR         = here::here("eq2/")
BATCHTOOLS_DIR = paste0(BM_DIR, "batchtools")

loadRegistry(BATCHTOOLS_DIR, work.dir = BM_DIR)

ids_done = findDone()$job.id
#ids_done = ids_done[1:5]
if (file.exists(FNAME)) {
  load(FNAME)
  ids_done = setdiff(ids_done, df_res$job_id)
}


if (length(ids_done) > 0) {

  if (exists("df_res")) df_res_old = df_res

  jt = getJobTable()
  jt$learner = unlist(jt$algo.pars)

  ll = list()

  ### Process id by id:
  for (id in ids_done) {
    message("[", Sys.time(), "] Processing ", which(id == ids_done), "/", length(ids_done))
    id_idx = which(jt$job.id == id)
    #res = reduceResultsList(id)
    res = readRDS(file = paste0(BM_DIR, "/batchtools/results/", id, ".rds"))

    narcv = res$archive$n_evals

    auc_best = res$tuning_results$classif.auc
    ttrain   = 0
    tpred    = 0

    for (i in seq_len(narcv)) {
      lrns = res$archive$learners(i)
      for (j in seq_along(lrns)) {
        tt = lrns[[j]]$state[c("train_time", "predict_time")]
        ttrain = ttrain + tt$train_time
        tpred  = tpred + tt$predict_time
      }
    }

    if ("xgboost.output" %in% names(res$test_pred))
      auc_test = res$test_pred$xgboost.output$score(mlr3::msr("classif.auc"))
    else
      auc_test = res$test_pred$score(mlr3::msr("classif.auc"))

    if (grepl("cwb", jt$learner[id])) biters = res$tuning_results[["mstop"]]
    if (grepl("xgboost", jt$learner[id])) biters = res$tuning_results[["xgboost.nrounds"]]
    if (grepl("ebm", jt$learner[id])) biters = res$tuning_results[["max.rounds"]]

    ll = c(ll, list(data.frame(
      job_id           = id, 
      auc_val          = auc_best,
      auc_test         = unname(auc_test),
      time_train       = ttrain, 
      time_predict     = tpred,
      time_both        = ttrain + tpred, 
      mstop            = biters,
      job_time_running = as.numeric(jt$time.running[id_idx]),
      job_time_unit    = attr(jt$time.running[id_idx], "units"),
      learner          = jt$learner[id], 
      task             = extractString(jt$problem[id_idx], "-"), 
      fold             = extractString(jt$problem[id_idx], "-fold", "right"))))
  }
  df_res = do.call(rbind, ll)
  if (exists("df_res_old")) df_res = rbind(df_res_old, df_res)
  save(df_res, file = FNAME)
}
