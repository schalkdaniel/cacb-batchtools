BM_DIR         = paste0(here::here(), "/eq2/")
BATCHTOOLS_DIR = paste0(BM_DIR, "batchtools")

if (FALSE) unlink(BATCHTOOLS_DIR, recursive = TRUE)

if (! dir.exists(BATCHTOOLS_DIR)) {
  suppressMessages(library(data.table))
  suppressMessages(library(R6))
  suppressMessages(library(mlr3))
  suppressMessages(library(mlr3tuning))
  suppressMessages(library(mlrintermbo))
  suppressMessages(library(mlr3learners))
  suppressMessages(library(mlr3extralearners))
  suppressMessages(library(mlr3pipelines))
  suppressMessages(library(paradox))
  suppressMessages(library(xgboost))


  source(paste0(BM_DIR, "helper.R"))
  source(paste0(BM_DIR, "setup.R"))
  temp = lapply(FILES, function(f) source(paste0(BM_DIR, f)))
}


## Batchtools
## ===========================================================

library(batchtools)

if (dir.exists(BATCHTOOLS_DIR)) {

  loadRegistry(BATCHTOOLS_DIR, writeable = TRUE, work.dir = BM_DIR)
  #loadRegistry(BATCHTOOLS_DIR, work.dir = BM_DIR)

  #jt = getJobTable()

  #ids_resubmit = jt$job.id[unlist(jt$algo.pars) == "acc_hcwb"]
  #ids_resubmit = intersect(ids_resubmit, jt$job.id[grepl("spam", jt$problem)])
  #ids_resubmit = c(ids_resubmit, 61)

  #hcwb_resubmit = jt$job.id[unlist(jt$algo.pars) == "acc_hcwb"]
  #hcwb_resubmit = intersect(seq_len(75L), hcwb_resubmit)

  #not_done = setdiff(seq_len(100L), findDone()$job.id)
  #not_done = unique(c(not_done, ids_resubmit))

  submitJobs(121:149)
  submitJobs(findNotDone())
} else {

  reg = makeExperimentRegistry(
    file.dir = BATCHTOOLS_DIR,
    packages = c("data.table", "R6", "mlr3", "mlr3learners", "mlr3extralearners",
      "mlr3pipelines", "mlr3tuning", "compboost", "paradox", "mlr3hyperband", "reticulate"),
    #source = c("helper.R", "classifCompboost.R", "setup.R"),
    source   = FILES,
    seed     = 31415)

  #reg = getDefaultRegistry()

  # reg$cluster.functions = makeClusterFunctionsSSH(workers = list(
  #   Worker$new("localhost", ncpus = 1L), # 192.168.9.131
  #   #Worker$new("192.168.9.132", ncpus = 1L),
  #   Worker$new("192.168.9.133", ncpus = 1L)))

  reg$cluster.functions = makeClusterFunctionsInteractive(external = TRUE)
  reg$default.resources = list(
    #walltime = 3600L * 2,
    #memory = 1024L * 16L,
    max.concurrent.jobs = 1L,
    ntasks = 1L,
    ncpus = 1L,
    nodes = 1L
  )

  saveRegistry(reg)

  source(paste0(BM_DIR, "add-experiments.R"))
  submitJobs()
}


if (FALSE) {
  # cpuserver3:
  submitJobs(findNotDone()[1:75,])
  # cpuserver5:
  submitJobs(findNotDone()[76,150])
}


### Code for testing:
if (FALSE) {

l = constructLearner2("ebm", raw_learner = TRUE)
l$param_set


BM_DIR         = paste0(here::here(), "/eq1/")
BATCHTOOLS_DIR = paste0(BM_DIR, "batchtools")

suppressMessages(library(data.table))
suppressMessages(library(R6))
suppressMessages(library(mlr3))
suppressMessages(library(mlr3tuning))
suppressMessages(library(mlrintermbo))
suppressMessages(library(mlr3learners))
suppressMessages(library(mlr3extralearners))
suppressMessages(library(mlr3pipelines))
suppressMessages(library(paradox))

source(paste0(BM_DIR, "classifCompboost.R"))
source(paste0(BM_DIR, "helper.R"))
source(paste0(BM_DIR, "setup.R"))

tl = constructLearner("bin_cwb_nb", ncores = 30L, test_mode = TRUE, raw_learner = FALSE)
tl$train(TASKS[[2]])

tasks = list(train = TASKS[[2]], test = TASKS[[2]])
cbt = getCboostMsrsTrace(tl, tasks, SCORE_MEASURES, iters = c(10, 20, 90, 100, 200))

p1 = tl$predict(TASKS[[5]])

library(compboost)
cboost = boostSplines(data = TASKS[[5]]$data(), target = TASKS[[5]]$target_names,
  loss = LossBinomial$new(), iterations = 100L)

lcboost = lrn("classif.compboost", mstop = 50L)
lcboost$train(TASKS[[5]])

tl$setToIteration(1)
p2 = tl$predict(design$task[[1]])

p1$score(msr("classif.auc"))
p2$score(msr("classif.auc"))

clog = tl$model$cboost$getLoggerData()
tl$model$cboost_restart$getLoggerData()

library(compboost)
library(mlr3)
library(mlr3oml)

ts = tsk("oml", task_id = 359994)
cboost = boostSplines(data = ts$data(), target = ts$target_names, loss = LossBinomial$new(), iterations = 125L)

l = lrn("classif.compboost")
l$train(ts)
}
