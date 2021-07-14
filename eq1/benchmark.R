suppressMessages(library(data.table))
suppressMessages(library(R6))
suppressMessages(library(mlr3))
suppressMessages(library(mlr3tuning))
suppressMessages(library(mlrintermbo))
suppressMessages(library(mlr3learners))
suppressMessages(library(mlr3extralearners))
suppressMessages(library(mlr3pipelines))
suppressMessages(library(paradox))

bm_dir = paste0(here::here(), "/eq1/")

source(paste0(bm_dir, "classifCompboost.R"))
source(paste0(bm_dir, "helper.R"))
source(paste0(bm_dir, "setup.R"))
source(paste0(bm_dir, "learner.R"))

design = data.table(
  task = rep(tasks, times = length(learners)),
  resampling = rep(resamplings_outer, times = length(learners)),
  learner = rep(learners, each = length(tasks))
)
design = design[1:4]

batchtools_dir = paste0(bm_dir, "batchtools")
unlink(batchtools_dir, recursive = TRUE)
reg = batchtools::makeExperimentRegistry(
  file.dir = batchtools_dir,
  packages = c("data.table", "R6", "mlr3", "mlr3learners", "mlr3extralearners",
    "mlr3pipelines", "mlr3tuning", "compboost", "paradox"),
  source = paste0(bm_dir, c("helper.R", "classifCompboost.R", "learner.R", "setup.R")),
  seed   = 31415)

reg$default.resources = list(
  #walltime = 3600L * 2,
  #memory = 1024L * 16L,
  max.concurrent.jobs = 1L,
  ntasks = 1L,
  ncpus = 1L,
  nodes = 1L
)

batchtools::makeClusterFunctionsSSH(workers = list(
  Worker$new("localhost", ncpus = 1L),
  Worker$new("", ncpus = 1L),
  Worker$new("", ncpus = 1L)))

## Add problems and algorithms based on the design
## ===========================================================

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
    id = paste0(design$task[[i]]$id, "-", tail(design$learner[[1]]$graph$ids(), 1), "-fold", k)
    addProblem(name = id, data = prob) #, fun = function(job, data) return(data))
  }
}

addAlgorithm(name = "evaluate-learner", fun = function(job, data, instance) {
  instance = prob

  task       = instance$task$clone(deep = TRUE)
  learner    = instance$learner$clone(deep = TRUE)
  resampling = instance$resampling$clone(deep = TRUE)

  task_train = task$clone(deep = TRUE)$filter(resampling$train_set(1L))
  task_test  = task$clone(deep = TRUE)$filter(resampling$test_set(1L))

  learner$train(task_train)
  auc_trace = getCboostMsrsTrace(learner, list(train = task_train, test = task_test), score_measures)

  return(auc_trace)
})


if (FALSE) {
library(ggplot2)

ggplot(auc_trace, aes(x = microseconds, y = classif.auc, color = tset)) +
  geom_line() +
  geom_vline(xintercept = auc_trace$microseconds[auc_trace$iteration == auc_trace$transition[1]][1])


#batchtools::submitJobs()
#batchtools::getStatus()

tl = lrn_acc_hcwb$clone(deep = TRUE)
tl$train(design$task[[1]])
p1 = tl$predict(design$task[[1]])

tl$setToIteration(1)
p2 = tl$predict(design$task[[1]])

p1$score(msr("classif.auc"))
p2$score(msr("classif.auc"))

clog = tl$model$cboost$getLoggerData()
tl$model$cboost_restart$getLoggerData()
}
