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
batchtools_dir = paste0(bm_dir, "batchtools")

if (! dir.exists(batchtools_dir)) {
  source(paste0(bm_dir, "classifCompboost.R"))
  source(paste0(bm_dir, "helper.R"))
  source(paste0(bm_dir, "setup.R"))
  source(paste0(bm_dir, "learner.R"))

  design = data.table(
    task = rep(tasks, times = length(learners)),
    resampling = rep(resamplings_outer, times = length(learners)),
    learner = rep(learners, each = length(tasks))
  )
}


## Batchtools
## ===========================================================

library(batchtools)

if (FALSE) unlink(batchtools_dir, recursive = TRUE)
if (dir.exists(batchtools_dir)) {
  loadRegistry(batchtools_dir, writeable = TRUE, work.dir = bm_dir)
  submitJobs(findNotDone())
} else {
  reg = batchtools::makeExperimentRegistry(
    file.dir = batchtools_dir,
    packages = c("data.table", "R6", "mlr3", "mlr3learners", "mlr3extralearners",
      "mlr3pipelines", "mlr3tuning", "compboost", "paradox"),
    reg$source = c("helper.R", "classifCompboost.R", "learner.R", "setup.R")
    seed   = 31415)

  reg$cluster.functions = makeClusterFunctionsSSH(workers = list(
    Worker$new("localhost", ncpus = 1L), # 192.168.9.131
    Worker$new("192.168.9.132", ncpus = 1L),
    Worker$new("192.168.9.133", ncpus = 1L)))

  saveRegistry(reg)

  source(paste0(bm_dir, "add-problem-algorithm.R"))
  batchtools::addExperiments()
}

#reg$default.resources = list(
#  #walltime = 3600L * 2,
#  #memory = 1024L * 16L,
#  max.concurrent.jobs = 1L,
#  ntasks = 1L,
#  ncpus = 1L,
#  nodes = 1L
#)



#submitJobs(findNotDone())
#getStatus()





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
