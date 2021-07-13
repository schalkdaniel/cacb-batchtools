suppressMessages(library(mlr3))
suppressMessages(library(mlr3tuning))
suppressMessages(library(mlrintermbo))
suppressMessages(library(mlr3learners))
suppressMessages(library(mlr3extralearners))
suppressMessages(library(mlr3pipelines))
suppressMessages(library(paradox))

bm_dir = paste0(here::here(), "/eq1/")

source(paste0(bm_dir, "classifCompboost.R"))
source(paste0(bm_dir, "setup.R"))
source(paste0(bm_dir, "learner.R"))

design = data.table(
  task = rep(tasks, times = length(learner)),
  resampling = rep(resamplings_outer, times = length(learner)),
  learner = rep(learner, each = length(tasks))
)

reg = batchtools::makeExperimentRegistry(
  file.dir = "eq1-benchmark",
  packages = c("mlr3", "mlr3learners", "mlr3extralearners",
    "mlr3pipelines", "mlr3tuning", "compboost", "paradox"),
  source = c("learner.R", "classifCompboost.R", "setup.R"),
  seed = 31415
)
reg$default.resources = list(
  #walltime = 3600L * 2,
  #memory = 1024L * 16L,
  ntasks = 1L,
  ncpus = 1L,
  nodes = 1L,
  clusters = "cm2",
  partition = "cm2_tiny"
)

batchmark(design, reg = reg)#, store_models = TRUE)

getStatus()
