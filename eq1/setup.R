## Used packages
if (FALSE) {
  install.packages(c("processx", "callr", "mlr3", "mlr3tuning", "mlr3learners", "mlr3pipelines",
    "paradox", "xgboost", "ranger", "mboost", "mlr3oml", "reticulate", "mlrMBO",
    "DiceKriging"))
  remotes::install_github("mlr-org/mlr3extralearners")
  remotes::install_github("mb706/mlrintermbo@fixed-initial-design")
  #remotes::install_github("schalkdaniel/compboost", ref = "ba044d3a6f6814080eb097acca2e59fd8bad9805")
  remotes::install_github("schalkdaniel/compboost", ref = "agbm_optim")
}

## Files to load:
FILES = c(paste0("learner-src/", c("classifCWB.R", "classifHCWB.R", "classifCompboost.R", "learner-helper.R")), "helper.R", "setup.R")

## Rebuild tasks and resampling objects:
REBUILD = FALSE

## Base directory of the benchmark:
BM_DIR = paste0(here::here(), "/eq1/")


## Tasks/Datasets
## ============================================

TSKS_SETUP = rbind(
  data.frame(type = "mlr-task", id = "spam"),          # Spam
  data.frame(type = "oml-task", id = "7592"),          # Adult
  data.frame(type = "oml-task", id = "168335"),        # MiniBooNE
  data.frame(type = "oml-task", id = "189866"),        # Albert
  data.frame(type = "oml-task", id = "168908"),        # Christine (1637 feats, 5418 rows)
  data.frame(type = "oml-task", id = "9977")           # namao (119 feats, 34465 rows)

  # Additional tasks:

  #data.frame(type = "oml-task", id = "359994"),      # SF Police Incidents
  #data.frame(type = "oml-task", id = "54"),          # Hepatitis
  #data.frame(type = "oml-task", id = "37"),          # Diabetes
  #data.frame(type = "oml-task", id = "4534"),        # Analcat Halloffame
  #data.frame(type = "oml-task", id = "168337"),      # Guillermo
  #data.frame(type = "oml-task", id = "3945"),        # KDDCup09_appetency (231 feats, 50' feats)
  #data.frame(type = "oml-task", id = "168896")       # gina (970 feats, 3153 rows)
)
message("[", as.character(Sys.time()), "] Loading tasks")
if (! file.exists(paste0(BM_DIR, "meta/tasks.Rda")) || REBUILD) {
  TASKS = constructTasks(TSKS_SETUP$id, TSKS_SETUP$type)
  save(TASKS, file = paste0(BM_DIR, "meta/tasks.Rda"))
} else {
  ## LOAD TASKS:
  load(paste0(BM_DIR, "meta/tasks.Rda"))
}

## Learners
## ============================================

LEARNER_IDS = c("bin_cwb_nb", "bin_cwb_b", "acc_acwb", "acc_hcwb")
#LEARNER_IDS = c("bin_cwb_nb", "bin_cwb_b", "acc_cwb", "acc_acwb", "acc_hcwb")


## Measures
## ============================================

TUNING_MEASURE = "classif.auc"
SCORE_MEASURES = c("classif.acc", "classif.auc", "time_train")


## Evaluation
## ============================================

message("[", as.character(Sys.time()), "] Loading resampling")
if (! file.exists(paste0(BM_DIR, "meta/resample-sets.Rda")) || REBUILD) {
  RESAMPLE_SETS = createResampleSets(TASKS, seed = 31415L, .key = "cv", folds = 5L)
  save(RESAMPLE_SETS, file = paste0(BM_DIR, "meta/resample-sets.Rda"))
} else {
  ## Load RESAMPLE_SETS:
  load(paste0(BM_DIR, "meta/resample-sets.Rda"))
}
