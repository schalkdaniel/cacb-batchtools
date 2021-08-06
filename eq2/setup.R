## Used packages
if (FALSE) {
  install.packages(c("processx", "callr", "mlr3", "mlr3tuning", "mlr3learners", "mlr3pipelines",
    "paradox", "xgboost", "ranger", "mboost", "mlr3oml", "reticulate", "mlrMBO",
    "DiceKriging", "mlr3hyperband"))
  remotes::install_github("mlr-org/mlr3extralearners")
  remotes::install_github("mb706/mlrintermbo@fixed-initial-design")
  #remotes::install_github("schalkdaniel/compboost", ref = "ba044d3a6f6814080eb097acca2e59fd8bad9805")
  remotes::install_github("schalkdaniel/compboost", ref = "agbm_optim")
}

## Files to load:
FILES = c(paste0("learner-src/", c("classifCWB.R", "classifHCWB.R", "learner-helper.R", "classifInterpretML_reticulate.R")), "helper.R", "setup.R")

## Rebuild tasks and resampling objects:
REBUILD = FALSE

## Base directory of the benchmark:
BM_DIR = paste0(here::here(), "/eq2/")


## Tasks/Datasets
## ============================================

TSKS_SETUP = rbind(
  data.frame(type = "mlr-task", id = "spam"),          # Spam
  data.frame(type = "oml-task", id = "168908"),        # Christine (1637 feats, 5418 rows)
  data.frame(type = "oml-task", id = "7592"),          # Adult
  data.frame(type = "oml-task", id = "168335"),        # MiniBooNE
  data.frame(type = "oml-task", id = "189866"),        # Albert
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

## TUNING:
## ===================================

RESAMPLING_INNER_FOLDS = 3L
HYPERBAND_ETA = 2
MSTOP_MIN = 39L
MSTOP_MAX = 5000L


## - 61.22 Model fits until 5000 iterations
## - 314 Tried parameter configurations

#' @title Hyperband Schedule
#'
#' @description
#' Calculates hyperband schedule.
#'
#' @param r_min (`numeric(1)`)\cr
#' Lower bound of budget parameter.
#' @param r_max (`numeric(1)`)\cr
#' Upper bound of budget parameter.
#' @param eta (`numeric(1)`).
#' @param round (`logical(1)`)\cr
#' Determines if budget is an integer.
#'
#' @return [data.table::data.table()]
#' @export
hyperband_schedule = function(r_min, r_max, eta, round = FALSE) {
  r = r_max / r_min
  s_max = floor(log(r, eta))
  b = (s_max + 1) * r

  map_dtr(s_max:0, function(s) {
    nb = ceiling((b / r) * ((eta^s) / (s + 1)))
    rb = r * eta^(-s)
    map_dtr(0:s, function(i) {
      ni = floor(nb * eta^(-i))
      ri = r_min * rb * eta^i
      if (round) ri = round(ri)
      data.table(bracket = s, stage = i, budget = ri, n = ni)
    })
  })
}
#hs = hyperband_schedule(39, 5000, 2)
#hs
#>     bracket stage  budget   n
#>  1:       7     0   39.06 128
#>  2:       7     1   78.12  64
#>  3:       7     2  156.25  32
#>  4:       7     3  312.50  16
#>  5:       7     4  625.00   8
#>  6:       7     5 1250.00   4
#>  7:       7     6 2500.00   2
#>  8:       7     7 5000.00   1
#>  9:       6     0   78.12  74
#> 10:       6     1  156.25  37
#> 11:       6     2  312.50  18
#> 12:       6     3  625.00   9
#> 13:       6     4 1250.00   4
#> 14:       6     5 2500.00   2
#> 15:       6     6 5000.00   1
#> 16:       5     0  156.25  43
#> 17:       5     1  312.50  21
#> 18:       5     2  625.00  10
#> 19:       5     3 1250.00   5
#> 20:       5     4 2500.00   2
#> 21:       5     5 5000.00   1
#> 22:       4     0  312.50  26
#> 23:       4     1  625.00  13
#> 24:       4     2 1250.00   6
#> 25:       4     3 2500.00   3
#> 26:       4     4 5000.00   1
#> 27:       3     0  625.00  16
#> 28:       3     1 1250.00   8
#> 29:       3     2 2500.00   4
#> 30:       3     3 5000.00   2
#> 31:       2     0 1250.00  11
#> 32:       2     1 2500.00   5
#> 33:       2     2 5000.00   2
#> 34:       1     0 2500.00   8
#> 35:       1     1 5000.00   4
#> 36:       0     0 5000.00   8

#sum(hs$budget * hs$n) / 5000
#> 61.22
#sum(hs$n[hs$stage == 0])
#> 314



