## Used packages
## ============================================

if (FALSE) {
  install.packages(c("processx", "callr", "mlr3", "mlr3tuning", "mlr3learners", "mlr3pipelines",
    "paradox", "xgboost", "ranger", "mboost", "mlr3oml", "reticulate", "mlrMBO",
    "DiceKriging"))
  remotes::install_github("mlr-org/mlr3extralearners")
  remotes::install_github("mb706/mlrintermbo@fixed-initial-design")
  #remotes::install_github("schalkdaniel/compboost", ref = "ba044d3a6f6814080eb097acca2e59fd8bad9805")
  remotes::install_github("schalkdaniel/compboost", ref = "agbm_optim")
}


## Use debug mode
## ============================================

options("mlr3.debug" = TRUE)

## Datasets
## ============================================

tsks_setup = rbind(
  data.frame(type = "mlr-task", id = "spam"),          # Spam
  data.frame(type = "oml-task", id = "7592"),          # Adult
  data.frame(type = "oml-task", id = "168335"),        # MiniBooNE
  data.frame(type = "oml-task", id = "189866"),        # Albert
  data.frame(type = "oml-task", id = "359994"),        # SF Police Incidents
  data.frame(type = "oml-task", id = "9977")           # namao (119 feats, 34465 rows)

  # Additional tasks:

  #data.frame(type = "oml-task", id = "54"),          # Hepatitis
  #data.frame(type = "oml-task", id = "37"),          # Diabetes
  #data.frame(type = "oml-task", id = "4534"),        # Analcat Halloffame
  #data.frame(type = "oml-task", id = "168337"),      # Guillermo
  #data.frame(type = "oml-task", id = "3945"),        # KDDCup09_appetency (231 feats, 50' feats)
  #data.frame(type = "oml-task", id = "168908"),      # Christine (1637 feats, 5418 rows)
  #data.frame(type = "oml-task", id = "168896")       # gina (970 feats, 3153 rows)
)

tasks = getTasks(ids = tsks_setup$id, types = tsks_setup$type)

## Measures
## ============================================

tuning_measure = "classif.auc"
score_measures = c("classif.acc", "classif.auc", "time_train")

## Learners
## ============================================

#learner_names = c(
  #"bm_bin_cwb_nb",
  #"bm_bin_cwb_b",

  #"bm_acc_cwb",
  #"bm_acc_acwb",
  #"bm_acc_hcwb"
#)


## Evaluation
## ============================================

bm_dir = paste0(here::here(), "/eq1/")

#nfolds = 5L
#resample_sets2 = createResampleSets(tasks, seed = 31415L, .key = "cv", folds = nfolds)
#save(resample_sets, file = paste0(bm_dir, "meta/resample-sets.Rda"))

load(paste0(bm_dir, "meta/resample-sets.Rda"))
resamplings_outer = getResampleInstances(tasks, resample_sets)


