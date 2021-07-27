library(batchtools)
library(mlr3)

base_dir = here::here()
bm_dir   = paste0(base_dir, "/eq1/batchtools/")

loadRegistry(file.dir = bm_dir, work.dir = paste0(base_dir, "/eq1"))

jids = findDone()
jt   = getJobTable(jids)
res = reduceResultsList(jt$job.id)

#res = reduceResultsList(setdiff(findDone()$job.id, 76))
getStatus()


library(ggplot2)
library(dplyr)
library(tidyr)

for (i in seq_along(res)) {
  res[[i]] = cbind(res[[i]],
    fold = as.numeric(gsub("\\D", "", jt$problem[i])),
    task = sub("\\-.*", "", jt$problem[i]),
    learner = jt$algo.pars[[i]]$lid)
  #res[[i]] = cbind(res[[i]], fold = i)
}
df_res = do.call(rbind, lapply(res, function(r) {
  lnames = c("_iterations", "riskauc", "time", "blearner", "risk", "model", "transition", "fold", "task", "learner")
  r[, lnames]
}))
df_res[["iteration"]]    = df_res[["_iterations"]]
df_res[["_iterations"]]  = NULL
df_res[["classif.auc"]]  = df_res[["riskauc"]]
df_res[["riskauc"]]      = NULL
df_res[["microseconds"]] = df_res[["time"]]


## SUMMARY VALUES:
## =============================================

df_smry = df_res %>%
  group_by(task, learner, iteration) %>%
  summarize(classif.auc = mean(classif.auc),
    microseconds = mean(microseconds),
    transition = transition[1]) %>%
  group_by(task, learner) %>%
  summarize(
    auc_start  = min(classif.auc),
    iter_best  = iteration[which.max(classif.auc)],
    auc_best   = max(classif.auc),
    ms_best    = microseconds[which.max(classif.auc)],
    transition = transition[1],
    auc_expl   = (max(classif.auc) - min(classif.auc)) / min(classif.auc)
  ) %>%
  group_by(task) %>%
  mutate(auc_start = min(auc_start)) %>%
  group_by(task) %>%
  mutate(auc_diff_to_cwb = auc_best[learner == "bin_cwb_nb"] - auc_best)

## FITTING TRACES:
## =============================================

df_aggr = df_res %>%
  group_by(task, learner, iteration) %>%
  summarize(classif.auc = mean(classif.auc), microseconds = mean(microseconds))

sf  = function(x, p = 10) 1 - (1 - (0.2 * x)^p)^(1/p)
sfi = function(x, p = 10) 5 * (1 - (1 - x)^p)^(1/p)
sfb = function(x, p = 10) {
  bl = sf(x[1], p)
  bu = sf(x[2], p)
  sfi(seq(bl, bu, length.out = 4L))
}
stretch_trans = function() scales::trans_new("stretch", sf, sfi, sfb, )
formatFun = function(x) sprintf("%.3f", x)

gg_bmr =

ggplot(data = df_res, mapping = aes(x = microseconds / 1e6, y = classif.auc, color = learner)) +
  #geom_line(aes(group = paste0(learner, fold)), alpha = 0.2, size = 0.6) +
  geom_line(data = df_aggr %>% filter(learner != "acc_cwb"), size = 1.2) +
  #tidyquant::geom_ma(data = df_aggr %>% filter(learner != "acc_cwb"), size = 1.2, n = 3) +
  geom_point(data = df_smry %>% filter(learner != "acc_cwb"), mapping = aes(x = 0, y = auc_best)) +
  geom_point(data = df_smry %>% filter(learner != "acc_cwb"), mapping = aes(x = ms_best / 1e6, y = auc_start)) +
  scale_y_continuous(trans = "stretch", labels = formatFun) +
  xlab("Seconds") +
  ylab("AUC") +
  ggsci::scale_color_uchicago() +
  facet_wrap(. ~ task, nrow = 2, scales = "free")


a = df_res %>%
  filter(grepl("359994", task), learner == "acc_hcwb")

#ggplot(a, aes(x = microseconds, y = classif.auc, color = as.character(fold))) +
ggplot(a, aes(x = iteration, y = classif.auc, color = as.character(fold))) +
  geom_line() +
  geom_vline(xintercept = unique(a$transition))


## INDIVIDUAL INSPECTION
## ------------------------------------------

robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")


tname = "189866"

task = tsk("oml", task_id = 168908)

#task = TASKS[[tname]]
task_new = robustify$train(task)[[1]]

rr = RESAMPLE_SETS[[tname]]
ttrain = task_new$clone(deep = TRUE)$filter(rr$train_set(2))
ttest  = task_new$clone(deep = TRUE)$filter(rr$test_set(2))

tl1 = constructLearner("acc_hcwb", ncores = 4L, test_mode = FALSE, raw_learner = TRUE)
#tl1$param_set$values$momentum = 0.001
tl1$param_set$values$learning_rate = 0.1
tl1$param_set$values$task_extra_log = ttest
tl1$param_set$values$log_auc = TRUE
tl1$param_set$values$mstop = 100
tl1$train(ttrain)

log = getCboostLog(tl1)
library(ggplot2)
ggplot(data = log) +
  geom_line(aes(x = time / 1e6, y = oob_risk, color = "Val Risk")) +
  geom_line(aes(x = time / 1e6, y = risk, color = "Inbag Risk")) +
  geom_line(aes(x = time / 1e6, y = riskauc, color = "Test AUC"))


tl1$model$cboost$getInbagRisk()
tl1$model$cboost_restart$getInbagRisk()





robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")


tname = "359994"

task = TASKS[[tname]]
task_new = robustify$train(task)[[1]]

rr = RESAMPLE_SETS[[tname]]
ttrain = task_new$clone(deep = TRUE)$filter(rr$train_set(1))
ttest  = task_new$clone(deep = TRUE)$filter(rr$test_set(1))

aucLoss = function(truth, response) return(mlr::measureAUC(response, truth, negative = -1, positive = 1) * length(truth))
aucGrad = function(truth, response) return(rep(0, length(truth)))
aucInit = function(truth) {
  p = mean(truth == 1)
  return(0.5 * p / (1 - p))
}
my_auc_loss = LossCustom$new(aucLoss, aucGrad, aucInit)
additional_risk_log = list(auc = list(data = ttest$data(), loss = my_auc_loss))

set.seed(1618)
cboost = compboost::boostSplines(
  data          = ttrain$data(),
  target        = ttrain$target_names,
  #data          = iris,
  #target        = "Sepal.Width",
  iterations    = 80,
  optimizer     = compboost::OptimizerAGBM$new(0.03),
  #optimizer     = compboost::OptimizerCoordinateDescent$new(),
  loss          = compboost::LossBinomial$new(),
  #loss          = LossQuadratic$new(),
  oob_fraction  = 0.3,
  learning_rate = 0.01,
  additional_risk_logs = additional_risk_log)

cboost$train(50)

set.seed(1618)
cboost2 = compboost::boostSplines(
  data          = ttrain$data(),
  target        = ttrain$target_names,
  #data          = iris,
  #target        = "Sepal.Width",
  iterations    = 50,
  optimizer     = compboost::OptimizerAGBM$new(0.03),
  #optimizer     = compboost::OptimizerCoordinateDescent$new(),
  loss          = compboost::LossBinomial$new(),
  #loss          = LossQuadratic$new(),
  oob_fraction  = 0.3,
  learning_rate = 0.01,
  additional_risk_logs = additional_risk_log)

all.equal(cboost$data, cboost2$data)

cbind(cboost$getSelectedBaselearner(), cboost2$getSelectedBaselearner())
cbind(cboost$optimizer$getSelectedMomentumBaselearner()[seq_len(50)], cboost2$optimizer$getSelectedMomentumBaselearner())

table(cboost$optimizer$getSelectedMomentumBaselearner()[seq_len(50)])
table(cboost2$optimizer$getSelectedMomentumBaselearner())


cbind(cboost$predict(), cboost2$predict(), cboost$predict(cboost$data), cboost2$predict(cboost$data))

cf1 = cboost$getEstimatedCoef()
cf2 = cboost2$getEstimatedCoef()
str(cf1)
str(cf2)
