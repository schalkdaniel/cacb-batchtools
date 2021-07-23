library(batchtools)
library(mlr3)

base_dir = here::here()
bm_dir   = paste0(base_dir, "/eq1/batchtools/")

loadRegistry(file.dir = bm_dir, work.dir = paste0(base_dir, "/eq1"))
res = reduceResultsList(setdiff(findDone()$job.id, 76))
#res = reduceResultsList()
getStatus()


library(ggplot2)
library(dplyr)
library(tidyr)

for (i in seq_along(res)) {
  res[[i]] = cbind(res[[i]], fold = i)
}
df_res = do.call(rbind, res)


## SUMMARY VALUES:
## =============================================

df_smry = df_res %>%
  group_by(task, learner, iteration, tset) %>%
  summarize(classif.auc = mean(classif.auc),
    microseconds = mean(microseconds),
    transition = transition[1]) %>%
  group_by(task, learner, tset) %>%
  summarize(
    auc_start  = min(classif.auc),
    iter_best  = iteration[which.max(classif.auc)],
    auc_best   = max(classif.auc),
    ms_best    = microseconds[which.max(classif.auc)],
    transition = transition[1],
    auc_expl   = (max(classif.auc) - min(classif.auc)) / min(classif.auc)
  ) %>%
  group_by(task, tset) %>%
  mutate(auc_start = min(auc_start)) %>%
  group_by(task, tset) %>%
  mutate(auc_diff_to_cwb = auc_best[learner == "bin_cwb_nb"] - auc_best) %>%
  filter(tset == "test")

## FITTING TRACES:
## =============================================

df_aggr = df_res %>%
  group_by(task, learner, iteration, tset) %>%
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

gg_bmr = ggplot(data = df_res %>% filter(tset == "test"), mapping = aes(x = microseconds / 1e6, y = classif.auc, color = learner)) +
  geom_line(aes(group = paste0(learner, fold)), alpha = 0.2, size = 0.6) +
  geom_line(data = df_aggr %>% filter(tset == "test"), size = 1.2) +
  geom_point(data = df_smry, mapping = aes(x = 0, y = auc_best)) +
  geom_point(data = df_smry, mapping = aes(x = ms_best / 1e6, y = auc_start)) +
  scale_y_continuous(trans = "stretch", labels = formatFun) +
  xlab("Seconds") +
  ylab("AUC") +
  ggsci::scale_color_uchicago() +
  facet_wrap(. ~ task, nrow = 2, scales = "free")


a = df_res %>%
  filter(grepl("albert", task), learner == "acc_hcwb", tset == "test")

ggplot(a, aes(x = microseconds, y = risk, color = as.character(fold))) +
  geom_line()

plot(x, 1 - (1 - x^50)^(1/60), type = "l")

x = seq(0.5, 1, by = 0.01)
y = x
plot(x, y^8, type = "l")

ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_line() +
  scale_y_continuous(trans = "sqrt")


devtools::load_all("~/repos/compboost")

dat = mlr3::tsk("spam")$data()[1:3000,]
target = mlr3::tsk("spam")$target_names

set.seed(1618)
cboost = compboost::boostSplines(
  data          = dat,
  target        = target,
  iterations    = 5000,
  optimizer     = compboost::OptimizerAGBM$new(0.03),
  loss          = compboost::LossBinomial$new(),
  oob_fraction  = 0.3,
  stop_args     = list(patience = 10L, eps_for_break = 0.00001),
  learning_rate = 0.01,
  df = 20, df_cat = 1)

ri_full = cboost$getInbagRisk()

cboost$train(length(cboost$getSelectedBaselearner()) - 11L)

set.seed(1618)
cboost_restart = compboost::boostSplines(
  data          = dat,
  target        = target,
  iterations    = 100,
  optimizer     = compboost::OptimizerCoordinateDescent$new(),
  loss          = compboost::LossBinomial$new(cboost$predict(), TRUE),
  stop_args     = list(oob_offset = cboost$predict(cboost$data_oob)),
  oob_fraction  = 0.3,
  learning_rate = 0.01,
  df = 20, df_cat = 1)

testCWB = function(cboost, cboost_restart) {
  ri = cboost$getInbagRisk()
  ro = cboost_restart$getInbagRisk()
  r  = c(ri, ro)
  return(r)
}

r = testCWB(cboost, cboost_restart)
plot(x = seq_along(r), y = r, type = "l")

plot(x = seq_along(r[-92]), y = r[-92], type = "l")
lines(x = seq_along(ri_full), y = ri_full, col = "red", lty = 2)

a = cboost$predict(cboost$data)
b = cboost$response$getPrediction()

cbind(a, b)

cwb_pars = list(
  patience      = 10L,
  mstop         = 2000,
  oob_seed      = 1618,
  eps_for_break = 0.00001,
  ncores        = 4,
  stop_both     = FALSE,
  show_output   = TRUE)

## Helper to merge custom set HPs with default values:
updatePars = function(lrn, pars) {
  lvalues = lrn$param_set$values
  return(mlr3misc::insert_named(lvalues, cwb_pars))
}

library(mlr3)
library(R6)
library(mlr3tuning)

source("classifCompboost.R")
load("meta/tasks.Rda")

task = TASKS[[2]]
l = lrn("classif.compboost", id = "acc_hcwb", predict_type = "prob",
  optimizer = "nesterov", restart = TRUE, stop_both = FALSE,
  learning_rate = 0.01, momentum = 0.03, optimizer = "nesterov",
  oob_fraction = 0.3, df = 10, df_cat = 2)
l$param_set$values = updatePars(l, cwb_pars)
l$train(task$filter(which(complete.cases(task$data()))[1:30000]))


library(mlr3pipelines)
source("helper.R")
source("classifCompboost.R")
load("meta/tasks.Rda")

tint = 4

## HCWB:
tl1 = constructLearner("acc_hcwb", ncores = 4L, test_mode = FALSE, raw_learner = FALSE)

rr = RESAMPLE_SETS$`168335`
ttrain = TASKS[[tint]]$clone(deep = TRUE)$filter(rr$train_set(4))
ttest = TASKS[[tint]]$clone(deep = TRUE)$filter(rr$train_set(4))
tl1$train(ttrain)

r1 = tl1$model$acc_hcwb$model$cboost$getInbagRisk()
r2 = tl1$model$acc_hcwb$model$cboost_restart$getInbagRisk()

r = c(r1, r2)
plot(seq_along(r), r, type = "l")


 tl1$graph$pipeops$acc_hcwb$learner$transition
l1 = getCboostMsrsTrace(tl1, list(test = ttest, train = ttrain),
  iters = c(1, seq(16, 160, by = 16), seq(164, 256, by = 4), seq(260, 500, by = 16)))

library(ggplot2)
library(dplyr)
gg_task = ggplot() +
  geom_line(data = l1 %>% filter(tset == "test"), aes(x = microseconds, y = classif.auc, color = "Test AUC", linetype = "hCWB")) +
  geom_line(data = l1 %>% filter(tset == "test"), aes(x = microseconds, y = risk_oob, color = "OOB Risk", linetype = "hCWB")) +
  geom_line(data = l1 %>% filter(tset == "test"), aes(x = microseconds, y = risk, color = "Inbag Risk", linetype = "hCWB"))





## ACWB:
tl2 = constructLearner("acc_acwb", ncores = 4L, test_mode = FALSE, raw_learner = FALSE)
ttrain = TASKS[[tint]]$clone(deep = TRUE)$filter(1:3000)
ttest = TASKS[[tint]]$clone(deep = TRUE)$filter(3001:4601)
tl2$train(ttrain)

tr2 = tl2$graph$pipeops$acc_hcwb$learner$transition
l2 = getCboostMsrsTrace(tl2, list(test = ttest, train = ttrain), iters = c(1, seq(4, 5000, by = 4)))


library(ggplot2)
library(dplyr)
gg_task = ggplot() +
  geom_line(data = l1 %>% filter(tset == "test"), aes(x = microseconds, y = classif.auc, color = "Test AUC", linetype = "hCWB")) +
  geom_line(data = l1 %>% filter(tset == "test"), aes(x = microseconds, y = risk_oob, color = "OOB Risk", linetype = "hCWB")) +
  geom_line(data = l1 %>% filter(tset == "test"), aes(x = microseconds, y = risk, color = "Inbag Risk", linetype = "hCWB")) #+
  #geom_line(data = l2 %>% filter(tset == "test"), aes(x = microseconds, y = classif.auc, color = "Test AUC", linetype = "ACWB")) +
  #geom_line(data = l2 %>% filter(tset == "test"), aes(x = microseconds, y = risk, color = "Inbag Risk", linetype = "ACWB"))





r1 = tl1$model$cboost$getInbagRisk()
r2 = tl1$model$cboost_restart$getInbagRisk()

l1 = tl1$model$cboost$getLoggerData()
l2 = tl1$model$cboost_restart$getLoggerData()

jt = getJobTable()
jt$job.id[unlist(jt$algo.pars) == "acc_hcwb"]

dim(l1)
dim(l2)

r = c(r1, r2)
plot(x = seq_along(r), y = r, type = "l")
abline(v = 47, col = "gray", lty = 2)



lossBin = function(response, pred) {
  log(1 + exp(-2 * response * pred));
}
mean(lossBin(cboost$response$getResponse(), cboost$response$getPrediction()))
mean(lossBin(cboost$response$getResponse(), cboost$predict(cboost$data)))
ri = cboost$getInbagRisk()

rr = cboost_restart$getInbagRisk()


