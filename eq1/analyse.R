library(batchtools)
library(mlr3)

base_dir = here::here()
bm_dir   = paste0(base_dir, "/eq1/batchtools/")

loadRegistry(file.dir = bm_dir, work.dir = paste0(base_dir, "/eq1"))
res = reduceResultsList(setdiff(findDone()$job.id, 61))
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

ggplot(data = df_res %>% filter(tset == "test"), mapping = aes(x = microseconds / 1e6, y = classif.auc, color = learner)) +
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
  filter(grepl("adult", task), learner == "acc_hcwb", tset == "test")

ggplot(a, aes(x = microseconds, y = risk, color = as.character(fold))) +
  geom_line()

plot(x, 1 - (1 - x^50)^(1/60), type = "l")

x = seq(0.5, 1, by = 0.01)
y = x
plot(x, y^8, type = "l")

ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_line() +
  scale_y_continuous(trans = "sqrt")


tl1 = constructLearner("acc_hcwb", ncores = 4L, test_mode = FALSE, raw_learner = FALSE)
tl1$train(TASKS[[1]])

l = getCboostMsrsTrace(tl1, list(test = TASKS[[1]]), iters = c(10, 47, 48, 49, 50, 51, 52, 100, 1000))

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


