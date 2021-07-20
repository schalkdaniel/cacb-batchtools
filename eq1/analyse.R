library(batchtools)
library(mlr3)

base_dir = here::here()
bm_dir   = paste0(base_dir, "/eq1/batchtools/")

loadRegistry(file.dir = bm_dir, work.dir = paste0(base_dir, "/eq1"))
res = reduceResultsList(findDone())
getStatus()


library(ggplot2)
library(dplyr)
library(tidyr)

for (i in seq_along(res)) {
  fold = ifelse(i %% 5 == 0, 5, i %% 5)
  res[[i]] = cbind(res[[i]], fold = fold)
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
    iter_best  = iteration[which.max(classif.auc)],
    auc_best   = max(classif.auc),
    ms_best    = microseconds[which.max(classif.auc)],
    transition = transition[1],
    auc_expl   = (max(classif.auc) - min(classif.auc)) / min(classif.auc)
  ) %>%
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
  geom_line(aes(group = paste0(learner, fold)), alpha = 0.3, size = 0.6) +
  geom_line(data = df_aggr %>% filter(tset == "test"), size = 1.2) +
  geom_point(data = df_smry, mapping = aes(x = 0, y = auc_best)) +
  scale_y_continuous(trans = "stretch", labels = formatFun) +
  xlab("Seconds") +
  ylab("AUC") +
  ggsci::scale_color_uchicago() +
  facet_wrap(. ~ task, nrow = 2, scales = "free")



plot(x, 1 - (1 - x^50)^(1/60), type = "l")

x = seq(0.5, 1, by = 0.01)
y = x
plot(x, y^8, type = "l")

ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_line() +
  scale_y_continuous(trans = "sqrt")



