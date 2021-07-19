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

sf = function(x) max(x) - x^(-2)
stretch = function() scales::trans_new("one_over", sf, sf)

ggplot(mapping = aes(x = microseconds, y = classif.auc, color = learner)) +
  geom_line(data = df_res %>% filter(tset == "test"), aes(group = paste0(learner, fold)), alpha = 0.3) +
  geom_line(data = df_aggr %>% filter(tset == "test")) +
  geom_point(data = df_smry, mapping = aes(x = 0, y = auc_best)) +
  #scale_y_continuous(trans = "") +
  ggsci::scale_color_uchicago() +
  facet_wrap(. ~ task, nrow = 2, scales = "free")



