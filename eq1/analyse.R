library(batchtools)

base_dir = here::here()
bm_dir   = paste0(base_dir, "/eq1/batchtools/")

loadRegistry(bm_dir)
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

df_res %>%
  group_by(task, learner, iteration, tset) %>%
  summarize(auc = mean(classif.auc), ms = mean(microseconds)) %>%
  ggplot(aes(x = ms, y = auc))



