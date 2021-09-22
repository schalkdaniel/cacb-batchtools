## PACKAGES:
## =============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(batchtools)
library(mlr3)
library(gridExtra)

## FIGURE SETTINGS:
## =============================================

font = "TeX Gyre Bonum"

sysfonts::font_add(font,
    #regular = paste0(base_dir, "/paper-figures/gyre-bonum/texgyrebonum-regular.ttf"),
    #bold = paste0(base_dir, "/paper-figures/gyre-bonum/texgyrebonum-bold.ttf"))
    regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
    bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
#showtext::showtext_auto()
extrafont::font_import(paths = "~/repos/bm-CompAspCboost/paper-figures/gyre-bonum", prompt = FALSE)
extrafont::loadfonts()

theme_set(
  theme_minimal(base_family = font) +
  ggplot2::theme(
    strip.background = element_rect(fill = rgb(47, 79, 79, maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 6),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
)
dinA4width = 162
mycolors = ggsci::pal_uchicago()(6)[4:6]

my4breaks = function(x) {
  m = x[2] - x[1]
  seq(x[1] + 0.05*m, x[2] - 0.05*m, length.out = 4)
}
my4ylabels = function(x) as.character(round(my4breaks(x), 4))
my4xlabels = function(x) as.character(round(my4breaks(x), 0))

## HELPER:
## =============================================

getStopInfo = function(clog, patience = 10L, eps_for_break = 0.00001,
  vname = "val_auc", tname = "test_auc", minimize = TRUE) {

  dsign = 1
  if (minimize) dsign = -1

  rval     = 1 - clog[[vname]]
  rvaldiff = diff(rval) / rval[-length(rval)] * dsign
  p0 = 0
  for (i in seq_along(rvaldiff)) {
    if (rvaldiff[i] < eps_for_break)
      p0 = p0 + 1
    else
      p0 = 0

    if (p0 == patience) break
  }
  if (i < (nrow(clog) - 1))
    istop = i - (patience + 1)
  else
    istop = nrow(clog)

  return(data.frame(stop = istop, val_auc = clog[[vname]][istop], test_auc = clog[[tname]][istop],
    seconds = clog$seconds[istop], learner = clog$learner[1], task = clog$task[1], fold = clog$fold[1],
    val_acu_max = max(clog[[vname]]), test_auc_max = max(clog[[tname]]), transition = clog$transition[1]))
}

## GET RESULTS:
## =============================================

base_dir = here::here()
bm_dir   = paste0(base_dir, "/eq1/batchtools/")

loadRegistry(file.dir = bm_dir, work.dir = paste0(base_dir, "/eq1"))
getStatus()

jt  = getJobTable(findDone())
res = reduceResultsList(jt)

## PROCESS RESULTS:
## =============================================

for (i in seq_along(res)) {
  res[[i]] = cbind(res[[i]],
    fold = as.numeric(gsub("\\D", "", jt$problem[i])),
    task = sub("\\-.*", "", jt$problem[i]),
    learner = jt$algo.pars[[i]]$lid)
  res[[i]][["_iterations"]] = seq_len(nrow(res[[i]]))

  # In compboost, the AUC is defined as 1 - AUC since
  # stopping is done on minimization. Hence, transform
  # the AUC back:
  if ("riskauc" %in% names(res[[i]]))
    res[[i]]$test_auc  = res[[i]]$riskauc

  res[[i]]$test_auc = 1 - res[[i]]$test_auc
  res[[i]]$val_auc  = 1 - res[[i]]$val_auc
  res[[i]]$seconds  = res[[i]]$time / 1e6
}
df_res = do.call(rbind, lapply(res, function(r) {
  lnames = c("_iterations", "test_auc", "seconds", "blearner", "risk", "model", "transition", "fold", "task", "learner")
  r[, lnames]
}))
df_res[["iteration"]]    = df_res[["_iterations"]]
df_res[["_iterations"]]  = NULL
df_res[["test_auc"]]     = df_res[["test_auc"]]
df_res[["task"]]         = factor(df_res[["task"]], levels = TSKS_SETUP$id)

lids_old = c("cwb", "cwb_b", "acwb", "hcwb", "hcwb_b", "acwb_b")
lids_new = c("CWB", "CWB (b)", "ACWB", "ACWB (b)", "hCWB", "hCWB (b)")

lids = c(
  "cwb"    = "CWB",
  "cwb_b"  = "CWB (b)",
  "acwb"   = "ACWB",
  "acwb_b" = "ACWB (b)",
  "hcwb"   = "hCWB",
  "hcwb_b" = "hCWB (b)")

df_res = df_res %>%
  filter(learner != "acc_hcwb2") %>%
  mutate(learner = factor(learner))# %>%
  #filter(! learner %in% c("CWB (b)", "CWB (nb)"))

levels(df_res$learner) = lids[levels(df_res$learner)]


## SUMMARY VALUES:
## =============================================

df_smry = df_res %>%
  group_by(task, learner, iteration) %>%
  summarize(test_auc = mean(test_auc),
    seconds    = mean(seconds),
    transition = transition[1]) %>%
  group_by(task, learner) %>%
  summarize(
    auc_start  = min(test_auc),
    iter_best  = iteration[which.max(test_auc)],
    auc_best   = max(test_auc),
    sec_best    = seconds[which.max(test_auc)],
    transition = transition[1],
    auc_expl   = (max(test_auc) - min(test_auc)) / min(test_auc)
  ) %>%
  group_by(task) %>%
  mutate(auc_start = min(auc_start)) %>%
  group_by(task) %>%
  mutate(
    auc_diff_to_cwb = ifelse("CWB" %in% learner, auc_best[learner == "CWB"] - auc_best, NA),
    binning = ifelse(grepl("(b)", learner), "Binning", "No binning"),
    learner = gsub(" (nb)", "", gsub(" (b)", "", learner, fixed = TRUE), fixed = TRUE))

df_smry$learner = factor(df_smry$learner, levels = c("CWB", "ACWB", "hCWB"))
df_smry$binning = factor(df_smry$binning, levels = c("No binning", "Binning"))


## FITTING TRACES:
## =============================================

df_aggr = df_res %>%
  group_by(task, learner, iteration) %>%
  summarize(test_auc = mean(test_auc), seconds = mean(seconds))

sf  = function(x, p = 10) 1 - (1 - (0.2 * x)^p)^(1/p)
sfi = function(x, p = 10) 5 * (1 - (1 - x)^p)^(1/p)
sfb = function(x, p = 10) {
  bl = sf(x[1], p)
  bu = sf(x[2], p)
  sfi(seq(bl, bu, length.out = 4L))
}
stretch_trans = function() scales::trans_new("stretch", sf, sfi, sfb, )
formatFun = function(x) sprintf("%.3f", x)

df_aggr2 = df_aggr %>%
  mutate(
    binning = ifelse(grepl("(b)", learner), "Binning", "No binning"),
    learner = gsub(" (nb)", "", gsub(" (b)", "", learner, fixed = TRUE), fixed = TRUE))
df_aggr2$learner = factor(df_aggr2$learner, levels = c("CWB", "ACWB", "hCWB"))
df_aggr2$binning = factor(df_aggr2$binning, levels = c("No binning", "Binning"))

tdims = sapply(levels(df_aggr2$task), function(tn) {
  ts = TASKS[[tn]]
  if (tn == "spam") tnn = "Spam"
  if (tn == "168908") tnn = "Christine"
  if (tn == "9977") tnn = "Namao"
  if (tn == "7592") tnn = "Adult"
  if (tn == "168335") tnn = "MiniBooNE"
  if (tn == "189866") tnn = "Albert"
  return(paste0(tnn, "   n: ", ts$nrow, ", p: ", length(ts$feature_names)))
})

df_aggr2$taskn = factor(tdims[df_aggr2$task])
df_aggr2$taskn = factor(df_aggr2$taskn, levels = levels(df_aggr2$taskn)[c(6, 3, 5, 1, 4, 2)])

df_smry$taskn = factor(tdims[df_smry$task])
df_smry$taskn = factor(df_smry$taskn, levels = levels(df_smry$taskn)[c(6, 3, 5, 1, 4, 2)])


gg_tr = ggplot() +
  #geom_hline(data = df_smry, aes(yintercept = auc_best, color = learner), linetype = "dashed", alpha = 0.5, size = 0.2) +
  #geom_vline(data = df_smry, aes(xintercept = sec_best, color = learner), linetype = "dashed", alpha = 0.5, size = 0.2) +

  #geom_point(data = df_smry, mapping = aes(x = 0, y = auc_best, shape = binning, color = learner), size = 1.5) +
  #geom_point(data = df_smry, mapping = aes(x = sec_best, y = auc_start, shape = binning, color = learner), size = 1.5) +

  geom_line(data = df_aggr2, mapping = aes(x = seconds, y = test_auc, color = learner, linetype = binning), size = 0.5, alpha = 0.8) +

  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  xlab("Training time (seconds, log scale)") +
  ylab("Test AUC") +
  #scale_y_continuous(trans = "stretch") +
  scale_y_continuous(breaks = my4breaks, labels = my4ylabels) +
  labs(color = "Learner", linetype = "", shape = "") +
  facet_wrap(. ~ taskn, nrow = 2, scales = "free")

ggsave(gg_tr, filename = "fig-eq1-1.pdf", width = dinA4width * 1.15, height = dinA4width * 0.5, units = "mm")


## OPTIMAL STOPS:
## =============================================

#dev.off()
#grid.table(iris[1:5, 1:3], theme = t1)

df_stop           = do.call(rbind, lapply(res, getStopInfo))
df_stop[["task"]] = factor(df_stop[["task"]], levels = TSKS_SETUP$id)

df_stop_smr = df_stop %>%
  #filter(learner != "acc_hcwb2") %>%
  group_by(learner, task) %>%
  summarize(auc = median(test_auc), sec = median(seconds),
    auc_min = min(test_auc), auc_max = max(test_auc), auc25 = quantile(test_auc, 0.25), auc75 = quantile(test_auc, 0.75),
    sec_min = min(seconds), sec_max = max(seconds), sec25 = quantile(seconds, 0.25), sec75 = quantile(seconds, 0.75)) %>%
  ungroup() %>%
  mutate(learner = factor(learner))
levels(df_stop_smr$learner) = lids[levels(df_stop_smr$learner)]
df_stop_smr$learner = factor(df_stop_smr$learner, levels = lids)

df_stop_smr = df_stop_smr %>%
  mutate(
    binning = ifelse(grepl("(b)", learner), "Binning", "No binning"),
    learner = gsub(" (nb)", "", gsub(" (b)", "", learner, fixed = TRUE), fixed = TRUE))
df_stop_smr$learner = factor(df_stop_smr$learner, levels = c("CWB", "ACWB", "hCWB"))
df_stop_smr$binning = factor(df_stop_smr$binning, levels = c("No binning", "Binning"))

tdims = sapply(levels(df_stop_smr$task), function(tn) {
  ts = TASKS[[tn]]
  if (tn == "spam") tnn = "Spam"
  if (tn == "168908") tnn = "Christine"
  if (tn == "9977") tnn = "Namao"
  if (tn == "7592") tnn = "Adult"
  if (tn == "168335") tnn = "MiniBooNE"
  if (tn == "189866") tnn = "Albert"
  return(paste0(tnn, "   n: ", ts$nrow, ", p: ", length(ts$feature_names)))
})

df_stop_smr$task = factor(tdims[df_stop_smr$task])
df_stop_smr$task = factor(df_stop_smr$task, levels = levels(df_stop_smr$task)[c(6, 3, 5, 1, 4, 2)])

library(gridExtra)
library(ggpp)

tnames = unique(df_stop_smr$task)
tbls = lapply(tnames, function(tn) {
  if ("CWB" %in% df_stop_smr[(df_stop_smr$task == tn) & (df_stop_smr$binning == "No binning"), ]$learner) {
    out = df_stop_smr %>% filter(task == tn) %>%
      select(learner, auc, sec, binning) %>%
        mutate(auc = round(auc[(learner == "CWB") & (binning == "No binning")] - auc, 4),
          sec = round(sec[(learner == "CWB") & (binning == "No binning")] / sec, 2)) %>%
        filter(! ((learner == "CWB") & (binning == "No binning")))
    lln = c("CWB", "ACWB", "hCWB")
    idx = unlist(sapply(lln, function(ln) which(ln == out$learner)))
    out = out[idx, ]

    colnames(out) = latex2exp::TeX(c("", "$\\Delta$ AUC", "$\\Delta$ Sec.", ""), output = "character")
    return(out)
  } else {
    return(NULL)
  }
})

names(tbls) = tnames
tt2 = lapply(tnames, function (tn) {
  out = cbind(tbls[[tn]], task = tn)
  colnames(out) = c("lrn", "auc", "sec", "binning", "task")
  out
})
tt3 = do.call(rbind, tt2)
tt3 %>%
  group_by(lrn, binning) %>%
  summarize(sec = mean(sec), auc = mean(auc)) %>%
  arrange(desc(sec))



xlbs = sapply(tnames, function(tn) {
  return(Inf)
  #xmax = max(df_stop_smr[df_stop_smr$task == tn, ]$sec_max)
  #xmin = min(df_stop_smr[df_stop_smr$task == tn, ]$sec_min)
  #xmin + (xmax - xmin) * 2.3 / 3
})

ylbs = sapply(tnames, function(tn) {
  if (grepl("168335", tn)) return(Inf)
  return(-Inf)
  #low = 0.5
  #if (tn %in% c("168335")) low = 2.2
  #ymax = max(df_stop_smr[df_stop_smr$task == tn, ]$auc_max)
  #ymin = min(df_stop_smr[df_stop_smr$task == tn, ]$auc_min)
  #ymin + (ymax - ymin) * low / 3
})


#data_tb = tibble(task = tnames, x = c(12.5, 60, 17.5, 175, 200, 50), y = c(0.96, 0.77, 0.9105, 0.974, 0.7305, 0.964), tb = tbls)
data_tb = tibble(task = tnames, x = xlbs, y = ylbs, tb = tbls)
#data_tb = tibble(task = tnames, x = Inf, y = Inf, tb = tbls)


library(ggpp)
library(gridExtra)
t1 = ttheme_default(base_size = 2, parse = TRUE, core = list(
  #fg_params = list(fontface = c(rep("plain", 4), "bold.italic")),
  bg_params = list(fill = c(mycolors[1], mycolors[c(2, 2)], mycolors[c(3, 3)]), alpha = c(0.35, 0.2))
))


gg_bb = ggplot(df_stop_smr) +
  geom_rect(data = df_stop_smr %>% filter(learner == "CWB", binning == "No binning"),
    aes(xmin = -Inf, xmax = Inf, ymin = auc25, ymax = auc75),
    alpha = 0.1, size = 0, show.legend = FALSE, fill = mycolors[1]) +
  geom_rect(data = df_stop_smr %>% filter(learner == "CWB", binning == "No binning"),
    aes(xmin = sec25, xmax = sec75, ymin = -Inf, ymax = Inf),
    alpha = 0.1, size = 0, show.legend = FALSE, fill = mycolors[1]) +
  geom_segment(aes(y = auc_min, x = sec, yend = auc, xend = sec, color = learner), size = 0.2, alpha = 0.8) +
  geom_segment(aes(y = auc, x = sec_min, yend = auc, xend = sec_max, color = learner), size = 0.2, alpha = 0.8) +
  geom_segment(aes(y = auc25, x = sec, yend = auc75, xend = sec, color = learner), size = 1., alpha = 0.8) +
  geom_segment(aes(y = auc, x = sec25, yend = auc, xend = sec75, color = learner), size = 1., alpha = 0.8) +
  geom_point(aes(x = sec, y = auc, shape = binning), color = "white", size = 3) +
  geom_point(aes(x = sec, y = auc, color = learner, shape = binning), size = 2) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  #theme_bw() +
  labs(shape = "") +
  xlab("Training time (seconds)") +
  ylab("Test AUC") +
  labs(color = "Learner") +
  scale_y_continuous(breaks = my4breaks, labels = my4ylabels) +
  scale_x_continuous(breaks = my4breaks, labels = my4xlabels) +
  #geom_table(data = data_tb %>% filter(y < 0), aes(x, y, label = tb), vjust = "left", hjust = "top",
    #size = 0.01, parse = TRUE, table.theme = t1) +
  #geom_table(data = data_tb %>% filter(y > 0), aes(x = 131, y = 0.97306, label = tb), vjust = "left", hjust = "bottom",
    #size = 0.01, parse = TRUE, lineheight = 0, table.theme = t1) +
  facet_wrap(. ~ task, scales = "free")

ggsave(gg_bb, filename = "fig-eq1-2.pdf", width = dinA4width * 1.15, height = dinA4width * 0.5, units = "mm")


## MBOOST WITH OPTIMAL STOPS:
## =============================================

cwb_stops = df_stop %>% filter(learner == "cwb")
substrRight = function(x, n = 1) substr(x, nchar(x) - n + 1, nchar(x))

ll_tt = list()
for (i in seq_len(nrow(cwb_stop))) {
  message("[", Sys.time(), "] ", i, "/", nrow(cwb_stops))
  l = lrn("classif.gamboost", mstop = cwb_stops$stop[i])
  fold = as.integer(substrRight(cwb_stops$fold[i]))
  tset = RESAMPLE_SETS[[cwb_stops$task[i]]]$train_set(fold)
  t = TASKS[[cwb_stops$task[i]]]$filter(tset)
  l$train(t)
  ll_tt[[i]] = data.frame(learner = "mboost", train_time = l$state$train_time,
    task = cwb_stops$task[i], fold = fold)
}
df_mboost = do.call(rbind, ll_tt)


## AVERAGE RUNTIME IMPROVEMENT:
## =============================================

ntsks = c("spam" = "Spam", "168908" = "Christine", "7592" = "Adult",
  "168335" = "MiniBooNE", "189866" = "Albert", "9977" = "Namao")

df_smry2 = df_stop_smr %>%
  group_by(task) %>%
  mutate(
    speedup = sec[(learner == "CWB") & (binning == "No binning")] / sec,
    auc_imp = auc - auc[(learner == "CWB") & (binning == "No binning")])

#df_smry2 %>%
  #filter(! ((learner == "CWB") & (binning == "No binning"))) %>%
  #group_by(task) %>%
  #summarize(mspeedup = mean(speedup))

df_smry2 %>%
  filter(! ((learner == "CWB") & (binning == "No binning"))) %>%
  group_by(learner, binning) %>%
  summarize(mspeedup = mean(speedup))

df_smry31 = df_smry2 %>%
  filter(! ((learner == "CWB") & (binning == "No binning"))) %>%
  mutate(taskn = ntsks[task]) %>%
  group_by(taskn, learner, binning) %>%
  summarize(mspeedup = mean(speedup)) %>%
  pivot_wider(names_from = taskn, values_from = mspeedup, names_prefix = "sec-")

df_smry32 = df_smry2 %>%
  filter(! ((learner == "CWB") & (binning == "No binning"))) %>%
  mutate(taskn = ntsks[task]) %>%
  group_by(taskn, learner, binning) %>%
  summarize(mauc_imp = mean(auc_imp)) %>%
  pivot_wider(names_from = taskn, values_from = mauc_imp, names_prefix = "auc-")

df_smry3 = cbind(df_smry31, df_smry32[,-c(1,2)])
pres = c("sec-", "auc-")
knitr::kable(df_smry3[, c("learner", "binning",
  paste0(pres, "Spam"),
  paste0(pres, "Christine"),
  paste0(pres, "Namao"),
  paste0(pres, "Adult"),
  paste0(pres, "MiniBooNE"),
  paste0(pres, "Albert"))], format = "latex")


## TESTING PERFORMANCE:
## =============================================

library(mgcv)

df_smry$learner = factor(df_smry$learner, levels = c("CWB", "ACWB", "hCWB"))
df_smry$binning = factor(df_smry$binning, levels = c("No binning", "Binning"))
#mod = gam(auc_best ~ task + binning + learner + task*binning + learner*task + binning*learner, data = df_smry, family = betar)
mod = gam(auc_best ~ task + binning + learner + binning*learner, data = df_smry, family = betar)
knitr::kable(summary(mod)$p.table, format = "latex")
knitr::kable(summary(mod)$pTerms.table, format = "latex")


## INDIVIDUAL INSPECTION
## =============================================

source("classifCompboost.R")

robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")

tname = "9977"
task = TASKS[[tname]]

task_new = robustify$train(task)[[1]]

rr = RESAMPLE_SETS[[tname]]
ttrain = task_new$clone(deep = TRUE)$filter(rr$train_set(3))
ttest  = task_new$clone(deep = TRUE)$filter(rr$test_set(3))

#source("learner-src/classifCWB.R")

tl1 = constructLearner2("cwb", ncores = 4L, test_mode = TRUE, raw_learner = TRUE)
tl1$param_set$values$additional_auc_task = ttest
tl1$param_set$values$df_cat = 2
tl1$param_set$values$df     = 20
tl1$param_set$values$mstop  = 2
tl1$param_set$values$df_autoselect = FALSE

problem_feats = c()
for (i in seq_along(ttrain$feature_names)) {
  message("Use feature: ", ttrain$feature_names[i])
  ttt = ttrain$clone(deep = TRUE)
  nn = try(tl1$train(ttt$select(ttt$feature_names[i])), silent = TRUE)
  if (inherits(nn, "try-error")) {
    problem_feats = c(problem_feats, ttrain$feature_names[i])
    message("Was not able to fit")
  } else {
    message("Was able to fit model")
  }
}

problem_feats

log_acwb = getCboostLog(tl1)
log_acwb$oob_risk = NA

#tl1 = constructLearner("acc_hcwb2", ncores = 4L, test_mode = FALSE, raw_learner = TRUE)
#tl1$param_set$values$task_extra_log = ttest
#tl1$param_set$values$log_auc = TRUE
#tl1$param_set$values$mstop = 5000
#tl1$train(ttrain)

#log_hcwb1 = getCboostLog(tl1)



tl2 = constructLearner2("acc_hcwb2", ncores = 4L, test_mode = FALSE, raw_learner = TRUE)
tl2$param_set$values$additional_auc_task = ttest
tl2$param_set$values$use_stopper = FALSE
tl2$param_set$values$use_stopper_auc = TRUE
tl2$param_set$values$mstop = 3000
tl2$train(ttrain)

ldat = rbind(tl2$model$cboost$getLoggerData(), tl2$model$cboost_restart$getLoggerData())
auc = ldat$test_auc

dauc = (diff(auc) / auc[-length(auc)]) < tl2$param_set$values$eps_for_break
cumsum(dauc)

library(ggplot2)

ggplot() +
  geom_line(data = log_acwb, aes(x = seq_along(test_auc), y = test_auc, color = "ACWB")) +
  geom_line(data = ldat, aes(x = seq_along(test_auc), y = 1 - test_auc, color = "hCWB"))
plot(x = seq_along(auc), y = 1 - auc, type = "l")

log_hcwb_auc = rbind(tl2$model$cboost$getLoggerData(), tl2$model$cboost_restart$getLoggerData())


#tl1 = constructLearner("acc_hcwb2", ncores = 4L, test_mode = FALSE, raw_learner = TRUE)
#tl1$param_set$values$task_extra_log = ttest
#tl1$param_set$values$log_auc = TRUE
#tl1$param_set$values$mstop = 5000
#tl1$param_set$values$eps_for_break = 0
#tl1$train(ttrain)

#log_hcwb2 = getCboostLog(tl1)


library(ggplot2)
ggplot() +
  geom_line(data = log_acwb, aes(x = seq_along(test_auc), y = test_auc, color = "ACWB")) +
  geom_line(data = log_hcwb1, aes(x = seq_along(test_auc), y = test_auc, color = "hCWB eps>0")) +
  #geom_line(data = log_hcwb2, aes(x = seq_along(test_auc), y = test_auc, color = "hCWB eps=0")) +
  geom_vline(data = log_hcwb1, aes(xintercept = transition[1], color = "hCWB eps>0"))
  #geom_vline(data = log_hcwb2, aes(xintercept = transition[1], color = "hCWB eps=0"))


r = log_hcwb1$test_auc
plot(r, type = "l")

which.min(r)
which(diff(r) < 0)


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
