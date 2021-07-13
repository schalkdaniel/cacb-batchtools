getTasks = function(ids, types) {
  suppressMessages(requireNamespace("mlr3oml"))
  tasks = list()

  for (i in seq_along(ids)) {

    id   = ids[i]
    type = types[i]

    message(as.character(Sys.time()), ": (", i, "/", length(ids),
      ") Process task ", id)
    if (type == "oml-task") {
      e = try({

        nuisance = capture.output({
          ts = tsk("oml", task_id = as.integer(as.character(id)))
        })
        if (as.character(id) == "168335") {
          dat = ts$data()
          for (i in seq_along(dat)) {
            if (is.numeric(dat[[i]])) {
              idx_na = dat[[i]] == -999
              dat[[i]][idx_na] = NA
            }
          }
          ts = TaskClassif$new(id = ts$id, backend = dat, target = "signal")
        }
        ts
      }, silent = TRUE)
      if (! "try-error" %in% class(e)) {
        if ("twoclass" %in% e$properties) {
          if (! all(is.na(e$data()))) tasks[[as.character(id)]] = e
        }
      } else {
        cat(e)
      }
    }

    #if (type == "oml-data") {
      #ts = mlr3oml::read_arff("https://www.openml.org/data/download/19335520/file7b53746cbda2.arff")
      #tasks[[as.character(id)]] = TaskClassif$new(id = "albert", backend = ts, target = "class")
    #}

    if (type == "mlr-task") {
      tasks[[as.character(id)]] = tsk(as.character(id))
    }
  }
  return(tasks)
}

createResampleSets = function(tasks, seed, ...) {
  resample_sets = list()
  tnames = names(tasks)
  for (tn in tnames) {
    ts = tasks[[tn]]$clone(deep = TRUE)
    ts$col_roles$stratum = ts$target_names
    rr = rsmp(...)

    set.seed(seed)
    rr$instantiate(ts)
    riters = rr$iters
    resample_sets[[tn]] = list(
      train = lapply(seq_len(riters), function(i) rr$train_set(i)),
      test  = lapply(seq_len(riters), function(i) rr$test_set(i)))
  }
  return(resample_sets)
}

getResampleInstances = function(tasks, resample_sets) {
  resamplings = list()
  tnames = names(tasks)
  for (tn in tnames) {

    strain = resample_sets[[tn]]$train
    stest  = resample_sets[[tn]]$test
    rr = rsmp("custom")
    rr$instantiate(tasks[[tn]],
      train = strain,
      test  = stest)
    resamplings[[tn]] = rr
  }
  return(resamplings)
}
