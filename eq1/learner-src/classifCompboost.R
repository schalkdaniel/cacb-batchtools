
cat("[", as.character(Sys.time()),   "] Loading new learner\n", sep = "")

LearnerClassifCompboost = R6Class("LearnerClassifCompboost",
  inherit = LearnerClassif,
  public = list(

    transition = NULL,
    iter = NULL,

    #' @description
    #' Create a `LearnerClassifCompboost` object.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "df", default = 5, lower = 1),
          ParamInt$new(id = "mstop", default = 100L, lower = 1L),
          ParamDbl$new(id = "learning_rate", default = 0.05, lower = 0),
          ParamDbl$new(id = "n_knots", default = 20L, lower = 4),
          ParamFct$new(id = "optimizer", default = "cod", levels = c("cod", "nesterov", "cos-anneal")),
          ParamInt$new(id = "ncores", default = 1L, lower = 1L, upper = parallel::detectCores()),
          ParamDbl$new(id = "momentum", default = 0.0005, lower = 0),
          ParamDbl$new(id = "oob_fraction", default = 0, lower = 0, upper = 0.9),
          ParamLgl$new(id = "use_stopper", default = FALSE),
          ParamInt$new(id = "patience", default = 5, lower = 1),
          ParamDbl$new(id = "eps_for_break", default = 0),
          ParamDbl$new(id = "bin_root", default = 0, lower = 0, upper = 4),
          ParamFct$new(id = "bin_method", default = "linear", levels = c("linear", "quantile")),
          ParamDbl$new(id = "df_cat", default = 1, lower = 1),
          ParamLgl$new(id = "restart", default = TRUE),
          ParamLgl$new(id = "stop_both", default = FALSE),
          ParamLgl$new(id = "df_autoselect", default = FALSE),
          ParamInt$new(id = "oob_seed", default = sample(seq_len(1e6), 1), lower = 1L),
          ParamLgl$new(id = "show_output", default = FALSE),
          ParamLgl$new(id = "log_auc", default = FALSE),
          ParamLgl$new(id = "stop_auc", default = FALSE),
          ParamUty$new(id = "task_extra_log")
        )
      )
      super$initialize(
        id = "classif.compboost",
        packages = "compboost",
        feature_types = c("numeric", "factor", "integer", "character"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass")
      )
    },
    setToIteration = function(iter) {
      if (is.null(self$transition)) {
        stop("No trained model!")
      } else {
        if (self$transition < iter) {
          if ("cboost_restart" %in% names(self$model))
            self$model$cboost_restart$train(iter - self$transition)
        } else {
          self$model$cboost$train(iter)
        }
        self$iter = iter
      }
    }
  ),

  private = list(
    .train = function(task) {

      ## Check number of cores:
      ncores = self$param_set$values$ncores
      if (ncores > length(task$feature_names)) {
        warning("Number of cores ", ncores, " exceeds number of features ",
          length(task$feature_names), ". The number of cores are set to ", length(task$feature_names), "!")
        ncores = length(task$feature_names)
      }

      ## Merge default hyperparameter with the specified ones:
      pdefaults = self$param_set$default
      pars = self$param_set$values
      for (id in self$param_set$ids()) {
        if (is.null(pars[[id]])) pars[[id]] = pdefaults[[id]]
      }
      self$param_set$values = pars

      if (self$param_set$values$df_autoselect) {
        factor_cols = task$feature_types$id[task$feature_types$type == "factor"]
        if (length(factor_cols) > 0) {
          df_cat_min = min(vapply(
            X = task$data(cols = factor_cols),
            FUN = function(fc) length(unique(fc)),
            FUN.VALUE = integer(1L)
          ))
          df = min(c(df_cat_min, self$param_set$values$n_knots))
          if (df <= 3) df = 5L

          self$param_set$values$df = df
          self$param_set$values$df_cat = df_cat_min
        } else {
          self$param_set$values$df = self$param_set$values$n_knots
        }
      }

      ## Create AUC loss:
      if (self$param_set$values$log_auc) {
        aucLoss = function(truth, response) return(mlr::measureAUC(response, truth, negative = -1, positive = 1) * length(truth))
        aucGrad = function(truth, response) return(rep(0, length(truth)))
        aucInit = function(truth) {
          p = mean(truth == 1)
          return(0.5 * p / (1 - p))
        }
        my_auc_loss = LossCustom$new(aucLoss, aucGrad, aucInit)
        additional_risk_log = list(auc = list(data = self$param_set$values$task_extra_log$data(), loss = my_auc_loss, stop = self$param_set$values$stop_auc))
      } else {
        additional_risk_log = list()
      }

      ## Define optimizer:
      if (self$param_set$values$optimizer == "cod") {
        optimizer = compboost::OptimizerCoordinateDescent$new(ncores)
      }
      if (self$param_set$values$optimizer == "nesterov") {
        optimizer = compboost::OptimizerAGBM$new(self$param_set$values$momentum, ncores)
      }
      if (self$param_set$values$optimizer == "cos-anneal") {
        optimizer = compboost::OptimizerCosineAnnealing$new(0.001, 0.3, 4, self$param_set$values$mstop,
          ncores)
      }

      ## Define stop arguments passed to compboost:
      if (self$param_set$values$use_stopper) {
        stop_args = list(patience = self$param_set$values$patience, eps_for_break = self$param_set$values$eps_for_break)
      } else {
        stop_args = NULL
      }

      out = list()
      #seed = sample(seq_len(100000), 1)
      seed = self$param_set$values$oob_seed

      if (self$param_set$values$oob_fraction == 0)
        oobf = NULL
      else
        oobf = self$param_set$values$oob_fraction

      if (self$param_set$values$show_output) {
        set.seed(seed)
        cboost = compboost::boostSplines(
          data          = task$data(),
          target        = task$target_names,
          iterations    = self$param_set$values$mstop,
          optimizer     = optimizer,
          loss          = compboost::LossBinomial$new(),
          df            = self$param_set$values$df,
          learning_rate = self$param_set$values$learning_rate,
          oob_fraction  = oobf,
          stop_args     = stop_args,
          bin_root      = self$param_set$values$bin_root,
          bin_method    = self$param_set$values$bin_method,
          df_cat        = self$param_set$values$df_cat,
          additional_risk_log = additional_risk_log)
      } else {
        nuisance = capture.output({
          set.seed(seed)
          cboost = compboost::boostSplines(
            data          = task$data(),
            target        = task$target_names,
            iterations    = self$param_set$values$mstop,
            optimizer     = optimizer,
            loss          = compboost::LossBinomial$new(),
            df            = self$param_set$values$df,
            learning_rate = self$param_set$values$learning_rate,
            oob_fraction  = oobf,
            stop_args     = stop_args,
            bin_root      = self$param_set$values$bin_root,
            bin_method    = self$param_set$values$bin_method,
            df_cat        = self$param_set$values$df_cat,
            additional_risk_log = additional_risk_log)
        })
      }
      iters = length(cboost$getSelectedBaselearner())

      ### Reset iterations if early stopping was used:
      if ((iters < self$param_set$values$mstop) && (stop_args$patience > 0)) {
        if (iters <= (stop_args$patience + 1)) {
          if (self$param_set$values$restart) {
            iters = 0
            out$cboost = NULL
            self$transition = 0
          } else {
            iters = iters - (stop_args$patience + 1)
            cboost$train(iters)
            self$transition = iters
            out$cboost      = cboost
          }
        } else {
          iters = iters - (stop_args$patience + 1)
          cboost$train(iters)
          self$transition = iters
          out$cboost      = cboost
        }
      } else {
        self$transition = iters
        out$cboost      = cboost
      }


      ### Restart:
      if (self$param_set$values$restart) {
        iters_remaining = self$param_set$values$mstop - iters

        if (iters_remaining > 0) {
          if (self$param_set$values$use_stopper) {
            if (self$param_set$values$stop_both) {
              if (is.null(out$cboost))
                stop_args_restart = stop_args
              else
                stop_args_restart = c(stop_args, list(oob_offset = out$cboost$predict(out$cboost$data_oob)))
            } else {
              if (is.null(out$cboost))
                stop_args_restart = list(patience = iters_remaining, eps_for_break = 0)
              else
                stop_args_restart = list(patience = iters_remaining, eps_for_break = 0,
                  oob_offset = out$cboost$predict(out$cboost$data_oob))
            }

            ## Create AUC loss:
            if (self$param_set$values$log_auc) {
              if (!is.null(out$cboost)) {
                cinit = out$cboost$predict(self$param_set$values$task_extra_log$data())
                aucInit = function(truth) cbind(cinit)
              }

              my_auc_loss = LossCustom$new(aucLoss, aucGrad, aucInit)
              additional_risk_log = list(auc = list(data = self$param_set$values$task_extra_log$data(), loss = my_auc_loss, stop = self$param_set$values$stop_auc))
            } else {
              additional_risk_log = list()
            }

            if (!is.null(out$cboost))
              loss = compboost::LossBinomial$new(out$cboost$predict(out$cboost$data), TRUE)
            else
              loss = compboost::LossBinomial$new()

            if (self$param_set$values$show_output) {
              set.seed(seed)
              cboost_restart = compboost::boostSplines(
                data          = task$data(),
                target        = task$target_names,
                iterations    = iters_remaining,
                optimizer     = compboost::OptimizerCoordinateDescent$new(ncores),
                loss          = loss,
                df            = self$param_set$values$df,
                stop_args     = stop_args_restart,
                oob_fraction  = oobf,
                learning_rate = self$param_set$values$learning_rate,
                bin_root      = self$param_set$values$bin_root,
                bin_method    = self$param_set$values$bin_method,
                df_cat        = self$param_set$values$df_cat,
                additional_risk_log = additional_risk_log)
            } else {
              nuisance = capture.output({
                set.seed(seed)
                cboost_restart = compboost::boostSplines(
                  data          = task$data(),
                  target        = task$target_names,
                  iterations    = iters_remaining,
                  optimizer     = compboost::OptimizerCoordinateDescent$new(ncores),
                  loss          = loss,
                  df            = self$param_set$values$df,
                  stop_args     = stop_args_restart,
                  oob_fraction  = oobf,
                  learning_rate = self$param_set$values$learning_rate,
                  bin_root      = self$param_set$values$bin_root,
                  bin_method    = self$param_set$values$bin_method,
                  df_cat        = self$param_set$values$df_cat,
                  additional_risk_log = additional_risk_log)
              })
            }
          } else {
            if (!is.null(out$cboost))
              loss = compboost::LossBinomial$new(out$cboost$predict(task$data()), TRUE)
            else
              loss = compboost::LossBinomial$new()

            if (self$param_set$values$show_output) {
              set.seed(seed)
              cboost_restart = compboost::boostSplines(
                data          = task$data(),
                target        = task$target_names,
                iterations    = iters_remaining,
                optimizer     = compboost::OptimizerCoordinateDescent$new(ncores),
                loss          = loss,
                df            = self$param_set$values$df,
                learning_rate = self$param_set$values$learning_rate,
                bin_root      = self$param_set$values$bin_root,
                bin_method    = self$param_set$values$bin_method,
                df_cat        = self$param_set$values$df_cat,
                additional_risk_log = additional_risk_log)
            } else {
              nuisance = capture.output({
                set.seed(seed)
                cboost_restart = compboost::boostSplines(
                  data          = task$data(),
                  target        = task$target_names,
                  iterations    = iters_remaining,
                  optimizer     = compboost::OptimizerCoordinateDescent$new(ncores),
                  loss          = loss,
                  df            = self$param_set$values$df,
                  learning_rate = self$param_set$values$learning_rate,
                  bin_root      = self$param_set$values$bin_root,
                  bin_method    = self$param_set$values$bin_method,
                  df_cat        = self$param_set$values$df_cat,
                  additional_risk_log = additional_risk_log)
              })
            }
          }
          out$cboost_restart = cboost_restart

          check1 = all.equal(cboost$data, cboost_restart$data)
          check2 = all.equal(cboost$data_oob, cboost_restart$data_oob)
          if (check1 + check2 < 2)
            stop("Data of restarted model is not equal the one of the first model!")
        }
      }
      return(out)
    },

    .predict = function(task) {
      #browser()
      newdata = task$data(cols = task$feature_names)

      if (is.null(self$model$cboost))
        lin_pred = 0
      else
        lin_pred = self$model$cboost$predict(newdata)

      if (("cboost_restart" %in% names(self$model)) && (self$transition < self$iter))
        lin_pred = lin_pred + self$model$cboost_restart$predict(newdata)

      probs = 1 / (1 + exp(-lin_pred))

      pos = self$model$cboost$response$getPositiveClass()
      neg = setdiff(names(self$model$cboost$response$getClassTable()), pos)
      pmat = matrix(c(probs, 1 - probs), ncol = 2L, nrow = length(probs))
      colnames(pmat) = c(pos, neg)
      if (self$predict_type == "prob") {
        list(prob = pmat)
      }
      if (self$predict_type == "response") {
        list(response = ifelse(probs > self$model$cboost$response$getThreshold(), pos, neg))
      } else {
        list(prob = pmat)
      }
    }
  )
)
mlr_learners$add("classif.compboost", LearnerClassifCompboost)


#suppressMessages(library(mlr3))
#suppressMessages(library(mlr3tuning))
#suppressMessages(library(mlrintermbo))
#suppressMessages(library(mlr3learners))
#suppressMessages(library(mlr3extralearners))
#suppressMessages(library(mlr3pipelines))
#suppressMessages(library(paradox))
#suppressMessages(library(R6))


#lr1 = lrn("classif.compboost", optimizer = "nesterov", use_stopper = TRUE,
  #eps_for_break = 0, patience = 2, oob_fraction = 0.3, predict_type = "prob",
  #mstop = 5000L, restart = TRUE, stop_both = TRUE, df_autoselect = TRUE,
  #oob_seed = 100)

#lr1$train(tsk("sonar"))

#length(lr1$model$cboost$getSelectedBaselearner())
#length(lr1$model$cboost_restart$getSelectedBaselearner())
#gridExtra::grid.arrange(
  #lr1$model$cboost$plotInbagVsOobRisk() + ggplot2::ylim(0, 1),
  #lr1$model$cboost_restart$plotInbagVsOobRisk() + ggplot2::ylim(0, 1),
  #ncol = 2)

#design = benchmark_grid(
  #tasks = tsk("sonar"),
  #learners = lr1,
  #resamplings = rsmp("cv", folds = 3)
#)

#bmr = benchmark(design, store_models = TRUE)
#bmr$aggregate(msrs(c("classif.auc", "classif.ce")))



