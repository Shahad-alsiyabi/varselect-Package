#' @title Variable Selection Pipeline Package
#' @description A pipeline-based approach to variable selection in R using method chaining.
#' @name varselect
#' @import methods
#' @importFrom stats lm glm AIC BIC cor.test kruskal.test chisq.test p.adjust predict var cor as.formula binomial coef sd model.matrix
#' @importFrom car vif
#' @importFrom caret createFolds confusionMatrix
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs theme_minimal
#' @importFrom pROC auc roc
NULL

#Helpers

calc_mallows_cp <- function(model, full_mse, full_n) {
  p <- length(coef(model))
  sse <- sum(model$residuals^2)
  return((sse / full_mse) - full_n + 2 * p)
}

calc_aicc <- function(model) {
  k <- length(coef(model))
  n <- length(model$residuals)
  aic <- AIC(model)
  return(aic + (2 * k * (k + 1)) / (n - k - 1))
}

calc_class_metrics <- function(actual, pred_prob, threshold=0.5) {
  pred_class <- factor(ifelse(pred_prob > threshold, "yes", "no"), levels=c("no","yes"))
  act_class <- factor(ifelse(actual == 1, "yes", "no"), levels=c("no","yes"))
  cm <- caret::confusionMatrix(pred_class, act_class)
  list(
    accuracy = cm$overall["Accuracy"],
    f1 = cm$byClass["F1"],
    precision = cm$byClass["Precision"],
    recall = cm$byClass["Recall"],
    mcc = (cm$table[1,1]*cm$table[2,2] - cm$table[1,2]*cm$table[2,1]) /
      sqrt((cm$table[1,1]+cm$table[1,2])*(cm$table[1,1]+cm$table[2,1])*(cm$table[2,2]+cm$table[1,2])*(cm$table[2,2]+cm$table[2,1]))
  )
}

detect_type <- function(x) {
  if (is.numeric(x)) return("numeric")
  if (length(unique(x[!is.na(x)])) == 2) return("binary")
  return("multiclass")
}

normalize_scores <- function(scores) {
  lower_is_better <- c("aic", "bic", "aicc", "rmse", "mae", "mallows_cp")
  norm <- numeric(length(scores))
  names(norm) <- names(scores)
  for (m in names(scores)) {
    val <- scores[m]
    if (m %in% lower_is_better) norm[m] <- 1 / (1 + abs(val)) else norm[m] <- val
  }
  if (max(norm) > min(norm)) norm <- (norm - min(norm)) / (max(norm) - min(norm))
  return(norm)
}


#' @title Pipeline Class for Variable Selection
#' @description S4 class to store the state of the variable selection pipeline.
#' @slot data The input data frame.
#' @slot target The name of the target variable.
#' @slot predictors Vector of predictor names.
#' @slot target_type Type of target ("numeric", "binary", "multiclass").
#' @slot selected_vars Variables selected during the selection phase.
#' @slot final_vars Final subset of variables used for the model.
#' @slot scores Statistical scores for variables.
#' @slot performance List of model performance metrics.
#' @slot model The trained model object.
#' @slot steps History of steps applied.
#' @slot full_mse MSE of the full model (used for Mallow's Cp).
#' @exportClass varselectPipeline
setClass("varselectPipeline",
         slots = list(
           data = "data.frame",
           target = "character",
           predictors = "character",
           target_type = "character",
           selected_vars = "character",
           final_vars = "character",
           scores = "numeric",
           performance = "list",
           model = "ANY",
           steps = "character",
           full_mse = "numeric"
         )
)

#' @title Chain Pipeline Steps
#' @description Enables the use of the `+` operator to chain pipeline functions.
#' @param e1 A \code{varselectPipeline} object.
#' @param e2 A function returning a modified \code{varselectPipeline} object.
#' @return A modified \code{varselectPipeline} object.
#' @export
setMethod("+", signature(e1 = "varselectPipeline", e2 = "function"),
          function(e1, e2) { invisible(e2(e1)) }
)



#' @title Initialize Pipeline
#' @description Creates a new variable selection pipeline object.
#' @param data A data frame containing target and predictors.
#' @param target A character string specifying the target variable name.
#' @return An object of class \code{varselectPipeline}.
#' @export
varselect <- function(data, target) {
  if(!target %in% names(data)) stop("Target not found")

  t_type <- detect_type(data[[target]])
  f_mse <- 0
  if(t_type == "numeric") {
    f <- as.formula(paste(target, "~ ."))
    try({
      m <- lm(f, data)
      f_mse <- sum(m$residuals^2) / m$df.residual
    }, silent=TRUE)
  }

  new("varselectPipeline",
      data = data,
      target = target,
      predictors = setdiff(names(data), target),
      target_type = t_type,
      selected_vars = character(0),
      final_vars = character(0),
      scores = numeric(0),
      performance = list(),
      model = NULL,
      steps = "Initialized",
      full_mse = f_mse
  )
}


#' @title Validate Data
#' @description Removes variables based on missingness, variance, or user exclusion.
#' @param exclude Vector of variable names to exclude manually.
#' @param missing_threshold Max allowed proportion of missing values (default 0.3).
#' @param variance_threshold Min required variance (default 0.01).
#' @return A closure that modifies the pipeline object.
#' @export
validate <- function(exclude = NULL, missing_threshold = 0.3, variance_threshold = 0.01) {
  function(obj) {
    curr <- setdiff(obj@predictors, exclude)
    miss <- sapply(curr, function(v) sum(is.na(obj@data[[v]]))/nrow(obj@data))
    curr <- curr[miss <= missing_threshold]

    vars_ok <- sapply(curr, function(v) {
      if(is.numeric(obj@data[[v]])) var(obj@data[[v]], na.rm=TRUE) > variance_threshold
      else length(unique(obj@data[[v]])) > 1
    })
    curr <- curr[vars_ok]

    obj@predictors <- curr
    obj@steps <- c(obj@steps, paste0("Validated (", length(curr), " vars)"))
    invisible(obj)
  }
}

#' @title Filter by Significance
#' @description Filters variables based on statistical tests against the target (Spearman, Kruskal-Wallis, or Chi-Square).
#' @param alpha Significance threshold (default 0.05).
#' @param correction Correction method: "fdr" (default) or "bonferroni".
#' @export
significance <- function(alpha = 0.05, correction = "fdr") {
  function(obj) {
    p_vals <- sapply(obj@predictors, function(v) {
      tryCatch({
        if (obj@target_type == "numeric") {
          cor.test(obj@data[[v]], obj@data[[obj@target]], method = "spearman")$p.value
        } else if (is.numeric(obj@data[[v]])) {
          kruskal.test(obj@data[[v]] ~ obj@data[[obj@target]])$p.value
        } else {
          chisq.test(table(obj@data[[v]], obj@data[[obj@target]]))$p.value
        }
      }, error=function(e) 1)
    })

    adj_p <- p.adjust(p_vals, method = if(correction=="fdr") "BH" else "bonferroni")
    obj@predictors <- names(adj_p[adj_p <= alpha])
    obj@steps <- c(obj@steps, paste0("Significance (", length(obj@predictors), " vars)"))
    invisible(obj)
  }
}

#' @title Refine by Multicollinearity
#' @description Removes variables using VIF and Condition Number analysis to reduce multicollinearity.
#' @param vif_threshold VIF threshold to trigger removal (default 10).
#' @param condition_threshold Condition number threshold (default 30).
#' @export
refine <- function(vif_threshold = 10, condition_threshold = 30) {
  function(obj) {
    curr <- obj@predictors

    # VIF
    repeat {
      nums <- curr[sapply(curr, function(v) is.numeric(obj@data[[v]]))]
      if(length(nums) < 2) break
      f <- as.formula(paste(obj@target, "~", paste(nums, collapse="+")))

      mod <- try(if(obj@target_type=="numeric") lm(f, obj@data) else glm(f, obj@data, family=binomial), silent=TRUE)
      if(inherits(mod, "try-error")) break

      vifs <- try(car::vif(mod), silent=TRUE)
      if(inherits(vifs, "try-error")) break
      if(is.matrix(vifs)) vifs <- vifs[,1]

      if(max(vifs) > vif_threshold) curr <- setdiff(curr, names(which.max(vifs))) else break
    }

    # Condition Number
    nums <- curr[sapply(curr, function(v) is.numeric(obj@data[[v]]))]
    if(length(nums) > 1) {
      cm <- cor(model.matrix(~.+0, data=obj@data[,nums, drop=FALSE]))
      eigs <- eigen(cm)$values
      cond_idx <- sqrt(max(eigs) / min(eigs))
      if(cond_idx > condition_threshold) {
        cor_mat <- cor(obj@data[,nums])
        diag(cor_mat) <- 0
        worst <- names(which.max(colMeans(abs(cor_mat))))
        curr <- setdiff(curr, worst)
      }
    }

    obj@predictors <- curr
    obj@steps <- c(obj@steps, "Refined (VIF/Cond)")
    invisible(obj)
  }
}

#' @title Feature Selection
#' @description Selects best features using backward elimination and weighted metrics (AIC, RMSE, Mallow's Cp, etc).
#' @param method Selection method (default "backward").
#' @param metrics Vector of metrics. Choices: "aic", "bic", "aicc", "mallows_cp", "rmse", "mae", "r2_adj", "spearman", "auc", "f1", "mcc".
#' @param weights Numeric vector of weights corresponding to metrics.
#' @param cv_folds Number of cross-validation folds (default 10).
#' @param top Number of variables to select ("auto" or integer).
#' @export
#' @export
#' @export
#' @export
selection <- function(method = "backward", metrics = c("aic", "rmse"), weights = NULL, cv_folds = 10, top = "auto") {
  function(obj) {
    if(is.null(weights)) weights <- rep(1/length(metrics), length(metrics))

    # --- Scoring Helper ---
    calc_score <- function(vars) {
      if(length(vars)==0) return(-Inf)
      f <- as.formula(paste(obj@target, "~", paste(vars, collapse="+")))
      m_vals <- numeric(length(metrics)); names(m_vals) <- metrics

      mod <- if(obj@target_type=="numeric") lm(f, obj@data) else glm(f, obj@data, family=binomial)

      if("aic" %in% metrics) m_vals["aic"] <- AIC(mod)
      if("bic" %in% metrics) m_vals["bic"] <- BIC(mod)
      if("aicc" %in% metrics) m_vals["aicc"] <- calc_aicc(mod)
      if("mallows_cp" %in% metrics && obj@target_type=="numeric") m_vals["mallows_cp"] <- calc_mallows_cp(mod, obj@full_mse, nrow(obj@data))
      if("r2_adj" %in% metrics && obj@target_type=="numeric") m_vals["r2_adj"] <- summary(mod)$adj.r.squared
      if("spearman" %in% metrics) {
        cors <- sapply(vars, function(v) abs(cor(as.numeric(obj@data[[v]]), as.numeric(obj@data[[obj@target]]), method="spearman")))
        m_vals["spearman"] <- mean(cors, na.rm=TRUE)
      }

      if(any(c("rmse", "mae", "auc", "f1", "mcc") %in% metrics)) {
        folds <- caret::createFolds(obj@data[[obj@target]], k=cv_folds)
        preds <- numeric(nrow(obj@data))
        for(i in 1:cv_folds) {
          idx <- folds[[i]]
          m_cv <- if(obj@target_type=="numeric") lm(f, obj@data[-idx,]) else glm(f, obj@data[-idx,], family=binomial)
          preds[idx] <- predict(m_cv, obj@data[idx,], type="response")
        }
        act <- obj@data[[obj@target]]
        if(obj@target_type=="numeric") {
          if("rmse" %in% metrics) m_vals["rmse"] <- sqrt(mean((act-preds)^2))
          if("mae" %in% metrics) m_vals["mae"] <- mean(abs(act-preds))
        } else {
          y_bin <- ifelse(act == levels(factor(act))[2], 1, 0)
          if("auc" %in% metrics) m_vals["auc"] <- tryCatch(as.numeric(pROC::auc(y_bin, preds)), error=function(e) 0.5)
        }
      }
      sum(normalize_scores(m_vals) * weights, na.rm=TRUE)
    }


    curr <- obj@predictors

    repeat {
      # stop if we reach target
      if(top != "auto" && length(curr) <= as.numeric(top)) break

      # stop if we run out of vars
      if(length(curr) <= 1) break

      curr_sc <- calc_score(curr)
      best_drop <- NULL
      best_sc <- -Inf

      # get highest score
      for(v in curr) {
        sc <- calc_score(setdiff(curr, v))
        if(sc > best_sc) {
          best_sc <- sc
          best_drop <- v
        }
      }

      #auto
      if(top != "auto") {
        # force drop
        curr <- setdiff(curr, best_drop)
      } else {
        if(best_sc > curr_sc) {
          curr <- setdiff(curr, best_drop)
        } else {
          break
        }
      }
    }

    obj@selected_vars <- curr
    obj@final_vars <- curr
    obj@steps <- c(obj@steps, paste0("Selection (", method, ")"))
    invisible(obj)
  }
}

#' @title Assess Prediction
#' @description Calculates final performance metrics (RMSE, MAE, R2 for regression; AUC for classification) using Cross-Validation.
#' @param metric Primary metric to display (default "rmse").
#' @param cv_folds Number of CV folds for validation.
#' @export
prediction <- function(metric = "rmse", cv_folds = 10) {
  function(obj) {
    if(length(obj@final_vars)==0) return(invisible(obj))
    f <- as.formula(paste(obj@target, "~", paste(obj@final_vars, collapse="+")))

    folds <- caret::createFolds(obj@data[[obj@target]], k=cv_folds)
    preds <- numeric(nrow(obj@data))
    for(i in 1:cv_folds) {
      idx <- folds[[i]]
      m <- if(obj@target_type=="numeric") lm(f, obj@data[-idx,]) else glm(f, obj@data[-idx,], family=binomial)
      preds[idx] <- predict(m, obj@data[idx,], type="response")
    }

    act <- obj@data[[obj@target]]
    if(obj@target_type == "numeric") {
      obj@performance <- list(RMSE = sqrt(mean((act-preds)^2)), MAE = mean(abs(act-preds)), R2 = cor(act, preds)^2)
    } else {
      y_bin <- ifelse(act == levels(factor(act))[2], 1, 0)
      obj@performance <- list(AUC = tryCatch(as.numeric(pROC::auc(y_bin, preds)), error=function(e) NA))
    }
    obj@steps <- c(obj@steps, "Prediction (CV)")
    invisible(obj)
  }
}

#' @title Train Final Model
#' @description Trains the final model (lm or glm) on the full dataset using the selected variables.
#' @export
train <- function() {
  function(obj) {
    vars <- if(length(obj@final_vars)==0) "1" else paste(obj@final_vars, collapse="+")
    f <- as.formula(paste(obj@target, "~", vars))
    obj@model <- if(obj@target_type=="numeric") lm(f, obj@data) else glm(f, obj@data, family=binomial)
    obj@steps <- c(obj@steps, "Trained")
    invisible(obj)
  }
}

#' @title Visualize Importance
#' @description Plots variable importance based on final model coefficients.
#' @param type Type of plot (default "importance").
#' @param top Number of top variables to show.
#' @export
visualize <- function(type="importance", top = 5, col = "steelblue") {
  function(obj) {
    if(!is.null(obj@model) && type == "importance") {
      imp <- abs(coef(obj@model)[-1])
      if(length(imp) > 0) {
        df <- data.frame(Variable=names(imp), Score=as.numeric(imp))
        df <- df[order(-df$Score),][1:min(top, nrow(df)),]

        print(ggplot2::ggplot(df, ggplot2::aes(x=reorder(Variable, Score), y=Score)) +
                ggplot2::geom_bar(stat="identity", fill=col) + # Uses the 'col' argument here
                ggplot2::coord_flip() +
                ggplot2::theme_minimal() +
                ggplot2::labs(title="Variable Importance", subtitle="Based on Model Coefficients"))
      }
    }
    invisible(obj)
  }
}

#' @title Show Results
#' @description Prints the selected variables, performance metrics, and pipeline steps.
#' @param show_scores Logical. Note: Detailed scores require verbose mode (not yet implemented).
#' @export
show_vars <- function(show_scores = FALSE) {
  function(obj) {
    print(list(Selected = obj@final_vars, Performance = obj@performance, Steps = obj@steps))
    invisible(obj)
  }
}

#' @title Full Selection Pipeline Wrapper
#' @description A convenience wrapper that runs the complete default pipeline: Validate -> Significance -> Refine -> Selection -> Prediction.
#' @return A function that applies the pipeline steps to a \code{varselectPipeline} object.
#' @export
fullselection <- function() {
  function(obj) obj + validate() + significance() + refine() + selection() + prediction()
}
