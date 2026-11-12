#' Fit a Propensity Score Model With Fallback Error Handling
#'
#' Fits a binomial propensity score model for `group_binary` using the supplied
#' predictor names. By default, the function first attempts to fit the model
#' with [speedglm::speedglm()] and falls back to [stats::glm()] if `speedglm`
#' fails.
#'
#' Warning and error messages are captured and written to the log with
#' [logger::log_info()].
#'
#' @param predictors A character vector of column names to include on the
#'   right-hand side of the model formula.
#' @param cohort_covariates A data frame containing `group_binary` and all
#'   variables listed in `predictors`.
#' @param use_speedglm A logical scalar indicating whether to try
#'   [speedglm::speedglm()] before falling back to [stats::glm()]. Defaults to
#'   `TRUE`.
#'
#' @return A fitted model object produced by [speedglm::speedglm()] or
#'   [stats::glm()]. Returns `NULL` when fitting fails and no fallback result is
#'   available.
#'
#' @examples
#' cohort_covariates <- data.frame(
#'   group_binary = c(0, 1, 0, 1),
#'   age = c(44, 51, 39, 63),
#'   bmi = c(24.1, 30.5, 27.2, 29.8)
#' )
#' fit_ps_model(c("age", "bmi"), cohort_covariates, use_speedglm = FALSE)
#' @export
fit_ps_model <- function(predictors, cohort_covariates, use_speedglm = TRUE) {
  # Initialize result and warning message
  result <- NULL
  warning_msg <- NULL
  error_msg <- NULL
  model_method <- "speedglm"

  # Data quality check: remove predictors with < 2 unique values (zero variance)
  valid_predictors <- c()
  dropped_predictors <- c()
  for (pred in predictors) {
    n_unique <- length(unique(cohort_covariates[[pred]]))
    if (n_unique < 2) {
      dropped_predictors <- c(dropped_predictors, pred)
      logger::log_warn(paste0(
        "Dropping predictor '",
        pred,
        "' with only ",
        n_unique,
        " unique value(s)."
      ))
    } else {
      valid_predictors <- c(valid_predictors, pred)
    }
  }

  if (length(valid_predictors) == 0) {
    logger::log_error("All predictors have zero variance. Cannot fit model.")
    return(NULL)
  }

  if (length(dropped_predictors) > 0) {
    logger::log_info(paste0(
      "Removed ",
      length(dropped_predictors),
      " predictor(s) with zero variance: ",
      paste(dropped_predictors, collapse = ", ")
    ))
  }

  # remove SV_AGE integer from valid predictors. 
  if ("SV_AGE" %in% valid_predictors) {
    spline_term <- 'splines::ns(SV_AGE, df = 4)' # need to confirm the degrees of freedom for the spline term
    other_predictors <- setdiff(valid_predictors, "SV_AGE")
    terms <- c(other_predictors, spline_term)
  } else {
  terms <- valid_predictors
  }

  model_formula <- as.formula(paste0(
    "group_binary ~ ",
    paste(terms, collapse = " + ")
  ))

  # Define a helper to run glm with warning capture
  run_glm_with_warnings <- function(model_formula, cohort_covariates) {
    warning_msg <<- NULL
    model_method <<- "glm"
    withCallingHandlers(
      expr = glm(
        formula = model_formula,
        data = cohort_covariates,
        family = "binomial"
      ),
      warning = function(w) {
        warning_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }

  # Try speedglm first, catch errors and fallback to glm
  if (use_speedglm) {
    result <- tryCatch(
      withCallingHandlers(
        expr = speedglm::speedglm(
          formula = model_formula,
          data = cohort_covariates,
          family = binomial(),
          fitted = TRUE
        ),
        warning = function(w) {
          warning_msg <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        error_msg <<- conditionMessage(e)
        logger::log_info(paste0(
          "speedglm failed with error: ",
          error_msg,
          ". Falling back to glm()."
        ))
        run_glm_with_warnings(model_formula, cohort_covariates)
      }
    )
  } else {
    result <- tryCatch(
      withCallingHandlers(
        expr = run_glm_with_warnings(model_formula, cohort_covariates),
        warning = function(w) {
          warning_msg <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        error_msg <<- conditionMessage(e)
        logger::log_info(paste0("glm failed with error: ", error_msg))
      }
    )
  }

  # Log the method used
  logger::log_info(paste0("Model fit using: ", model_method))

  # Log any warning message
  if (!is.null(warning_msg)) {
    logger::log_info(paste0("GLM model ran with warning: ", warning_msg))
  }

  return(result)
}
