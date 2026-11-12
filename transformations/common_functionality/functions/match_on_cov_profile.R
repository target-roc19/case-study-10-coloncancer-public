#' @description
#' Performs exact matching without replacement within predefined categorical
#' covariate profiles to construct a balanced study cohort. A profile is defined 
#' as a unique combination of one or more categorical variables, specified via 
#' the `profile_var` argument.
#'
#' Within each profile, individuals are split into two groups (exposed and 
#' control, as defined by `group_var`, `exposed_label`, and `control_label`). 
#' The function determines the minimum group size within each profile and randomly 
#' samples that number of individuals from both groups, thereby ensuring equal 
#' numbers of exposed and control individuals per profile. Matching is performed 
#' without replacement.
#'
#' Profiles in which one of the groups is absent (zero exposed or zero control 
#' individuals) are excluded from the matched cohort.
#'
#' This function does not incorporate any time-based matching or alignment (e.g., 
#' no matching on index date or T0) matching is based solely on categorical 
#' covariate profiles.
#'
#' It is assumed that each individual appears only once in the input dataset (`dt`), 
#' and belongs to either the exposed or control group, but not both.
#'
#' The function returns a dataset consisting of both matched and unmatched individuals.
#' Individuals who are not selected during the matching procedure (i.e., whose
#' `person_id` is not present in the matched cohort) are retained in the output and
#' relabeled with `group_var = "unmatched"`, regardless of their original group.
#'
#' @param dt A data.table containing the input data.
#' @param profile_var Character string specifying the column that defines the profile
#'   (default is "profile").
#' @param group_var Character string specifying the column that defines group 
#'   (default is "group").
#' @param exposed_label Value in `group_var` identifying the exposed group
#'   (default is "exposed").
#' @param control_label Value in `group_var` identifying the control/control group
#'   (default is "control").
#' @param seed Optional integer to set the random seed for reproducible sampling
#'   (default is NULL).
#'   
#' @return A data.table containing matched observations, balanced within each profile.
#' 
match_on_cov_profile <- function(dt, 
                     profile_var = "profile", 
                     group_var = "group",
                     exposed_label = "exposed",
                     control_label = "control", 
                     seed = NULL) {
  
  dt <- as.data.table(dt)
  
  # Validate that the profile and group variable exist in the dt
  if (!profile_var %in% names(dt)) {
    stop(sprintf("Column '%s' specified in 'profile_var' not found in 'dt.", profile_var))
  }
  
  if (!group_var %in% names(dt)) {
    stop(sprintf("Column '%s' specified in 'group_var' not found in 'dt.", group_var))
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # store matched exposed ids
  matched_exposed_ids <- c()
  
  result <- dt[, {
    
    # Split into exposed vs control
    exp_dt  <- .SD[get(group_var) == exposed_label]
    cont_dt <- .SD[get(group_var) == control_label]
    
    # Minimum group size
    n_min <- min(nrow(exp_dt), nrow(cont_dt))
    
    # If one group is empty → return NULL (correct way)
    if (n_min == 0) {
      NULL
    } else {
      # Randomly sample n_min rows from each group
      exp_dt  <- exp_dt[sample(.N, n_min)]
      cont_dt <- cont_dt[sample(.N, n_min)]
      
      # Combine
      rbind(exp_dt, cont_dt)
    }
    
  }, by = profile_var]
  
  # Find the unmatched patients
  unmatched <- dt[!person_id %in% result$person_id]
  
  # overwrite group label to unmatched
  unmatched[, (group_var) := "unmatched"]
  
  # bind together
  final_result <- rbind(result, unmatched, fill = TRUE)
  
  return(final_result)

  }
