.identify_multilevel = function(x_i, mult.level, verbose, r) {
  #### Multivariate situations ------
  if (any(x_i$duplicate == TRUE)) {

    x_i$all_vals_study = paste0("study: '", x_i$author, " (", x_i$year, ")' contains multiple ", x_i$multiple_es)

    if (mult.level == FALSE) {
      stop(paste(paste(unique(x_i$all_vals_study[x_i$duplicate == TRUE]), collapse = " / "), " and is repeated several times in your dataset. \nPlease, check that it is not a repeated entry. If not, indicate that you have multivariate data by specfying 'mult.level = TRUE' as an argument of the 'umbrella' function."))
    }

    ## if the input dataset has a multivariate structure, we create a message to indicate whether each study with multiple outcomes has been handled as having multiple groups or outcomes
    if (verbose) {
      message(paste("In factor '", unique(x_i$factor), "': \n",
                    paste("-", unique(x_i[x_i$duplicate == TRUE, ]$all_vals_study), collapse = "\n"), sep = ""))
    }
    # if the input dataset has multiple outcomes but with no r associated, we create a message to warn users about the r value used
    if (any(x_i$multiple_es %in% c("outcome", "Outcome", "outcomes", "Outcomes") & is.na(x_i$r))) {
      if (verbose) message(paste("In factor '", unique(x_i$factor), "' some studies have multiple outcomes but they are not associated with any within-study correlation (which can be indicated in the 'r' column of the dataset). A value of r = ", r, " is assumed.", sep = ""))
    }
    if (any(x_i$multiple_es %in% c("outcome", "Outcome", "outcomes", "Outcomes", "Group", "Groups", "group", "groups") &
            x_i$duplicate == FALSE)) {
      if (verbose) message(paste("\nIn factor '", unique(x_i$factor), "' some studies have an identifier of multiple studies, but are not duplicated. Carefully check the 'multiple_es' entries. We assumed it was a mistake and remove the multiple identifier tag.", sep = ""))
      row_problems = which(x_i$multiple_es %in% c("outcome", "Outcome", "outcomes", "Outcomes", "Group", "Groups", "group", "groups") &
                             x_i$duplicate == FALSE)
      # message(row_problems)
      x_i$multiple_es[row_problems] <- NA
      # message(x_i$multiple_es)
    }

    attr(x_i, "REPEATED_STUDIES") <- TRUE
  } else {
    attr(x_i, "REPEATED_STUDIES") <- FALSE
  }
  x_i
 }
