#' This function checks the input data to have the correct format and that it has no incongruencies
#'
#' @param x
#'
#' @noRd
.check_data = function (x) {

  #### Check input type ----------------

  # we check if the input object is a dataframe. if it is not, we display an error message
  if (!("data.frame" %in% class(x))) {
    stop("The object passed to the umbrella function is not a data.frame object.")
  } else if (length(class(x)) > 1 & "data.frame" %in% class(x)) { # if it is not only a dataframe, we convert it
    x <- data.frame(x)
  }

  if (length(colnames(x)) == 0) {
    stop("Dataframe passed to the umbrella() function has no column. Check format of the dataset.")
  } else if (nrow(x) == 0) {
    stop("Dataframe passed to the umbrella() function has no row. Check format of the dataset.")
  }
  # JAMOVI
  # if (length(colnames(x)) == 0 | nrow(x) == 0) {
  #   stop("No dataset detected. Load (or reload) your dataset and drag-and-drop appropriate column names to the 'List of variables' selector.")
  # }


  #### Initialize some settings ------
  status <- "OK" # this is the general status of the whole analysis
  error_msgs <- c() # this will contain all errors and warnings for each study
  row_wrong <- c() # row of error
  col_wrong <- c() # col of error
  column_errors <- rep("", nrow(x))
  column_type_errors <- rep("", nrow(x))
  situation <- rep("", nrow(x))
  x$row_index <- 1:nrow(x)
  x$duplicate <- FALSE
  returned <- paste0("Checkings finished.") # object returned

  ###########################################################################################################
  ###################################### CHECK 1: columns ###################################################
  ###########################################################################################################



  #### Multiple paper identification columns ----------------

  # if author year is mixed (name2009) then split it up
  if ("study" %in% colnames(x) &
      !"author" %in% colnames(x) &
      !"year" %in% colnames(x)) {
    x$author = x$year = NA
    x <- .split_study_into_author_year(x)
  }

  if (("study" %in% colnames(x)) && ("author" %in% colnames(x) || "year" %in% colnames(x))) {
    status <- "ERROR"
    error_msgs <- append(error_msgs,
                         paste0("There are both a column called 'study' and a column called 'author' or 'year'."))
  }


  #### Overall columns check ----------------

  #### Mandatory and optional columns
  if (any(c("OR", "RR", "IRR", "logOR", "logRR", "logIRR", "R", "Z") %in% x$measure | all(is.na(x$measure)))) {
    # column names
    mandatory_cols <- c("meta_review", "factor", "author", "year", "measure")
    other_expected_cols <- c("analysis", "discard",
                             "value", "var", "se", "ci_lo", "ci_up",
                             "n_sample", "n_exp", "n_nexp",
                             "n_cases", "n_controls",
                             "n_cases_exp", "n_cases_nexp", "n_controls_exp", "n_controls_nexp",
                             "prop_cases", "prop_controls",
                             "mean_cases", "sd_cases", "mean_controls", "sd_controls",
                             "mean_pre_cases", "sd_pre_cases", "mean_pre_controls", "sd_pre_controls", "pre_post_cor",
                             "mean_change_cases", "sd_change_cases", "mean_change_controls", "sd_change_controls",
                             "time", "time_exp", "time_nexp",
                             "shared_controls", "shared_nexp", "thr",
                             "rob", "amstar", "multiple_es", "r", "reverse_es")
    # column types
    mandatory_cols_types <- c("factor","factor", "factor","factor", "factor")
    other_expected_cols_types <- c("numeric", "factor",
                                   "numeric", "numeric", "numeric", "numeric","numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "factor", "factor", "numeric",
                                   "factor", "numeric", "factor", "numeric","factor")
  } else {
    # column names
    mandatory_cols <- c("meta_review", "factor", "author", "year", "measure", "n_cases", "n_controls")
    other_expected_cols <- c("analysis", "discard",
                             "value", "var", "se", "ci_lo", "ci_up",
                             "n_sample", "n_exp", "n_nexp",
                             "n_cases_exp", "n_cases_nexp", "n_controls_exp", "n_controls_nexp",
                             "prop_cases", "prop_controls",
                             "mean_cases", "sd_cases", "mean_controls", "sd_controls",
                             "mean_pre_cases", "sd_pre_cases", "mean_pre_controls", "sd_pre_controls", "pre_post_cor",
                             "mean_change_cases", "sd_change_cases", "mean_change_controls", "sd_change_controls",
                             "time", "time_exp", "time_nexp",
                             "shared_controls", "shared_nexp", "thr",
                             "rob", "amstar", "multiple_es", "r", "reverse_es")

    # column types
    mandatory_cols_types <- c("factor", "factor", "factor","factor","factor",
                              "numeric", "numeric")
    other_expected_cols_types <- c("numeric", "factor",
                                   "numeric","numeric","numeric","numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric","numeric","numeric","numeric",
                                   "numeric", "numeric",
                                   "numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric", "numeric",
                                   "numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric",
                                   "factor","factor","numeric",
                                   "factor", "numeric", "factor", "numeric", "factor")
  }

  colnames(x) <- tolower(colnames(x))

  # remove all rows that should be discarded from analyses
    if (any(x$discard %in% c("yes", "Yes", "remove", "removed", "TRUE", TRUE))) {
      removed_rows <- which(x$discard %in% c("yes", "Yes", "remove", "removed", "TRUE", TRUE))
      status <- ifelse(status == "ERROR", "ERROR", "WARNING")
      error_msgs <- append(error_msgs, paste0("Some rows of the original dataset have been removed based on the 'discard' column inputs: rows = ", paste(removed_rows, collapse = ", "), " (only a warning, not an error)."))
      column_errors = column_errors[-removed_rows]
      column_type_errors = column_type_errors[-removed_rows]
      x <- subset(x, !x$discard %in% c("yes", "Yes", "remove", "removed", "TRUE", TRUE))
      situation <- situation[-removed_rows]
    }

  #### check if mandatory columns and optional columns are all available
  if (!all(mandatory_cols %in% colnames(x))) {
    status <- "ERROR"
    error_msgs <- append(error_msgs,
                         paste("The following required variables are missing: ",
                               paste(mandatory_cols[which(!mandatory_cols %in% colnames(x))],
                                     collapse = ", ")))
  }

  other_expected_cols_available <- other_expected_cols[which(other_expected_cols %in% colnames(x))]

  ####  Create NA columns for missing optional columns
  for (col in other_expected_cols) {
    if (!(col %in% colnames(x))) {
      x[, col] <- NA
    }
  }

  #### set "na", "inf" or blank as NA
  x[x == "" | x == " " | x == "NaN" | x == "na" | x == "NA" | x == "n/a" | x == "N/A" | x == "inf" | x == "infinity" | x == "Infinity" | x == "INFINITY" | x == "INF" | x == "Inf"] <- NA

  #### remove rows that do not contain author or study
  if (("study" %in% colnames(x)) & nrow(x[is.na(x$study),]) > 0) {
    missing_rows <- which(is.na(x$study))
    status <- ifelse(status == "ERROR", "ERROR", "WARNING")
    error_msgs <- append(error_msgs, paste0("Some rows of the original dataset Have been removed because they do not include any value in 'study' column: rows = ", paste(missing_rows, collapse = ", "), " (only a warning, not an error)."))
    column_errors = column_errors[which(!is.na(x$study))]
    column_type_errors = column_type_errors[which(!is.na(x$study))]
    x <- x[!is.na(x$study),]
    situation <- situation[which(!is.na(x$study))]
  } else if (("author" %in% colnames(x)) & nrow(x[is.na(x$author),]) > 0) {
    missing_rows <- which(is.na(x$author))
    status <- ifelse(status == "ERROR", "ERROR", "WARNING")
    error_msgs <- append(error_msgs, paste0("Some rows of the original dataset have been removed because they do not include any value in 'author' column: rows = ", paste(missing_rows, collapse = ", "), " (only a warning, not an error)."))
    column_errors = column_errors[which(!is.na(x$author))]
    column_type_errors = column_type_errors[which(!is.na(x$author))]
    x <- x[!is.na(x$author),]
    situation <- situation[which(!is.na(x$author))]
  }


  #### support intervention instead of factor column for intervention studies
  if ("intervention" %in% colnames(x) & !("factor" %in% colnames(x))) {
    x$factor = x$intervention
  }

  #### load x, convert all "NA" values to a standard NA and parse all supported infinity possible names

  #### Convert numerical columns to numeric
  all_cols = c(mandatory_cols, other_expected_cols)
  all_cols_types = c(mandatory_cols_types, other_expected_cols_types)
  for (j in 1:length(all_cols)) {
    idx = which(colnames(x) == all_cols[j])
    x[, idx] = as.character(x[, idx])
    if (all_cols_types[j] == "numeric") {

      # check the presence of . and , in numeric columns
      if (any(grepl("\\.", x[, idx]) & grepl(",", x[, idx]))) {
        commas_and_points <- which(grepl("\\.", x[, idx]) & grepl(",", x[, idx]))
        status <- "ERROR"
        error_msgs <- append(error_msgs, paste0("A numeric cell in the column '", paste(colnames(x)[idx]), "' contains both '.' & ',', and thus cannot be safely converted to a numeric format. Please, homogeneise the notation."))
        column_errors[commas_and_points] = paste(column_errors[commas_and_points], "Column '", paste(colnames(x)[idx]), "' contains both '.' and ','. //")
        column_type_errors[commas_and_points] = "ERROR"
      } else if (any(grepl(",", x[, idx]))) {
        commas <- which(grepl(",", x[, idx]))
        status <- ifelse(status == "ERROR", "ERROR", "WARNING")
        error_msgs <- append(error_msgs, paste0("A numeric cell in the column '", paste(colnames(x)[idx]), "' contains a ',' that was converted to a '.' (this is only a warning, not an error)."))
        column_errors[commas] = paste(column_errors[commas], "A ',' has been converted to a '.' in column '", paste(colnames(x)[idx]), "' (this is only a warning, not an error). //")
        column_type_errors[commas] = ifelse(column_type_errors[commas] == "ERROR", "ERROR", "WARNING")
        x[, idx] <- gsub(",", ".", x[, idx])
      }

      # check the presence of non-numeric characters in numeric columns
      if (any(suppressWarnings(is.na(as.numeric(as.character(na.omit(x[, idx]))))))) {
        not_num <- x[which(!is.na(x[, idx])), ][suppressWarnings(is.na(as.numeric(as.character(na.omit(x[, idx]))))),]$row_index
        status <- "ERROR"
        error_msgs <- append(error_msgs, paste0("The dataframe contains non-numeric characters while this is expected. Please check inputs."))
        column_errors[not_num] = paste(column_errors[not_num], "Non-numeric characters ('", paste(unique(x[not_num, idx]), collapse = " ' / ' "), "') in column '", paste(colnames(x)[idx], "'. //"))
        column_type_errors[not_num] = "ERROR"
      }

      # convert numeric columns to numeric format
      x[, idx] = .as_numeric(x[, idx])
    }
  }


  #### convert factors to characters to allow entering new values
  for (col_idx in colnames(x)) {
    if (is.factor(x[col_idx])) {
      x[col_idx] = as.character(x[col_idx])
    }
  }
  #### copy the parameters from first row to the next ones of same study.
  if (dim(x)[1] > 1) {
    columns_to_copy = intersect(c("meta_review"), colnames(x)) #c("meta_review", "amstar")
    for (col_idx in columns_to_copy) {
      for (row_idx in 2:dim(x)[1]) {
        if(is.na(x[row_idx, col_idx]) & row_idx > 1) {
          if(is.na(x[row_idx, "meta_review"]) || (col_idx > 1 && x[row_idx - 1, "meta_review"] == x[row_idx, "meta_review"])) {
            x[row_idx, col_idx] <- x[row_idx - 1, col_idx]
          } else {
            x[row_idx, col_idx] <- NA
          }
        }
      }
    }
  }

  ###########################
  #### interim checkings ####
  ###########################

  if (status == "ERROR") {
    df <- cbind(column_type_errors, column_errors, row_index = x$row_index, subset(x, select =  -c(row_index)))
    attr(returned, "message") <- paste("ERROR:\n-", paste(unique(error_msgs), collapse = "\n- "))
    attr(returned, "status") <- "ERRORS"
    attr(returned, "data") <- df
    return(returned)
    stop()
  }

  #################################### END CHECK 1 ##################################################

  ###########################################################################################################
  ###################################### CHECK 2: cell ###################################################
  ###########################################################################################################

  #### fix n_cases and n_controls for genetic studies
  x <- .fix_n_in_allelic_analyses(x)

  #### Overall inputs check ----------------
  # check which columns contain input errors
  measure_vals = c("SMD", "SMC", "R", "Z", "OR", "HR", "IRR", "RR", "MD", "G", "logOR", "logRR", "logHR", "logIRR")
  measure_warning_vals = c('log2 fold change')
  cols_no_na = c('measure')

  n_studies = max(nrow(x))
  x$measure = as.character(x$measure) # Otherwise some correct measures may be converted to NA

  # Iterate all fields, and check whether each field has the properties expected.
  # All errors will be appended in two columns that will detail what failed to a study
  for (i in 1:n_studies) {
    # substitute supported names for the different measures. i.e.: D will be converted to SMD
    x[i, "measure"] = .fix_measure_name(x[i, "measure"])
    x[i, "factor"] = trimws(x[i, "factor"])
    for (col in append(mandatory_cols, other_expected_cols_available)) {
      cell_value = x[i, col]

      if (!is.na(cell_value)) {

        switch(col,
               'measure' = {
                 # check measure value, should be OR, SMD, IRR or RR
                 if (!(cell_value %in% measure_vals)) { # if unknown measure
                   if (!(cell_value %in% measure_warning_vals)) {
                     status <- "ERROR"
                     error_msgs <- append(error_msgs, paste("Measure '", cell_value, "' not supported. Should be either 'SMD', 'SMC', 'R', 'Z', 'OR', 'HR', 'IRR', 'RR', 'MD', 'G'"))
                     column_errors[i] = paste(column_errors[i], "Measure '", cell_value, "' not supported. Should be either 'SMD', 'SMC', 'R', 'Z', 'OR', 'HR', 'IRR', 'RR', 'MD', 'G'. //")
                     column_type_errors[i] = "ERROR"
                     i = i + 1
                   } else {
                     status <- "ERROR"
                     error_msgs <- append(error_msgs, paste("Measure '", cell_value, "' not supported. Should be either 'SMD', 'SMC', 'R', 'Z', 'OR', 'HR', 'IRR', 'RR', 'MD', 'G'."))
                     column_errors[i] = paste(column_errors[i], " Measure '", cell_value, "' not supported. Should be either 'SMD', 'SMC', 'R', 'Z', 'OR', 'HR', 'IRR', 'RR', 'MD', 'G'. //")
                     column_type_errors[i] = "ERROR"
                     i = i + 1
                   }
                 }
               },

               'value' = {
                 # check whether the value for ratios is not < 0
                if (!(x[i, "measure"] %in% c("SMD", "SMC", "R", "Z", "G", "MD")) && cell_value <= 0 && !x[i, "measure"] %in% measure_warning_vals) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste0("Wrong value. Values should be positive for 'OR', 'RR', 'HR' and 'IRR'."))
                   column_errors[i] = paste(column_errors[i], "Wrong value. Should be a positive number for 'OR', 'RR', 'HR' and 'IRR'. //")
                   column_type_errors[i] = status
                 }
               },

               'se' = ,
               'var' = {
                 if (cell_value <= 0) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Standard error and variance of the effect size should be a positive number."))
                   column_errors[i] = paste(column_errors[i], "Wrong 'se' or 'var' should be > 0. //")
                   column_type_errors[i] = status
                 }
               },

               'ci_lo' = {
                 if ( ((cell_value <= 0) & !(x[i,"measure"] %in% c("SMD", "SMC", "R", "Z", "G", "MD"))) | (sum(- x[i, "value"], cell_value, na.rm = TRUE) > 0 & !is.na(x[i, "value"])) ) {
                     status <- "ERROR"
                     error_msgs <- append(error_msgs, paste("Lower bound of the 95% CI should be lower than 'value' and should not be negative or null with 'OR', 'RR', 'IRR' and 'HR' measures."))
                     column_errors[i] = paste(column_errors[i], "Wrong 'ci_lo', should be lower than 'value' and should be > 0 with OR RR HR IRR. //")
                     column_type_errors[i] = status
                   }
                 },

               'ci_up' = {
                   if ( ((cell_value <= 0) & !(x[i,"measure"] %in% c("SMD", "SMC", "R", "Z", "G", "MD"))) | (sum(- cell_value, x[i, "value"], na.rm = TRUE) > 0 & !is.na(x[i, "value"])) ) {
                     status <- "ERROR"
                     error_msgs <- append(error_msgs, paste("Upper bound of the 95% CI should be higher than 'value' and should not be negative or null with 'OR', 'RR', 'IRR' and 'HR' measures."))
                     column_errors[i] = paste(column_errors[i], "Wrong 'ci_up', should be higher than 'value' and should be > 0 with OR RR HR IRR. // ")
                     column_type_errors[i] = status
                   }
                 },

               'n_exp' = ,
               'n_nexp' = ,
               'n_cases' = ,
               'n_controls' = {
                 if (cell_value < 0) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste(" Wrong sample size in columns n_exp/n_nexp/n_cases/n_controls: should be positive a number."))
                   column_errors[i] = paste(column_errors[i], " Wrong sample size in columns n_exp/n_nexp/n_cases/n_controls: should be positive a number. //")
                   column_type_errors[i] = status
                 }
               },
               'time' = ,
               'time_exp' = ,
               'time_nexp' = {
                 if (cell_value <= 0) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste(" Wrong number in columns time/time_exp/time_nexp: should be a positive number."))
                   column_errors[i] = paste(column_errors[i], " Wrong sample size in columns time/time_exp/time_nexp: should be a positive number. //")
                   column_type_errors[i] = status
                 }
               },
               'n_cases_exp' = ,
               'n_cases_nexp' = ,
               'n_controls_exp' = ,
               'n_controls_nexp' = {
                 if (cell_value < 0) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong sample size in columns n_cases_exp/n_cases_nexp/n_controls_exp/n_controls_nexp: should be a positive number."))
                   column_errors[i] = paste(column_errors[i], " Wrong sample size in columns n_cases_exp/n_cases_nexp/n_controls_exp/n_controls_nexp: should be a positive number. //")
                   column_type_errors[i] = status
                 }
               },

               'sd_cases' = ,
               'sd_controls' = {
                 if (cell_value <= 0) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong numbers in sd_cases/sd_controls: should be a positive number."))
                   column_errors[i] = paste(column_errors[i], " Wrong numbers in sd_cases/sd_controls: should be a positive number. //")
                   column_type_errors[i] = status
                 }
               },

               'thr' = {
                 if (cell_value < 0) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste(" Wrong thr value, it should be a positive number."))
                   column_errors[i] = paste(column_errors[i], " Wrong thr, it should be a positive number. // ")
                   column_type_errors[i] = status
                 }
               },

               'rob' = {
                 if (!(cell_value %in% c("high", "High", "unclear", "Unclear", "low", "Low"))) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong risk of bias: should be either high/unclear/low."))
                   column_errors[i] = paste(column_errors[i],
                                            " Wrong risk of bias (rob) value. Should be 'high', 'unclear' or 'low'. //")
                   column_type_errors[i] = status
                 }
               },
               'amstar' = {
                 if (cell_value < 0) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong AMSTAR value. Should be a positive number."))
                   column_errors[i] = paste(column_errors[i],
                                            " Wrong AMSTAR value. Should be a positive number. // ")
                   column_type_errors[i] = status}
               },
               'multiple_es' = {
                 if (!(cell_value %in% c("unique", "Unique", "group", "Group", "groups", "Groups", "outcome", "Outcome", "outcomes", "Outcomes", ""))) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong identifier for studies with multiple effect sizes: should be 'groups' or 'outcomes'."))
                   column_errors[i] = paste(column_errors[i],
                                            " Wrong identifier for studies with multiple effect sizes: should be 'groups' or 'outcomes'. //")
                   column_type_errors[i] = status}
               },
               'r' = {
                 if (cell_value <= - 1 | cell_value >= 1) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong within-study correlation between outcomes. Should be >= -1 & <= 1."))
                   column_errors[i] = paste(column_errors[i],
                                            " Wrong within-study correlation between outcomes. Should be >= -1 & <= 1. // ")
                   column_type_errors[i] = status}
               },
               'reverse_es' = {
                 if (!(cell_value %in% c("reverse", "Reverse", "reversed", "Reversed"))) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong identifier for studies in which the direction of the effect size must be reversed. Should be 'reverse'."))
                   column_errors[i] = paste(column_errors[i],
                                            " Wrong identifier for studies in which the direction of the effect size must be reversed. Should be 'reverse'. //")
                   column_type_errors[i] = status}
               }
        )
      } else {
        if (col %in% cols_no_na) {
          status <- "ERROR"
          error_msgs <- append(error_msgs, paste0("Measure cannot be empty or NA."))
          column_errors[i] = paste(column_errors[i], " Wrong measure: measure cannot be empty or NA. // ")
          column_type_errors[i] = status
        }
      }
    }
  }
  ###########################
  #### interim checkings ####
  ###########################
  if (status == "ERROR") {
    df <- cbind(column_type_errors, column_errors, row_index = x$row_index, subset(x, select =  -c(row_index)))
    attr(returned, "message") <- paste("ERROR:\n-", paste(unique(error_msgs), collapse = "\n- "))
    attr(returned, "status") <- "ERRORS"
    attr(returned, "data") <- df
    return(returned)
    stop()
  }

  # remove
  ###########################################################################################################
  ###################################### CHECK 3: study ###################################################
  ###########################################################################################################

  for (row in 1:nrow(x)) {

    ########################################
    ## check inputs regarding sample size ##
    ########################################

    if ( (x[row, "measure"] %in% c("SMD", "G", "MD", "SMC")) && (is.na(x[row, "n_cases"]) || is.na(x[row, "n_controls"])) ) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("For SMD, SMC, MD and G the number of cases and controls is mandatory"))
      column_errors[row] = paste(column_errors[row], " For SMD, SMC, MD, G the number of cases/controls is mandatory. // ")
      column_type_errors[row] = status
    }

    if ( (x[row, "measure"] %in% c("R", "Z")) && (is.na(x[row, "n_sample"]))) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("For R and Z, the total number of participants in the sample is mandatory"))
      column_errors[row] = paste(column_errors[row], " For R and Z the total number of participants in the sample is mandatory. // ")
      column_type_errors[row] = status
    }

    if (x[row, "measure"] %in% c("logHR", "HR") && is.na(x[row, "n_cases"])) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("For HR measure, the number of cases is mandatory"))
      column_errors[row] = paste(column_errors[row], " For HR measure, the number of cases is mandatory. // ")
      column_type_errors[row] = status
    }

    if ( (x[row, "measure"] %in% c("logIRR", "IRR")) &&
         ((is.na(x[row, "n_cases"]) &&
          (is.na(x[row, "n_cases_exp"]) || is.na(x[row, "n_cases_nexp"])))) ) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("For IRR measure, only one group between (n_cases), and (n_cases_exp, n_cases_nexp) can be empty, not both.")
      )
      column_errors[row] = paste(column_errors[row], " For IRR measure, only one group between (n_cases), and (n_cases_exp, n_cases_nexp) can be empty, not both. // ")
      column_type_errors[row] = status
    }

    if ((x[row, "measure"] %in% c("logOR", "OR")) &&
        ((is.na(x[row, "n_cases"]) || is.na(x[row, "n_controls"])) &&
         (is.na(x[row, "n_exp"]) || is.na(x[row, "n_nexp"])) &&
         (is.na(x[row, "n_cases_exp"]) || is.na(x[row, "n_cases_nexp"]) || is.na(x[row, "n_controls_exp"]) || is.na(x[row, "n_controls_nexp"])))) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated.")
      )
      column_errors[row] = paste(column_errors[row], " For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated. // ")
      column_type_errors[row] = status
    }

    if ((x[row, "measure"] %in% c("logRR", "RR")) &&
        ((is.na(x[row, "n_cases"]) || is.na(x[row, "n_controls"])) &&
         (is.na(x[row, "n_cases_exp"]) || is.na(x[row, "n_cases_nexp"]) || is.na(x[row, "n_exp"]) || is.na(x[row, "n_nexp"])) &&
         (is.na(x[row, "n_cases_exp"]) || is.na(x[row, "n_cases_nexp"]) || is.na(x[row, "n_controls_exp"]) || is.na(x[row, "n_controls_nexp"])))) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated.")
      )
      column_errors[row] = paste(column_errors[row], " For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated. // ")
      column_type_errors[row] = status
    }

    ######################################
    ## check inputs confidence interval ##
    ######################################
    if (!is.na(x[row, "value"]) & !is.na(x[row, "ci_lo"]) & !is.na(x[row, "ci_up"])) {
      if (x[row, "measure"] %in% c("SMD", "G", "MD", "logOR", "logRR", "logIRR", "logHR")) {
        if ( (x[row, "ci_up"] - x[row, "value"] == 0) |
             (x[row, "value"] - x[row, "ci_lo"] == 0) |
             (x[row, "ci_up"] - x[row, "ci_lo"] == 0)) {

          status <- "ERROR"
          error_msgs <- append(error_msgs, paste("Wrong input in the confidence interval"))
          column_errors[row] = paste(column_errors[row], " Wrong confidence interval. //")
          column_type_errors[row] =  "ERROR"
        }
      } else if (x[row, "measure"] %in% c("OR", "RR", "HR", "IRR")) {
        if ( (log(x[row, "ci_up"]) - log(x[row, "value"]) == 0) |
             (log(x[row, "value"]) - log(x[row, "ci_lo"]) == 0) |
             (log(x[row, "ci_up"]) - log(x[row, "ci_lo"]) == 0)) {

          status <- "ERROR"
          error_msgs <- append(error_msgs, paste("Wrong input in the confidence interval"))
          column_errors[row] = paste(column_errors[row], " Wrong confidence interval. //")
          column_type_errors[row] =  "ERROR"
        }
      }
    }
    ################################################
    ## check inputs symmetric confidence interval ##
    ################################################
    if (!is.na(x[row, "value"]) & !is.na(x[row, "ci_lo"]) & !is.na(x[row, "ci_up"])) {
      if (x[row, "measure"] %in% c("SMD", "G", "MD", "logOR", "logRR", "logIRR", "logHR")) {
        if ( ((x[row, "ci_up"] - x[row, "value"]) > ((x[row, "value"] - x[row, "ci_lo"]) + 0.05 * (x[row, "value"] - x[row, "ci_lo"]))) |
             ((x[row, "ci_up"] - x[row, "value"]) < ((x[row, "value"] - x[row, "ci_lo"]) - 0.05 * (x[row, "value"] - x[row, "ci_lo"]))) ) {

          status <- ifelse(status == "ERROR", "ERROR", "WARNING")
          error_msgs <- append(error_msgs,
                               paste("The confidence interval is not symmetric around the effect size (this is only a warning, not an error).")
          )
          column_errors[row] = paste(column_errors[row], " Non-symmetric confidence interval. //")
          column_type_errors[row] = ifelse(column_type_errors[row] == "ERROR", "ERROR", "WARNING")
        }
      } else if (x[row, "measure"] %in% c("OR", "RR", "HR", "IRR")) {
        if (x[row, "value"] < 0) { stop("unplanned issue") }
        if ( ((log(x[row, "ci_up"]) - log(x[row, "value"])) > ((log(x[row, "value"]) - log(x[row, "ci_lo"])) + 0.05 * (log(x[row, "value"]) - log(x[row, "ci_lo"])))) |
             ((log(x[row, "ci_up"]) - log(x[row, "value"])) < ((log(x[row, "value"]) - log(x[row, "ci_lo"])) - 0.05 * (log(x[row, "value"]) - log(x[row, "ci_lo"])))) ) {

          status <- ifelse(status == "ERROR", "ERROR", "WARNING")
          error_msgs <- append(error_msgs,
                               paste("The confidence interval is not symmetric around the effect size (this is only a warning, not an error).")
          )
          column_errors[row] = paste(column_errors[row], " Non-symmetric confidence interval. //")
          column_type_errors[row] = ifelse(column_type_errors[row]  == "ERROR", "ERROR", "WARNING")
        }
      }
    }

    ################################################
    ## check inputs shared_controls / shared_nexp ##
    ################################################

    # check that users have provided sufficient information when specifying 'shared_controls' or 'shared_nexp'
    if ( (!is.na(x[row, "shared_nexp"]) & (is.na(x[row, "n_nexp"]) & (is.na(x[row, "n_cases_nexp"]) & is.na(x[row, "n_controls_nexp"])))) | (!is.na(x[row, "shared_controls"]) & (is.na(x[row, "n_controls"])  & (is.na(x[row, "n_controls_exp"]) & is.na(x[row, "n_controls_nexp"]))))) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("The number of participants in the non-exposed group(s) (resp. in the control group(s)) is mandatory when 'shared_nexp' (resp. 'shared_control') is indicated.")
      )
      column_errors[row] = paste(column_errors[row], " Missing n_controls / n_nexp while shared_controls / shared_nexp is indicated. // ")
      column_type_errors[row] = status
    }
    # check that users have provided sufficient information when specifying 'shared_nexp' for IRR
    if ( (!is.na(x[row, "shared_nexp"]) & (is.na(x[row, "time_nexp"])  & x[row, "measure"] == "IRR"))) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("The person-time for the non-exposed group is mandatory when 'shared_nexp' is indicated.")
      )
      column_errors[row] = paste(column_errors[row], " Missing time_nexp while shared_nexp is indicated. // ")
      column_type_errors[row] = status
    }
    # prevent users to indicate both 'shared_exp' and 'shared_controls'
    if (!is.na(x[row, "shared_nexp"]) & !is.na(x[row, "shared_controls"])) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("The 'shared_nexp' and 'shared_controls' columns cannot be indicated for the same study.")
      )
      column_errors[row] = paste(column_errors[row], " The 'shared_nexp' and 'shared_controls' columns cannot be indicated for the same study. // ")
      column_type_errors[row] = status
    }

    # prevent users to use the 'shared_exp' / 'shared_controls' columns when HR/R/Z are the ES
    if ((!is.na(x[row, "shared_nexp"]) & x[row, "measure"] %in% c("logHR", "HR", "R", "Z")) | (!is.na(x[row, "shared_controls"]) & x[row, "measure"] %in% c("logHR", "HR", "R", "Z"))) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("The 'shared_nexp' and 'shared_controls' columns are not supported in combination with the HR, R and Z measures."))
      column_errors[row] = paste(column_errors[row], " The 'shared_nexp' / 'shared_controls' columns cannot be indicated with a 'HR', 'R' or 'Z' measures. // ")
      column_type_errors[row] = status
    }

    # prevent users to use the shared_controls column when IRR, RR are the effect size
    if (!is.na(x[row, "shared_controls"]) & x[row, "measure"] %in% c("logIRR", "IRR", "RR", "logRR")) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("\nThe 'shared_controls' column is only supported for the SMD, SMC, MD, G or OR measure.")
      )
      column_errors[row] = paste(column_errors[row], " The 'shared_controls' column cannot be indicated with a 'RR' or 'IRR' measure. // ")
      column_type_errors[row] = status
    }

    # prevent users to use the shared_nexp column when SMD, MD or G are the effect size
    if (!is.na(x[row, "shared_nexp"]) & x[row, "measure"] %in% c("SMD", "SMC", "MD", "G")) {
      status <- "ERROR"
      error_msgs <- append(error_msgs,
                           paste("The 'shared_nexp' column cannot be indicated with a 'SMD', 'SMC', 'MD', or 'G' measure.")
      )
      column_errors[row] = paste(column_errors[row], " The 'shared_nexp' column cannot be indicated with a 'SMD', 'SMC', 'MD', or 'G' measure. // ")
      column_type_errors[row] = status
    }
  }

  ###########################################################################################################
  ###################################### CHECK 4: factor ###################################################
  ###########################################################################################################

  for (factor in unique(x$factor)) {
    df_i <- x[x$factor == factor, ]
    df_agg_i <- aggregate.data.frame(df_i,
                                     by = list(df_i$factor,
                                               df_i$author,
                                               df_i$year),
                                     function(x) length(unique(x)))

    ########################################
    ## check inputs for multivariate data ##
    ########################################

    # we check that users have correctly indicated repeated entries
    if (length(unique(paste(df_i$author, df_i$year, df_i$factor))) < length(df_i$author)) {
      all_vals = paste0("In factor '", df_i$factor, "', ", df_i$author, " (", df_i$year, ")")
      all_vals_study = paste("Author:", df_i$author, "; Year:", df_i$year, "; Factor:", df_i$factor)
      rep_all_vals = duplicated(all_vals_study) | duplicated(all_vals_study, fromLast = TRUE) # duplicated gets the n-1 duplicates, so, I use this to get them all

      x[x$factor == factor, ][rep_all_vals, "duplicate"] <- TRUE
      i = which(x$factor == factor & x$duplicate == TRUE & is.na(x$multiple_es))

      if (any(is.na(df_i$multiple_es) & rep_all_vals)) {
        status <- "ERROR"
        error_msgs <- append(error_msgs, paste("Some repeated studies (author and year) in the same factor do not have any 'multiple_es' value."))
        column_errors[i] = paste(column_errors[i],
                                 " Study with same factor, author and year. If it is not an erroneous repeated entry, specify whether the multiple effect sizes come from multiple 'groups' or 'outcomes' in the 'multiple_es' column. //")
        column_type_errors[i] = status
      }
    }
    # if the dataset has multiple ES inputs per study, an error is returned
    if (any(df_agg_i$multiple_es > 1)) {

      i = which(paste(x$factor,x$author, x$year) %in%
                  paste(factor,
                        df_agg_i[df_agg_i$multiple_es > 1, "Group.2"],
                        df_agg_i[df_agg_i$multiple_es > 1, "Group.3"]))

      status <- "ERROR"
      error_msgs <- append(error_msgs, paste("Study with several multiple_es values. Please, specify only one of either 'groups' or 'outcomes'."))
      column_errors[i] = paste(column_errors[i],
                               " Study with several multiple_es values. Please, specify only one of either 'groups' or 'outcomes'. //")
      column_type_errors[i] = status
    }
    # if the dataset has multiple r inputs per study, an error is returned
    if (any(df_agg_i$r > 1)) {

      i = which(paste(x$factor,
                      x$author,
                      x$year) %in%
                  paste(factor,
                        df_agg_i[df_agg_i$r > 1, "Group.2"],
                        df_agg_i[df_agg_i$r > 1, "Group.3"]))

      status <- "ERROR"
      error_msgs <- append(error_msgs, paste("Study with several r values. Please, specify only one unique value per study."))
      column_errors[i] = paste(column_errors[i],
                               " Study with several r values. Please, specify only one unique value per study. // ")
      column_type_errors[i] = status
    }
    # if the dataset has multiple shared_nexp or shared_controls per study, an error is returned
    if (any(df_agg_i$shared_nexp > 1) | any(df_agg_i$shared_controls > 1)) {

      i = which(paste(x$factor,
                      x$author,
                      x$year) %in%
                  paste(factor,
                        df_agg_i[(df_agg_i$shared_nexp > 1) | (df_agg_i$shared_controls > 1), "Group.2"],
                        df_agg_i[(df_agg_i$shared_nexp > 1) | (df_agg_i$shared_controls > 1), "Group.3"]))

      status <- "ERROR"
      error_msgs <- append(error_msgs, paste("A study is associated with multiple 'shared_controls' or 'shared_nexp' values. There should be an unique value per study."))
      column_errors[i] = paste(column_errors[i],
                               " Study associated with multiple 'shared_controls' or 'shared_nexp' values. There should be an unique value. //")
      column_type_errors[i] = status
    }
    # if a study with multiple groups shares a control/non-exposed group with another study, an error is returned
    if (any(df_i$multiple_es == "groups", na.rm = TRUE)) {
      list_nshared_controls <- sapply(subset(df_i, multiple_es == "groups")$shared_controls, function(x) x)
      list_nshared_nexp <- sapply(subset(df_i, multiple_es == "groups")$shared_nexp, function(x) x)
      list_nshared <- sapply(c(list_nshared_controls, list_nshared_nexp), function(x) x)
      if (length(list_nshared) != 0) {
        for (group in unique(list_nshared)) {
            if (length(unique(paste0(subset(df_i, shared_controls == group)$author,
                                     subset(df_i, shared_controls == group)$year))) > 1) {
              i = which(x$factor == factor &
                          x$shared_controls == group)

              status <- "ERROR"
              error_msgs <- append(error_msgs, paste("It is not currently possible to have a study with multiple independent subgroups that shares a control group with another study."))
              column_errors[i] = paste(column_errors[i],
                                       " It is not currently possible to have a study with multiple independent subgroups that shares a control group with another study. //")
              column_type_errors[i] = status
          } else if (length(unique(paste0(subset(df_i, shared_nexp == group)$author,
                                          subset(df_i, shared_nexp == group)$year))) > 1) {
            i = which(x$factor == factor &
                        x$shared_nexp == group)

            status <- "ERROR"
            error_msgs <- append(error_msgs, paste("It is not currently possible to have a study with multiple independent subgroups that shares a non-exposed group with another study."))
            column_errors[i] = paste(column_errors[i],
                                     " It is not currently possible to have a study with multiple independent subgroups that shares a non-exposed group with another study. //")
            column_type_errors[i] = status
          }
        }
      }
    }

    ##############################################
    ## consistency AMSTAR / Reverse in a factor ##
    ##############################################

    # we check that amstar values are unique within each factor
    if (length(unique(df_i$amstar)) > 1) {
      status <- "ERROR"

      i <- which(x$factor == factor)

      error_msgs <- append(error_msgs, paste("AMSTAR values should be constant within a factor."))
      column_errors[i] = paste0(column_errors[i],
                               " Values of amstar should not differ within a factor. Please insert a unique value for each factor. // ")
      column_type_errors[i] = status
    }
    # we warn users if only some effect sizes for a given factor are reversed
    if (length(unique(df_i$reverse_es)) > 1) {
      status <- ifelse(status == "ERROR", "ERROR", "WARNING")
      i <- which(x$factor == factor)

      error_msgs <- append(error_msgs, paste("Some but not all effect sizes are reversed for a factor. Check this is what you want (this is only a warning, not an error)."))
      column_errors[i] = paste(column_errors[i],
                               " Some but not all effect sizes are reversed for this factor. Check this is what you want. // ")
      column_type_errors[i] = ifelse(column_type_errors[i] == "ERROR", "ERROR", "WARNING")
    }

    if (any(duplicated(na.omit(df_i$shared_nexp)))) {
      i <- which(x$factor == factor)
      situation[i] <- "shared_nexp_"
    } else if (any(duplicated(na.omit(df_i$shared_controls)))) {
      i <- which(x$factor == factor)
      situation[i] <- "shared_controls_"
    }

    ######################
    #### multiple_es #####
    ######################
    x$multiple_es = tolower(x$multiple_es)
    x$multiple_es[x$multiple_es=="outcome"] <- "outcomes"
    x$multiple_es[x$multiple_es=="group"] <- "groups"

    if (length(unique(df_i$measure)) > 1 & any(df_i$measure == "IRR")) {
      status <- "ERROR"
      i <- which(x$factor == factor)

      error_msgs <- append(error_msgs, paste("'IRR' measure cannot be converted into another effect size measure. Within a single factor, it is thus not possible to combine 'IRR' with another additional measure."))
      column_errors[i] = paste(column_errors[i],
                               " 'IRR' is used in combination with another effect size measure. Please, specify 'IRR' for all or none of the studies of this factor. //")
      column_type_errors[i] = "ERROR"
    }
  }

  ###########################################################################################################
  ###################################### CHECK 5: Sample size conversions ###################################################
  ###########################################################################################################

  measure            = x$measure
  missing_measure    = is.na(measure)

  # Effect size and CI
  indexs = which(is.na(x$ci_lo))
  x$ci_lo[indexs] = suppressWarnings(ifelse(measure[indexs] %in% c("SMD", "MD", "G", "R", "Z", "SMC", "logIRR", "logOR", "logRR", "logHR"),
                           x$value[indexs] - (x$ci_up[indexs] - x$value[indexs]),
                           exp(log(x$value[indexs]) - (log(x$ci_up[indexs]) - log(x$value[indexs])))))

  indexs = which(is.na(x$ci_up))
  x$ci_up[indexs] = suppressWarnings(ifelse(measure[indexs] %in% c("SMD", "MD", "G", "R", "Z", "SMC", "logIRR", "logOR", "logRR", "logHR"),
                           x$value[indexs] + (x$value[indexs] - x$ci_lo[indexs]),
                           exp(log(x$value[indexs]) + (log(x$value[indexs]) - log(x$ci_lo[indexs])))))


  indexs = which(is.na(x$value))
  x$value[indexs] = suppressWarnings(ifelse(measure[indexs] %in% c("SMD", "MD", "G", "R", "Z", "SMC", "logIRR", "logOR", "logRR", "logHR"),
                           (x$ci_lo[indexs] + x$ci_up[indexs]) / 2,
                           exp((log(x$ci_lo[indexs]) + log(x$ci_up[indexs])) / 2)))

  # IRR marginals as sums:
  indexs = which(is.na(x$time) & measure %in% c("IRR", "logIRR"))
  x$time[indexs] = x$time_exp[indexs] + x$time_nexp[indexs]

  # OR / RR marginals as sums:
  indexs = which(is.na(x$n_controls) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_controls[indexs] = x$n_controls_exp[indexs] + x$n_controls_nexp[indexs]

  indexs = which(is.na(x$n_exp) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_exp[indexs] = x$n_cases_exp[indexs] + x$n_controls_exp[indexs]

  indexs = which(is.na(x$n_nexp) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_nexp[indexs] = x$n_cases_nexp[indexs] + x$n_controls_nexp[indexs]

  n = x$n_exp + x$n_nexp
  indexs = which(is.na(n)  & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  n[indexs] = x$n_cases[indexs] + x$n_controls[indexs]

  # IRR marginals as complements:
  indexs = which(is.na(x$time_exp) & measure %in% c("IRR", "logIRR"))
  x$time_exp[indexs] = x$time[indexs] - x$time_nexp[indexs]

  indexs = which(is.na(x$time_nexp) & measure %in% c("IRR", "logIRR"))
  x$time_nexp[indexs] = x$time[indexs] - x$time_exp[indexs]

  # OR / RR marginals as complements:
  indexs = which(is.na(x$n_exp) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_exp[indexs] = n[indexs] - x$n_nexp[indexs]

  indexs = which(is.na(x$n_nexp) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_nexp[indexs] = n[indexs] - x$n_exp[indexs]

  indexs = which(is.na(x$n_cases) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_cases[indexs] = n[indexs] - x$n_controls[indexs]

  indexs = which(is.na(x$n_cases) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_cases[indexs] = x$n_cases_exp[indexs] + x$n_cases_nexp[indexs]

  indexs = which(is.na(x$n_controls) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_controls[indexs] = n[indexs] - x$n_cases[indexs]

  # IRR / OR / RR n_cases_exp and n_cases_nexp as complements:
  indexs = which(is.na(x$n_cases_exp) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_cases_exp[indexs] = x$n_cases[indexs] - x$n_cases_nexp[indexs]

  indexs = which(is.na(x$n_cases_nexp) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_cases_nexp[indexs] = x$n_cases[indexs] - x$n_cases_exp[indexs]

  # OR / RR n_controls_exp and n_controls_nexp as complements:
  indexs = which(is.na(x$n_controls_exp) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_controls_exp[indexs] = x$n_exp[indexs] - x$n_cases_exp[indexs]

  indexs = which(is.na(x$n_controls_nexp) & measure %in% c("OR", "RR", "HR", "IRR", "logIRR", "logOR", "logRR", "logHR"))
  x$n_controls_nexp[indexs] = x$n_nexp[indexs] - x$n_cases_nexp[indexs]

  # variance
  indexs = which(!is.na(x$var) & is.na(x$se))
  x$se[indexs] = sqrt(x$var[indexs])

  # n_sample
  indexs = which(is.na(x$n_sample))
  x$n_sample[indexs] = x$n_cases[indexs] + x$n_controls[indexs]

  # ERRORS: incongruencies
  wrong_n_cases = x$n_cases != x$n_cases_exp + x$n_cases_nexp
  if (any(wrong_n_cases, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("In some instance, the number of cases (n_cases) is different from the sum of the exposed- and non-exposed cases."))
    column_errors[which(wrong_n_cases)] <- paste(column_errors[which(wrong_n_cases)], " n_cases != n_cases_exp + n_cases_nexp. // ")
    column_type_errors[which(wrong_n_cases)] <- "ERROR"
  }
  wrong_time = (x$time < (x$time_exp + x$time_nexp) - 0.05 * (x$time_exp + x$time_nexp)) | (x$time > (x$time_exp + x$time_nexp) + 0.05 * (x$time_exp + x$time_nexp))

  if (any(wrong_time, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("In some instance, the overall person-time (time) is different from the sum of the person-time for exposed- and non-exposed cases."))
    column_errors[which(wrong_time)] <- paste(column_errors[which(wrong_time)], " time != time_exp + time_nexp. // ")
    column_type_errors[which(wrong_time)] <- "ERROR"
  }
  wrong_n_controls = x$n_controls != x$n_controls_exp + x$n_controls_nexp
  if (any(wrong_n_controls, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("In some instance, the number of controls (n_controls) is different from the sum of the exposed- and non-exposed number of controls."))
    column_errors[which(wrong_n_controls)] <- paste(column_errors[which(wrong_n_controls)], " n_controls != n_controls_exp + n_controls_nexp. // ")
    column_type_errors[which(wrong_n_controls)] <- "ERROR"
  }
  wrong_n_exp = x$n_exp != x$n_cases_exp + x$n_controls_exp
  if (any(wrong_n_exp, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("In some instance, the number of exposed participants (n_exp) is different from the sum of the cases and controls exposed (n_cases_exp + n_controls_exp)."))
    column_errors[which(wrong_n_exp)] <- paste(column_errors[which(wrong_n_exp)], " n_exp != n_cases_exp + n_controls_exp. // ")
    column_type_errors[which(wrong_n_exp)] <- "ERROR"
  }
  wrong_n_nexp = x$n_nexp != x$n_cases_nexp + x$n_controls_nexp
  if (any(wrong_n_nexp, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("In some instance, the number of non-exposed participants (n_nexp) is different from the sum of the cases and controls non-exposed (n_cases_nexp + n_controls_nexp)."))
    column_errors[which(wrong_n_nexp)] <- paste(column_errors[which(wrong_n_nexp)], " n_nexp != n_cases_nexp + n_controls_nexp. // ")
    column_type_errors[which(wrong_n_nexp)] <- "ERROR"
  }

  irr = which(x$measure == "IRR")
  x$n_controls[irr] = x$n_controls_exp[irr] = x$n_controls_nexp[irr] = NA

  ###########################################################################################################
  #################### CHECK 6: check information for effect size conversions ###############################
  ###########################################################################################################
  missing_value = is.na(x$value)
  missing_ci = is.na(x$ci_lo) | is.na(x$ci_up)
  missing_se = is.na(x$se)
  missing_value_ci = missing_value | missing_ci
  missing_value_se = missing_value | missing_se
  missing_means_post = is.na(x$mean_cases) | is.na(x$sd_cases) | is.na(x$mean_controls) | is.na(x$sd_controls)
  missing_means_pre_post = is.na(x$mean_pre_cases) | is.na(x$sd_pre_cases) | is.na(x$mean_pre_controls) | is.na(x$sd_pre_controls) |
                           is.na(x$mean_cases) | is.na(x$sd_cases) | is.na(x$mean_controls) | is.na(x$sd_controls)
  missing_means_change = is.na(x$mean_change_cases) | is.na(x$sd_change_cases) | is.na(x$mean_change_controls) | is.na(x$sd_change_controls)
  missing_n_sample = is.na(x$n_sample)
  missing_cases_controls = is.na(x$n_cases) | is.na(x$n_controls)
  missing_exp_nexp = is.na(x$n_exp) | is.na(x$n_nexp)
  missing_2x2 = is.na(x$n_cases_exp) | is.na(x$n_cases_nexp) | is.na(x$n_controls_exp) | is.na(x$n_controls_nexp)
  missing_time_cases_exp_nexp = is.na(x$n_cases_exp) | is.na(x$n_cases_nexp) | is.na(x$time_exp) | is.na(x$time_nexp)
  missing_time = is.na(x$time)

  # SMD/G/MD
  ## users indicate MD/G and means => convert measure to SMD
  md_g_means = measure %in% c("MD", "G") & missing_value & missing_ci & missing_se & !missing_means_post
  if (any(md_g_means, na.rm = TRUE)) {
    x[which(md_g_means), ]$measure <- "SMD"
    measure[which(md_g_means)] <- "SMD"
  }

  ## SMD : at least 'value' OR 'means' should be indicated
  d_missing = missing_value & missing_means_post & measure == "SMD"
  if (any(d_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("SMD measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(d_missing)] <- paste(column_errors[which(d_missing)], "Unknown value of the SMD / means and SD. // ")
    column_type_errors[which(d_missing)] <- ifelse(column_type_errors[which(d_missing)] == "ERROR", "ERROR", "ERROR")
  }

  ## G : at least 'value' should be indicated
  g_missing = missing_value & measure == "G"
  if (any(g_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("G measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(g_missing)] <- paste(column_errors[which(g_missing)], "Unknown value of G. // ")
    column_type_errors[which(g_missing)] <- ifelse(column_type_errors[which(g_missing)] == "ERROR", "ERROR", "ERROR")
  }


  ## MD: at least 'value' + ('ci' | 'se') should be indicated
  md_missing = missing_value_ci & missing_value_se & measure == "MD"
  if (any(md_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("MD measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(md_missing)] <- paste(column_errors[which(md_missing)], " Unknown value + (confidence interval / se / var) of the MD. // ")
    column_type_errors[which(md_missing)] <- "ERROR"
  }

  # SMC : at least 'value' + ('ci'|'se') OR 'mean_pre_post' OR 'mean_change' should be indicated
  smc_missing_ci = missing_value_ci & measure == "SMC"
  smc_missing_se = missing_value_se & measure == "SMC"
  smc_missing_pre_post = missing_means_pre_post & measure == "SMC"
  smc_missing_change = missing_means_change & measure == "SMC"

  smc_missing = smc_missing_ci & smc_missing_se & smc_missing_pre_post & smc_missing_change
  if (any(smc_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("SMC measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(smc_missing)] <- paste(column_errors[which(smc_missing)], " Unknown value + (confidence interval / se / var) OR pre+post means/SD OR means/SD change. // ")
    column_type_errors[which(smc_missing)] <- "ERROR"
  }

  # Z/R : value
  r_missing = (missing_value | missing_n_sample) & measure %in% c("R", "Z")

  if (any(r_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("R or Z measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(md_missing)] <- paste(column_errors[which(md_missing)], " Unknown R/Z values + 'n_sample'. // ")
    column_type_errors[which(md_missing)] <- "ERROR"
  }


  # HR: at least 'value' + ('ci' | 'se') should be indicated
  hr_missing = missing_value_ci & missing_value_se & measure %in% c("HR", "logHR")

  if (any(hr_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("HR measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(hr_missing)] <- paste(column_errors[which(hr_missing)], " Unknown value + (confidence interval / se / var) of the HR. // ")
    column_type_errors[which(hr_missing)] <- "ERROR"
  }

  # OR
  or_missing_2x2 = (missing_value | missing_cases_controls) & missing_2x2 & measure %in% c("OR", "logOR")
  or_missing_ci_se = missing_value_ci & missing_value_se & measure %in% c("OR", "logOR")
  or_missing = or_missing_2x2 & or_missing_ci_se
  if (any(or_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("OR measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(or_missing)] <- paste(column_errors[which(or_missing)], " Unknown value of the OR and n_cases/n_controls or unknown 2x2 table. // ")
    column_type_errors[which(or_missing)] <- "ERROR"
  }

  # RR
  rr_missing = missing_2x2 & missing_value_ci & missing_value_se & measure %in% c("RR", "logRR")
  if (any(rr_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("RR measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(rr_missing)] <- paste(column_errors[which(rr_missing)], " Unknown value + (confidence interval / se / var) of the RR or unknown 2x2 table. // ")
    column_type_errors[which(rr_missing)] <- "ERROR"
  }

  # Compulsory arguments for IRR
  irr_missing_ci_time = (missing_value_ci | missing_time) & measure %in% c("logIRR", "IRR")
  irr_missing_se_time = (missing_value_se | missing_time) & measure %in% c("logIRR", "IRR")
  missing_irr = missing_time_cases_exp_nexp & irr_missing_ci_time & irr_missing_se_time & measure %in% c("logIRR", "IRR")
  if (any(missing_irr, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("IRR measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(missing_irr)] <- paste(column_errors[which(missing_irr)], " Unknown (value + (ci/se/var) + n_cases + time) OR number of cases and time for both exp and non-exp groups. // ")
    column_type_errors[which(missing_irr)] <- "ERROR"
  }

  ##### determine the input to guide the umbrella function ----------
  ##### will be used in future updates----------
  for (i in 1:nrow(x)) {

    # measure
    situation[i] <- paste0(situation[i], x$measure[i])

    # effect size
    if (!is.na(x$value[i])) { situation[i] <- paste0(situation[i], "_", "ES") }

    # se/variance
    if (!is.na(x$se[i])) { situation[i] <- paste0(situation[i], "_", "SE") }

    # confidence interval
    if (!is.na(x$ci_lo[i]) & !is.na(x$ci_up[i])) { situation[i] <- paste0(situation[i], "_", "CI") }


    # specific information for each measure
    if (x$measure[i] %in% c("SMD", "G", "MD")) {
      if (!is.na(x$mean_cases[i]) & !is.na(x$mean_controls[i]) & !is.na(x$sd_cases[i]) & !is.na(x$sd_controls[i])) {
        situation[i] <- paste0(situation[i], "_", "mean/SD")
      }
    } else if (x$measure[i] %in% c("SMC")) {
      if (!is.na(x$mean_cases[i]) & !is.na(x$mean_controls[i]) &
          !is.na(x$sd_cases[i]) & !is.na(x$sd_controls[i]) &
          !is.na(x$mean_pre_cases[i]) & !is.na(x$mean_pre_controls[i]) &
          !is.na(x$sd_pre_cases[i]) & !is.na(x$sd_pre_controls[i])) {
        situation[i] <- paste0(situation[i], "_", "mean/SD_pre/post")
      } else if (!is.na(x$mean_change_cases[i]) & !is.na(x$mean_change_controls[i]) &
                 !is.na(x$sd_change_cases[i]) & !is.na(x$sd_change_controls[i])) {
        situation[i] <- paste0(situation[i], "_", "mean/SD_change")
      }
    } else if (x$measure[i] %in% c("OR", "RR", "HR", "logOR", "logRR", "logHR")) {
      if (!is.na(x$n_cases_exp[i]) & !is.na(x$n_cases_nexp[i]) &
          !is.na(x$n_controls_exp[i]) & !is.na(x$n_controls_nexp[i])) {
        situation[i] <- paste0(situation[i], "_", "2x2")
      } else if (!is.na(x$n_cases[i]) & !is.na(x$n_controls[i])) {
        situation[i] <- paste0(situation[i], "_", "cases_controls")
      } else if (!is.na(x$n_exp[i]) & !is.na(x$n_nexp[i])) {
        situation[i] <- paste0(situation[i], "_", "exp_nexp")
      }
    } else if (x$measure[i] %in% c("IRR", "logIRR")) {
      if (!is.na(x$n_cases_exp[i]) & !is.na(x$n_cases_nexp[i])) {
        situation[i] <- paste0(situation[i], "_", "cases_exp_nexp")
      } else if (!is.na(x$n_cases[i])) {
        situation[i] <- paste0(situation[i], "_", "cases_total")
      }
      if (!is.na(x$time_exp[i]) & !is.na(x$time_nexp[i])) {
        situation[i] <- paste0(situation[i], "_", "time_exp_nexp")
      } else if (!is.na(x$n_cases[i])) {
        situation[i] <- paste0(situation[i], "_", "time_total")
      }
    }
  }

  x$situation <- situation

  # provides the coefficient for the adjustment of the sample sizes when needed
  x$adj_controls = x$adj_nexp = NA

  for (factor in unique(x$factor)) {

    df_i = x[x$factor == factor, ]

    i = which(x$factor == factor)

    if(grepl("shared_controls", df_i$situation[1])) {
      adj_controls = .shared_adjustment_mod(df_i$shared_controls, df_i$author, df_i$year)
      x[i, ]$adj_controls = adj_controls
    } else if(grepl("shared_nexp", df_i$situation[1])) {
      adj_nexp = .shared_adjustment_mod(df_i$shared_nexp, df_i$author, df_i$year)
      x[i, ]$adj_nexp = adj_nexp
    }
  }

  column_type_errors[column_type_errors == ""] <- NA
  column_errors[column_errors == ""] <- NA

  df <- cbind(column_type_errors, column_errors, row_index = x$row_index, subset(x, select =  -c(row_index)))

  if (status == "ERROR") {
    attr(returned, "status") <- "ERRORS"
    errors_num = grepl("warning", error_msgs, fixed = TRUE)
    attr(returned, "message") <- paste("ERROR:\n-",
                                       paste(unique(error_msgs[!errors_num]), collapse = "\n- "),
                                       "\nWARNING:\n-",
                                       paste(ifelse(length(unique(error_msgs[errors_num])) > 0, unique(error_msgs[errors_num]), "No warning"), collapse = "\n- "))
  } else if (status == "WARNING") {
    attr(returned, "status") <- "WARNINGS"
    attr(returned, "message") <- paste("WARNING:\n-", paste(unique(error_msgs), collapse = "\n- "))
  } else {
    attr(returned, "status") <- "NO ERRORS OR WARNINGS"
    attr(returned, "message") <- "Your dataset is well formatted."
  }

  attr(returned, "data") <- df
  return(returned)
}

#' Fix measure name
#'
#' @param x
#'
#' @noRd
.fix_measure_name = function (x) {
  fixed_measure = switch(toupper(x),
         "SMD" =, "D"=, "d"=, "cohen"=, "COHEN D" =, "Cohen d" = "SMD",
         "HR" = "HR",
         "IRR" = "IRR",
         "OR"=, "ODDS RATIO" = "OR",
         "RR" = "RR",
         "G" =, "g"=, "HEDGES' G"=, "HEDGES G"=, "Hedges g" =, "Hedges' g" = "G",
         "MD" =, "MEAN DIFFERENCE" = "MD",
         x
  )
  return(fixed_measure)
}

#' Function that checks whether the analysis needs a special treatment
#'
#' @param x
#'
#' @noRd
.fix_n_in_allelic_analyses <- function(x){
  #
  # row-wise
  for (i in 1:dim(x)[1]) {
    if (!is.na(x[i, "analysis"]) && ("allelic" == tolower(x[i, "analysis"]))){
      x[i, "n_cases"] = x[i,"n_cases"] * 2
      x[i, "n_controls"] = x[i,"n_controls"] * 2
    }
  }
  return(x)
}


#' Split an unique study column into two author / year columns
#'
#' @param x
#'
#' @noRd
.split_study_into_author_year <- function(x){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  for (i in 1:nrow(x)) {
    x[i, "author"] <- trim(gsub("[0-9]*", "", x$study[i])) # keep characters
    x[i, "year"] <- gsub("[^0-9]*", "", x$study[i]) # remove non-numeric
    if (!is.na(x[i, "year"]) && x[i, "year"] == "") {
      x[i, "year"] <- NA
    }
  }
  x = subset(x, select = -c(study))
  return(x)
}

