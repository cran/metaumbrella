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
  x_saved = x

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
  # if (any(c("OR", "RR", "IRR", "logOR", "logRR", "logIRR", "R", "Z") %in% x$measure |
  #         all(is.na(x$measure)))) {
    # column names
    mandatory_cols <- c("meta_review", "factor", "author", "year", "measure")
    mandatory_cols_types <- rep("factor", length(mandatory_cols))
    other_expected_cols <- c("info_used", "value", "var", "se", "ci_lo", "ci_up",
                             "n_sample", "n_exp", "n_nexp",
                             "n_cases", "n_controls",
                             "n_cases_exp", "n_cases_nexp", "n_controls_exp", "n_controls_nexp",
                             "prop_cases_exp", "prop_cases_nexp",
                             "d", "d_se", "g", "g_se", "md", "md_se", "md_ci_lo", "md_ci_up",
                             "or", "logor", "or_se", "or_ci_lo", "or_ci_up", "logor_ci_lo", "logor_ci_up",
                             "rr", "logrr", "rr_se", "rr_ci_lo", "rr_ci_up", "logrr_ci_lo", "logrr_ci_up",
                             "hr", "loghr", "hr_se", "hr_ci_lo", "hr_ci_up", "loghr_ci_lo", "loghr_ci_up",
                             "irr", "logirr", "irr_se", "irr_ci_lo", "irr_ci_up", "logirr_ci_lo", "logirr_ci_up",
                             "pearson_r", "fisher_z",
                             "user_es_crude", "user_se_crude", "user_ci_lo_crude", "user_ci_up_crude",

                             "mean_cases", "sd_cases", "mean_controls", "sd_controls",
                             "mean_pre_cases", "sd_pre_cases", "mean_pre_controls", "sd_pre_controls", "pre_post_cor",
                             "mean_change_cases", "sd_change_cases", "mean_change_controls", "sd_change_controls",
                             "time", "time_exp", "time_nexp",
                             # "shared_controls", "shared_nexp",
                             # "thr",
                             "analysis", "discard",
                             "indirectness", "rob",
                             "rob1_rand", "rob1_allocation", "rob1_blind_pers",
                             "rob1_blind_outcome", "rob1_attrition","rob1_report",
                             "rob2_rand", "rob2_deviation", "rob2_missing",
                             "rob2_outcome", "rob2_report",
                             "amstar", "multiple_es", "r", "reverse_es")
    # column types

    other_expected_cols_types = rep("numeric", length(other_expected_cols))
    other_expected_cols_types[other_expected_cols %in%
                                c("discard", "indirectness", "rob",
                                  "rob1_rand", "rob1_allocation", "rob1_blind_pers",
                                  "rob1_blind_outcome", "rob1_attrition","rob1_report",
                                  "rob2_rand", "rob2_deviation", "rob2_missing",
                                  "rob2_outcome", "rob2_report",#"shared_controls", "shared_nexp",
                                  "multiple_es",  "reverse_es")] <- "factor"

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
  measure_vals = c("SMD", "SMC", "G",
                   "MD", "MC",
                   "R", "Z",
                   "OR", "HR", "IRR", "RR",
                   "logOR", "logRR", "logHR", "logIRR")
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
                     error_msgs <- append(error_msgs, paste("Measure '", cell_value, "' not supported. Should be either 'SMD', 'SMC', 'G', 'MD', 'MC' 'R', 'Z', 'OR', 'HR', 'IRR', 'RR'"))
                     column_errors[i] = paste(column_errors[i], "Measure '", cell_value, "' not supported. Should be either 'SMD', 'SMC', 'G', 'MD', 'MC' 'R', 'Z', 'OR', 'HR', 'IRR', 'RR'. //")
                     column_type_errors[i] = "ERROR"
                     i = i + 1
                   } else {
                     status <- "ERROR"
                     error_msgs <- append(error_msgs, paste("Measure '", cell_value, "' not supported. Should be either 'SMD', 'SMC', 'G', 'MD', 'MC' 'R', 'Z', 'OR', 'HR', 'IRR', 'RR'."))
                     column_errors[i] = paste(column_errors[i], " Measure '", cell_value, "' not supported. Should be either 'SMD', 'SMC', 'G', 'MD', 'MC' 'R', 'Z', 'OR', 'HR', 'IRR', 'RR'. //")
                     column_type_errors[i] = "ERROR"
                     i = i + 1
                   }
                 }
               },

               'value' = {
                 # check whether the value for ratios is not < 0
                if (!(x[i, "measure"] %in% c("SMD", "SMC", "G", "MD", "MC", "R", "Z")) && cell_value <= 0 && !x[i, "measure"] %in% measure_warning_vals) {
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
                 if ( ((cell_value <= 0) & !(x[i,"measure"] %in% c("SMD", "SMC", "G", "MD", "MC", "R", "Z"))) | (sum(- x[i, "value"], cell_value, na.rm = TRUE) > 0 & !is.na(x[i, "value"])) ) {
                     status <- "ERROR"
                     error_msgs <- append(error_msgs, paste("Lower bound of the 95% CI should be lower than 'value' and should not be negative or null with 'OR', 'RR', 'IRR' and 'HR' measures."))
                     column_errors[i] = paste(column_errors[i], "Wrong 'ci_lo', should be lower than 'value' and should be > 0 with OR RR HR IRR. //")
                     column_type_errors[i] = status
                   }
                 },

               'ci_up' = {
                   if ( ((cell_value <= 0) & !(x[i,"measure"] %in% c("SMD", "SMC", "G", "MD", "MC", "R", "Z"))) | (sum(- cell_value, x[i, "value"], na.rm = TRUE) > 0 & !is.na(x[i, "value"])) ) {
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

               # 'thr' = {
               #   if (cell_value < 0) {
               #     status <- "ERROR"
               #     error_msgs <- append(error_msgs, paste(" Wrong thr value, it should be a positive number."))
               #     column_errors[i] = paste(column_errors[i], " Wrong thr, it should be a positive number. // ")
               #     column_type_errors[i] = status
               #   }
               # },

               'rob' =, "rob1_rand"=, "rob1_allocation"=, "rob1_blind_pers"=,
               "rob1_blind_outcome"=, "rob1_attrition" =,"rob1_report" =,
               "rob2_rand"=, "rob2_deviation"=, "rob2_missing"=,
               "rob2_outcome"=, "rob2_report" = {
                 if (!(cell_value %in% c("high", "High", "some", "some", "unclear", "Unclear", "low", "Low", NA, "NA"))) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong risk of bias: should be either high/unclear/some/low."))
                   column_errors[i] = paste(column_errors[i],
                                            " Wrong risk of bias (rob) value. Should be 'high', 'unclear', 'some' or 'low'. //")
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
               'indirectness' = {
                 if (!(cell_value %in% c("serious", "very serious", "no indirectness", "no"))) {
                   status <- "ERROR"
                   error_msgs <- append(error_msgs, paste("Wrong identifier for indirectness: should be 'serious', 'very serious', or 'no indirectness'."))
                   column_errors[i] = paste(column_errors[i],
                                            " Wrong identifier for indirectness: should be 'serious', 'very serious', or 'no indirectness'. //")
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

  ###########################################################################################################
  ###################################### CHECK 3: study ###################################################
  ###########################################################################################################

  for (row in 1:nrow(x)) {

    ######################################
    ## check inputs confidence interval ##
    ######################################
    if (!is.na(x[row, "value"]) & !is.na(x[row, "ci_lo"]) & !is.na(x[row, "ci_up"])) {
      if (x[row, "measure"] %in% c("SMD", "SMC", "G", "MD", "MC", "R", "Z",
                                   "logOR", "logRR", "logIRR", "logHR")) {
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
      if (x[row, "measure"] %in% c("SMD", "SMC", "G", "MD", "MC", "R", "Z",
                                   "logOR", "logRR", "logIRR", "logHR")) {
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
    x$multiple_es = tolower(x$multiple_es)
    x$multiple_es[x$multiple_es=="outcome"] <- "outcomes"
    x$multiple_es[x$multiple_es=="group"] <- "groups"
    x$multiple_es[x$multiple_es==""] <- NA

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
    if (length(unique(df_i$indirectness)) > 1) {
      status <- "ERROR"
      i <- which(x$factor == factor)

      error_msgs <- append(error_msgs, paste("The indirectness values should be constant within a factor."))
      column_errors[i] = paste(column_errors[i],
                               " The indirectness values should be constant within a factor. // ")
      column_type_errors[i] = status
    }

  }


  null_rob = NA
  x$rob.recoded = ifelse(is.na(x$rob), null_rob,
                         ifelse(x$rob %in% c("low", "Low"), 1, 0))

  x$rob2_rand.recoded = ifelse(is.na(x$rob2_rand), null_rob,
                         ifelse(x$rob2_rand %in% c("low", "Low"), 1, 0))

  x$rob2_deviation.recoded = ifelse(is.na(x$rob2_deviation), null_rob,
                         ifelse(x$rob2_deviation %in% c("low", "Low"), 1, 0))

  x$rob2_missing.recoded = ifelse(is.na(x$rob2_missing), null_rob,
                         ifelse(x$rob2_missing %in% c("low", "Low"), 1, 0))

  x$rob2_outcome.recoded = ifelse(is.na(x$rob2_outcome), null_rob,
                         ifelse(x$rob2_outcome %in% c("low", "Low"), 1, 0))

  x$rob2_report.recoded = ifelse(is.na(x$rob2_report), null_rob,
                         ifelse(x$rob2_report %in% c("low", "Low"), 1, 0))

  x$rob1_rand.recoded = ifelse(is.na(x$rob1_rand), null_rob,
                               ifelse(x$rob1_rand %in% c("low", "Low"), 1, 0))

  x$rob1_allocation.recoded = ifelse(is.na(x$rob1_allocation), null_rob,
                                    ifelse(x$rob1_allocation %in% c("low", "Low"), 1, 0))

  x$rob1_blind_pers.recoded = ifelse(is.na(x$rob1_blind_pers), null_rob,
                                  ifelse(x$rob1_blind_pers %in% c("low", "Low"), 1, 0))

  x$rob1_blind_outcome.recoded = ifelse(is.na(x$rob1_blind_outcome), null_rob,
                                  ifelse(x$rob1_blind_outcome %in% c("low", "Low"), 1, 0))

  x$rob1_attrition.recoded = ifelse(is.na(x$rob1_attrition), null_rob,
                                 ifelse(x$rob1_attrition %in% c("low", "Low"), 1, 0))

  x$rob1_report.recoded = ifelse(is.na(x$rob1_report), null_rob,
                                 ifelse(x$rob1_report %in% c("low", "Low"), 1, 0))

  x$rob_report.recoded = ifelse(is.na(x$rob2_report.recoded) &
                                  is.na(x$rob1_report.recoded), null_rob,
                                ifelse(!is.na(x$rob2_report.recoded),
                                       x$rob2_report.recoded,
                                       x$rob1_report.recoded))

  # print(x$reverse_es[1:3])
  x$reverse_es = ifelse(x$reverse_es %in% c("reverse", "Reverse", "reversed", "Reversed"),
                          TRUE, FALSE)
  # print(x$reverse_es[1:3])

  x$indirectness = ifelse(is.na(x$indirectness), NA,
                               ifelse(x$indirectness %in% c("no indirectness"),
                                      "no", x$indirectness))

  ###########################################################################################################
  ###################################### CHECK 5: Sample size conversions ###################################################
  ###########################################################################################################

  measure            = x$measure
  missing_measure    = is.na(measure)

  # Effect size and CI
  indexs = which(is.na(x$ci_lo))
  x$ci_lo[indexs] = suppressWarnings(ifelse(measure[indexs] %in% c("SMD", "SMC", "G", "MD", "MC", "R", "Z", "logIRR", "logOR", "logRR", "logHR"),
                           x$value[indexs] - (x$ci_up[indexs] - x$value[indexs]),
                           exp(log(x$value[indexs]) - (log(x$ci_up[indexs]) - log(x$value[indexs])))))

  indexs = which(is.na(x$ci_up))
  x$ci_up[indexs] = suppressWarnings(ifelse(measure[indexs] %in% c("SMD", "SMC", "G", "MD", "MC", "R", "Z", "logIRR", "logOR", "logRR", "logHR"),
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
  x$n_sample =
    ifelse(is.na(x$n_sample),
           ifelse(x$measure == "IRR", x$n_cases, x$n_cases + x$n_controls),
           x$n_sample
  )

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
  missing_means_pre_post = is.na(x$mean_pre_cases) | is.na(x$sd_pre_cases) |
                           is.na(x$mean_pre_controls) | is.na(x$sd_pre_controls) |
                           is.na(x$mean_cases) | is.na(x$sd_cases) |
                           is.na(x$mean_controls) | is.na(x$sd_controls)
  missing_means_change = is.na(x$mean_change_cases) | is.na(x$sd_change_cases) |
                         is.na(x$mean_change_controls) | is.na(x$sd_change_controls)
  missing_n_sample = is.na(x$n_sample)
  missing_cases_controls = is.na(x$n_cases) | is.na(x$n_controls)
  missing_exp_nexp = is.na(x$n_exp) | is.na(x$n_nexp)
  missing_2x2 = is.na(x$n_cases_exp) | is.na(x$n_cases_nexp) | is.na(x$n_controls_exp) | is.na(x$n_controls_nexp)
  missing_time_cases_exp_nexp = is.na(x$n_cases_exp) | is.na(x$n_cases_nexp) | is.na(x$time_exp) | is.na(x$time_nexp)
  missing_time = is.na(x$time)

  # SMD/G/MD
  ## users indicate MD/G and means => convert measure to SMD
  # md_g_means = measure %in% c("MD", "G") & missing_value & missing_ci & missing_se & !missing_means_post
  # if (any(md_g_means, na.rm = TRUE)) {
  #   x[which(md_g_means), ]$measure <- "SMD"
  #   measure[which(md_g_means)] <- "SMD"
  # }

  ## SMD : at least 'value' OR 'means' should be indicated
  d_missing = missing_value & missing_means_post & measure %in% c("SMD", "G")
  if (any(d_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("SMD/G measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(d_missing)] <- paste(column_errors[which(d_missing)], "Unknown value of the SMD or G / means and SD. // ")
    column_type_errors[which(d_missing)] <- ifelse(column_type_errors[which(d_missing)] == "ERROR", "ERROR", "ERROR")
  }

  d_missing = missing_cases_controls & missing_se & missing_ci & measure %in% c("SMD", "G")
  if (any(d_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("SMD/G measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(d_missing)] <- paste(column_errors[which(d_missing)], "Unknown value of the n_cases/n_controls / se or 95% CI of the SMD. // ")
    column_type_errors[which(d_missing)] <- ifelse(column_type_errors[which(d_missing)] == "ERROR", "ERROR", "ERROR")
  }

  ## G : at least 'value' should be indicated
  # g_missing = missing_value & measure == "G"
  # if (any(g_missing, na.rm = TRUE)) {
  #   status = "ERROR"
  #   error_msgs <- append(error_msgs, paste("G measure is not associated with sufficient information to run the umbrella review. "))
  #   column_errors[which(g_missing)] <- paste(column_errors[which(g_missing)], "Unknown value of G. // ")
  #   column_type_errors[which(g_missing)] <- ifelse(column_type_errors[which(g_missing)] == "ERROR", "ERROR", "ERROR")
  # }


  ## MD: at least 'value' + (('ci' | 'se')  | means_post) should be indicated
  md_missing = (missing_cases_controls & measure %in% c("MD", "MC")) |
    ((missing_means_post & missing_value_ci & missing_value_se) & measure %in% c("MD")) |
    ((missing_means_pre_post & missing_means_change & missing_value_ci & missing_value_se) & measure %in% c("MC"))
  if (any(md_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("MC or MD measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(md_missing)] <- paste(column_errors[which(md_missing)], " Unknown value + (confidence interval / se / var) or n_cases+n_controls of the MC. // ")
    column_type_errors[which(md_missing)] <- "ERROR"
  }

  # SMC/MC : at least 'value' + ('ci'|'se') OR 'mean_pre_post' OR 'mean_change' should be indicated
  smc_missing_ci = missing_value_ci & measure %in% c("SMC", "MC")
  smc_missing_se = missing_value_se & measure %in% c("SMC", "MC")
  smc_missing_pre_post = missing_means_pre_post & measure %in% c("SMC", "MC")
  smc_missing_change = missing_means_change & measure %in% c("SMC", "MC")

  smc_missing = smc_missing_ci & smc_missing_se & smc_missing_pre_post & smc_missing_change
  if (any(smc_missing, na.rm = TRUE)) {
    status = "ERROR"
    error_msgs <- append(error_msgs, paste("SMC/MC measure is not associated with sufficient information to run the umbrella review. "))
    column_errors[which(smc_missing)] <- paste(column_errors[which(smc_missing)], " Unknown value + (confidence interval / se / var) OR pre+post means/SD OR means/SD change. // ")
    column_type_errors[which(smc_missing)] <- "ERROR"
  }

  # Z/R : value
  r_missing = (missing_ci & missing_n_sample) &
              (missing_se & missing_n_sample) &
              (missing_value & missing_n_sample) &
                measure %in% c("R", "Z")

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
         "MC" =, "mean change" = "MC",
         "SMC" = "SMC",
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

