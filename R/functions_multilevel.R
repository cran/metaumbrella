#' Calculate a fixed effect for studies with multiple independent subgroups
#'
#' @param x rows of studies with multiple groups
#' @param measure the measure used
#'
#' @noRd
.unique_es_subgroups <- function(x, measure) {
  weights = 1/(x$se^2)
  if (measure == "SMD") {
     value_list = x$value
  } else {
    value_list = log(x$value)
  }
  mean_value = sum(weights * value_list) / sum(weights)
  se = sqrt(1 / sum(weights))
  value = ifelse(measure == "SMD", mean_value, exp(mean_value))
  ci_lo = ifelse(measure == "SMD", mean_value - qt(0.975, x$n_cases + x$n_controls - 2) * se, exp(mean_value - qnorm(0.975) * se))
  ci_up = ifelse(measure == "SMD", mean_value + qt(0.975, x$n_cases + x$n_controls - 2) * se, exp(mean_value + qnorm(0.975) * se))
  return(data.frame(
    value = value,
    se = se,
    ci_lo = ci_lo,
    ci_up = ci_up
  ))
}


#

#' Calculate a mean ES for studies with multiple outcomes (based on the formula 24.5 of Boreinstein (2009) p. 231)
#'
#' @param x rows of studies with multiple outcomes
#' @param measure the measure used
#'
#' @noRd
.unique_es_outcomes <- function(x, measure) {
  var_es <- x$se^2
  # matrix with the product of all SE
  prod_se <- x$se %*% t(x$se)
  # matrix with the product of all SE * correlation
  prod_se_r <- prod_se * unique(x$r)
  # set the diagonal to 0 (the diagonal contains the variances of the ES)
  prod_se_r[lower.tri(prod_se_r)] <- 0
  # set the lower part of the matrix equal to 0
  diag(prod_se_r) <- 0
  mean_value <- ifelse(measure == "SMD", mean(x$value), exp(mean(log(x$value))))
  var = (1 / nrow(x))^2 * (sum(var_es) + 2 * sum(prod_se_r) )
  se = sqrt(var)
  ci_lo = ifelse(measure == "SMD", mean_value - qt(0.975, x$n_cases + x$n_controls - 2) * se, exp(log(mean_value) - qnorm(0.975) * se))
  ci_up = ifelse(measure == "SMD", mean_value + qt(0.975, x$n_cases + x$n_controls - 2) * se, exp(log(mean_value) + qnorm(0.975) * se))

  return(data.frame(
    value = mean_value,
    se = se,
    var = var,
    ci_lo = ci_lo,
    ci_up = ci_up
  ))
}

#' Create an aggregated dataframe for studies with multiple independent subgroups
#'
#' @param x a list of rows of studies with multiple groups
#' @param measure the measure used
#'
#' @noRd
.agg_subgroups <- function(x, measure) {

  n_cases = sum(x$n_cases)
  n_controls = sum(x$n_controls)
  sum_N = sum(n_cases, n_controls, na.rm = TRUE) # NA for IRR

  return(data.frame(
    row_index = unique(x$row_index)[1],
    author = unique(x$author),
    year = unique(x$year),
    multiple_es = "aggregated_group",
    duplicate = FALSE,
    value = .unique_es_subgroups(x, measure)$value,
    se = .unique_es_subgroups(x, measure)$se,
    ci_lo = .unique_es_subgroups(x, measure)$ci_lo,
    ci_up = .unique_es_subgroups(x, measure)$ci_up,
    n_sample = sum(x$n_sample),
    n_cases = n_cases,
    n_controls = n_controls,
    mean_cases = NA,
    sd_cases = NA,
    mean_controls = NA,
    sd_controls = NA,
    mean_pre_cases = NA,
    sd_pre_cases = NA,
    mean_pre_controls = NA,
    sd_pre_controls = NA,
    mean_change_cases = NA,
    sd_change_cases = NA,
    mean_change_controls = NA,
    sd_change_controls = NA,
    pre_post_cor = NA,
    sum_N = sum_N,
    n_cases_exp = sum(x$n_cases_exp),
    n_controls_exp = sum(x$n_controls_exp),
    n_cases_nexp = sum(x$n_cases_nexp),
    n_controls_nexp = sum(x$n_controls_nexp),
    n_exp = sum(x$n_exp),
    n_nexp = sum(x$n_nexp),
    time = sum(x$time),
    time_exp = sum(x$time_exp),
    time_nexp = sum(x$time_nexp),
    rob.recoded = weighted.mean(x$rob, apply(cbind(x$n_cases, x$n_controls), 1, sum, na.rm = TRUE)),
    shared_nexp = unique(x$shared_nexp),
    shared_controls = unique(x$shared_controls),
    thr = NA,
    reverse_es = ifelse(any(grepl("The effect size has been reversed", x$reverse_es, fixed = TRUE)), "Some effect sizes have been reversed.", "No effect sizes have been reversed."),
    r = mean(x$r)
  ))
}

#' Create an aggregated dataframe for studies with multiple outcomes
#'
#' @param x a list of rows of studies with multiple outcomes
#' @param measure the measure used
#'
#' @noRd
.agg_outcomes <- function(x,  measure) {
  value = .unique_es_outcomes(x, measure)$value
  se = .unique_es_outcomes(x, measure)$se
  ci_lo = .unique_es_outcomes(x, measure)$ci_lo
  ci_up = .unique_es_outcomes(x, measure)$ci_up

  if (measure == "IRR") {
    N_max =  .largest_irr(x, return = "nrow")
  } else if (measure %in% c("OR", "RR", "HR")) {
    N_max =  .largest_or_rr_hr(x, return = "nrow")
  } else if (measure == "SMD") {
    N_max = .largest_smd(x, return = "nrow")
  }

  n_cases = x$n_cases[N_max]
  n_controls = x$n_controls[N_max]
  n_exp = n_nexp = NA # we do not need this information except for RR
  sum_N = sum(n_cases, n_controls, na.rm = TRUE) # NA for IRR

  if (measure %in% c("SMD", "HR")) {
    n_cases_exp = n_controls_exp = n_cases_nexp = n_controls_nexp = time = time_exp = time_nexp = NA
  } else if (measure == "OR") {
    convert_2x2_or = .estimate_n_from_or_and_n_cases(value, se^2, n_cases, n_controls)
    n_cases_exp = convert_2x2_or$n_cases_exp
    n_controls_exp = convert_2x2_or$n_controls_exp
    n_cases_nexp = convert_2x2_or$n_cases_nexp
    n_controls_nexp = convert_2x2_or$n_controls_nexp
    time = time_exp = time_nexp = NA
  } else if (measure == "RR") {
    convert_2x2_rr = .estimate_n_from_rr(value, se^2, n_cases, n_controls)
    n_cases_exp = convert_2x2_rr$n_cases_exp
    n_cases_nexp = convert_2x2_rr$n_cases_nexp
    n_exp = convert_2x2_rr$n_exp
    n_nexp = convert_2x2_rr$n_nexp
    n_controls_exp = n_exp - n_cases_exp
    n_controls_nexp = n_nexp - n_cases_nexp
    time = time_exp = time_nexp = NA
  } else if (measure == "IRR") {
    time = x$time[N_max]
    convert_n_cases_time_irr = .estimate_n_from_irr(value, se^2, n_cases, time)
    n_cases_exp = convert_n_cases_time_irr$n_cases_exp
    n_cases_nexp = convert_n_cases_time_irr$n_cases_nexp
    time_exp = convert_n_cases_time_irr$time_exp
    time_nexp = convert_n_cases_time_irr$time_nexp
    n_controls_exp = n_controls_nexp = NA
  }

  return(data.frame(
    row_index = unique(x$row_index)[1],
    author = unique(x$author),
    year = unique(x$year),
    multiple_es = "aggregated_outcome",
    duplicate = FALSE,
    value = value,
    se = se,
    ci_lo = ci_lo,
    ci_up = ci_up,
    n_sample = x$n_sample[N_max],
    n_cases = n_cases,
    n_controls = n_controls,
    mean_cases = NA,
    sd_cases = NA,
    mean_controls = NA,
    sd_controls = NA,
    mean_pre_cases = NA,
    sd_pre_cases = NA,
    mean_pre_controls = NA,
    sd_pre_controls = NA,
    mean_change_cases = NA,
    sd_change_cases = NA,
    mean_change_controls = NA,
    sd_change_controls = NA,
    pre_post_cor = NA,
    sum_N = sum_N,
    n_exp = n_exp,
    n_nexp = n_nexp,
    n_cases_exp = n_cases_exp,
    n_controls_exp = n_controls_exp,
    n_cases_nexp = n_cases_nexp,
    n_controls_nexp = n_controls_nexp,
    time = time,
    time_exp = time_exp,
    time_nexp = time_nexp,
    rob.recoded = weighted.mean(x$rob, apply(cbind(x$n_cases, x$n_controls), 1, sum, na.rm = TRUE)),
    shared_nexp = unique(x$shared_nexp),
    shared_controls = unique(x$shared_controls),
    thr = NA,
    reverse_es = ifelse(any(grepl("The effect size has been reversed", x$reverse_es, fixed = TRUE)), "Some effect sizes have been reversed.", "No effect sizes have been reversed."),
    r = mean(x$r)
  ))
}

#' Create an aggregated dataframe with studies with both multiple outcomes and independent subgroups
#'
#' @param x a well formatted dataset
#' @param r a correlation coefficient between outcomes of the same study
#' @param measure the measure used
#'
#' @noRd
.agg_data <- function(x, r, measure) {

  ##### 1) subset the dataframe to isolate studies with multiple outcomes -----
  x_mult_outcome <- subset(x, duplicate & multiple_es == "outcomes")
  ## we insert corresponding values of r
  r_i <- which(is.na(x_mult_outcome$r))
  x_mult_outcome$r[r_i] <- r

  x_mult_outcome_split <- split(x_mult_outcome, paste(x_mult_outcome$author, x_mult_outcome$year))

  ##### 2) subset the dataframe to isolate studies with multiple groups
  x_mult_group <- subset(x, duplicate & multiple_es == "groups")
  x_mult_group_split <- split(x_mult_group, paste(x_mult_group$author, x_mult_group$year))

  ##### 3) subset the dataframe to isolate studies without multiple ES
  x_unique <- subset(x, !duplicate)

  ##### 4) we create the corresponding datasets for each situation
  df_group <- do.call(rbind, lapply(x_mult_group_split, .agg_subgroups, measure))
  df_outcome <- do.call(rbind, lapply(x_mult_outcome_split, .agg_outcomes, measure))
  df_unique <- x_unique[,c('row_index', 'author', 'year', 'multiple_es', 'duplicate',
                           'value', 'se', 'ci_lo', 'ci_up', 'n_sample', 'n_cases', 'n_controls', 'mean_cases',
                           'sd_cases', 'mean_controls', 'sd_controls',
                           'mean_pre_cases', 'sd_pre_cases', 'mean_pre_controls','sd_pre_controls',
                           'mean_change_cases', 'sd_change_cases', 'mean_change_controls', 'sd_change_controls', 'pre_post_cor',
                           'sum_N', 'n_cases_exp', 'n_controls_exp', 'n_cases_nexp', 'n_controls_nexp',
                           'n_exp', 'n_nexp', 'time', 'time_exp', 'time_nexp',
                            'rob.recoded', 'shared_nexp',
                           'shared_controls', 'thr', 'reverse_es', 'r')]

  # 4) we return the merged datasets
  return(data.frame(rbind(df_group, df_outcome, df_unique)))
}

#' Adjust sample size when several studies share a control/non exposed group
#'
#' @param shared a shared column: shared_nexp or shared_controls
#' @param author name of author
#' @param year year
#'
#' @noRd
.shared_adjustment_mod = function (shared, author, year) {

  # we create an ID for each study
  author_year = paste0(author, year)

  # we store the shared values in a new object and replace missing values
  shared_NA = shared

  shared_NA[which(is.na(shared))] <- "na_shared"

  # we create a dataframe storing information on the studies and the shared groups
  df <- data.frame(shared, author_year, shared_NA)

  # we store the initial ordering of the dataset
  df$order <- 1:nrow(df)

  # we reorder the dataset according to the shared_NA values
  df <- df[order(df$shared_NA), ]

  # we split the dataset according to the shared_NA values
  df.split <- split(df, df$shared_NA)

  # we apply the function determining the adjustment needed
  unord_df <- do.call(rbind, lapply(df.split, .func_shared))

  # we reorder the dataframe as initially so that returned values can be inserted in the original dataset
  return(unord_df[order(unord_df$order), ]$shared)
}

#' Determine the adjustment that should be made when several studies share a control/non-exposed group
#'
#' @param x a list of rows provided by .shared_adjustment_mod
#'
#' @noRd
.func_shared <- function (x) {
  # if users have indicated no shared values (or if a single value is indicated for an unique study), no adjustment is made
  if (all(is.na(x$shared)) | length(x$shared) == 1) {
    returned_df = data.frame(shared = rep(1, nrow(x)),
                             order = x$order)
  } else {
    # if users have indicated shared values we adjust the sample by the number of unique studies
    returned_df = data.frame(shared = rep(1 / length(unique(x$author_year)), nrow(x)),
                             order = x$order)
  }
  return(returned_df)
}
