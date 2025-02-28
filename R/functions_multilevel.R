#' Aggregate dataframe
#'
#' @param x a dataset
#' @param measure a measure
#' @param r a correlation
#'
#' @importFrom metaConvert aggregate_df
#'
#' @noRd
.agg_mcv = function(x, measure, r) {
  # x = x_i_format
  cols = c('row_index', 'factor', 'duplicate', 'author', 'year', 'value', 'se', 'ci_lo', 'ci_up', 'r',
           'n_sample', 'n_cases', 'n_controls', 'n_exp', 'n_nexp',
           'rob.recoded', 'amstar', "indirectness",
           "rob_report.recoded",
           "rob1_rand.recoded", "rob1_allocation.recoded", "rob1_blind_pers.recoded",
           "rob1_blind_outcome.recoded", "rob1_attrition.recoded","rob1_report.recoded" ,
           "rob2_rand.recoded", "rob2_deviation.recoded", "rob2_missing.recoded",
           "rob2_outcome.recoded", "rob2_report.recoded",
           'time', 'multiple_es', #'shared_nexp', 'shared_controls',
           'thr',
           'mean_cases', 'sd_cases', 'mean_controls',
           'sd_controls', 'mean_pre_cases', 'sd_pre_cases', 'mean_pre_controls',
           'sd_pre_controls', 'mean_change_cases', 'sd_change_cases',
           'mean_change_controls', 'sd_change_controls', 'pre_post_cor',
           'n_cases_exp', 'n_controls_exp', 'n_cases_nexp',
           'n_controls_nexp',
           'time_exp', 'time_nexp')

  # OUTCOMES ----
  x_mult_outcome <- subset(x, duplicate & multiple_es == "outcomes")
  x_mult_outcome$r[which(is.na(x_mult_outcome$r))] <- r
  x_mult_outcome$ID = paste(x_mult_outcome$author, x_mult_outcome$year)

  if (nrow(x_mult_outcome) > 0) {
    df_outcome = suppressWarnings(aggregate_df(x = x_mult_outcome, agg_fact = "ID",
                              es = "value", se = "se",
                              dependence = "outcomes",
                              cor_unit = unique(x_mult_outcome$r)[1],
                              col_fact = c("author", "year", "factor", "indirectness"),
                              col_max = c("n_sample", "n_cases", "n_controls", "time"),
                              col_mean = c("r", "amstar",
                                           "rob.recoded", "rob_report.recoded",
                                           "rob1_rand.recoded", "rob1_allocation.recoded", "rob1_blind_pers.recoded",
                                           "rob1_blind_outcome.recoded", "rob1_attrition.recoded", "rob1_report.recoded",
                                           "rob2_rand.recoded", "rob2_deviation.recoded", "rob2_missing.recoded",
                                           "rob2_outcome.recoded", "rob2_report.recoded"),
                              # col_weighted_mean = c("rob.recoded", "rob_report.recoded",
                              #                       "rob1_rand.recoded", "rob1_allocation.recoded", "rob1_blind_pers.recoded",
                              #                       "rob1_blind_outcome.recoded", "rob1_attrition.recoded","rob1_report.recoded" ,
                              #                       "rob2_rand.recoded", "rob2_deviation.recoded", "rob2_missing.recoded",
                              #                       "rob2_outcome.recoded", "rob2_report.recoded"),
                              # weights = "n_sample",
                              na.rm = FALSE))

    df_outcome$n_sample = with(df_outcome,
     ifelse(!is.na(n_cases) & !is.na(n_controls) & n_cases != n_controls,
            sum(n_cases+n_controls, na.rm=TRUE),
            n_sample)
    )

    df_outcome$value = df_outcome$es
    df_outcome$multiple_es = "aggregated_outcomes"
    df_outcome$duplicate = FALSE


    df_outcome$ci_lo = with(df_outcome,
                            ifelse(measure %in% c("G", "SMC", "SMD", "MD", "MC"),
                                   value - qt(0.975, n_cases + n_controls - 2) * se,
                                   value - qnorm(0.975) * se))
    df_outcome$ci_up = with(df_outcome,
                            ifelse(measure %in% c("G", "SMC", "SMD", "MD", "MC"),
                                   value + qt(0.975, n_cases + n_controls - 2) * se,
                                   value + qnorm(0.975) * se))

    df_outcome[, cols[!cols %in% names(df_outcome)]] <- NA
    df_outcome <- df_outcome[, cols]
  } else {
    df_outcome = NULL
  }

  x_mult_group <- subset(x, duplicate & multiple_es == "groups")
  x_mult_group$ID = paste(x_mult_group$author, x_mult_group$year)

  if (nrow(x_mult_group) > 0) {

    df_groups = suppressWarnings(aggregate_df(x = x_mult_group, agg_fact = "ID",
                             es = "value", se = "se",
                             dependence = "subgroups",
                             col_fact = c("author", "year", "factor", "indirectness"),
                             col_mean = c("r", "amstar",
                                          "rob.recoded", "rob_report.recoded",
                                          "rob1_rand.recoded", "rob1_allocation.recoded", "rob1_blind_pers.recoded",
                                          "rob1_blind_outcome.recoded", "rob1_attrition.recoded", "rob1_report.recoded",
                                          "rob2_rand.recoded", "rob2_deviation.recoded", "rob2_missing.recoded",
                                          "rob2_outcome.recoded", "rob2_report.recoded"),
                             col_sum = c("n_sample", "n_cases", "n_controls",
                                         "n_cases_exp", "n_controls_exp", "n_cases_nexp", "n_controls_nexp",
                                         "n_exp", "n_nexp", "time", "time_exp", "time_nexp"),
                             # col_weighted_mean = c("rob.recoded", "rob_report.recoded",
                             #                       "rob1_rand.recoded", "rob1_allocation.recoded", "rob1_blind_pers.recoded",
                             #                       "rob1_blind_outcome.recoded", "rob1_attrition.recoded","rob1_report.recoded" ,
                             #                       "rob2_rand.recoded", "rob2_deviation.recoded", "rob2_missing.recoded",
                             #                       "rob2_outcome.recoded", "rob2_report.recoded"),
                             # weights = "n_sample",
                             na.rm = TRUE))
    df_groups$value = df_groups$es
    df_groups$se = df_groups$se
    df_groups$multiple_es = "aggregated_subgroups"
    df_groups$duplicate = FALSE
    df_groups$ci_lo = with(df_groups,
                           ifelse(measure %in% c("G", "SMC", "SMD", "MD", "MC"),
                                  value - qt(0.975, n_cases + n_controls - 2) * se,
                                  value - qnorm(0.975) * se))
    df_groups$ci_up = with(df_groups,
                           ifelse(measure %in% c("G", "SMC", "SMD", "MD", "MC"),
                                  value + qt(0.975, n_cases + n_controls - 2) * se,
                                  value + qnorm(0.975) * se))

    df_groups[, cols[!cols %in% names(df_groups)]] <- NA
    df_groups <- df_groups[, cols]
  } else {
    df_groups = NULL
  }

  # Get unique records (those that don't need aggregation)
  x_unique <- subset(x, !duplicate & (!multiple_es %in% c("outcomes", "groups") | is.na(multiple_es)))
  if (nrow(x_unique) > 0) {
    x_unique[, cols[!cols %in% names(x_unique)]] <- NA
    df_unique <- x_unique[, cols]
  } else {
    df_unique = NULL
  }

  # Combine all results
  res_agg = data.frame(rbind(df_groups,
                             df_outcome,
                             df_unique))
  res_agg[res_agg == "N/A"] <- NA

  return(res_agg)
}
